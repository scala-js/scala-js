/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.linker.backend.wasmemitter

import java.nio.charset.StandardCharsets

import EmbeddedConstants._

/** Contents of the `__loader.js` file that we emit in every output. */
object LoaderContent {
  val bytesContent: Array[Byte] =
    stringContent.getBytes(StandardCharsets.UTF_8)

  private def stringContent: String = {
    raw"""
// This implementation follows no particular specification, but is the same as the JS backend.
// It happens to coincide with java.lang.Long.hashCode() for common values.
function bigintHashCode(x) {
  var res = 0;
  if (x < 0n)
    x = ~x;
  while (x !== 0n) {
    res ^= Number(BigInt.asIntN(32, x));
    x >>= 32n;
  }
  return res;
}

// JSSuperSelect support -- directly copied from the output of the JS backend
function resolveSuperRef(superClass, propName) {
  var getPrototypeOf = Object.getPrototyeOf;
  var getOwnPropertyDescriptor = Object.getOwnPropertyDescriptor;
  var superProto = superClass.prototype;
  while (superProto !== null) {
    var desc = getOwnPropertyDescriptor(superProto, propName);
    if (desc !== (void 0)) {
      return desc;
    }
    superProto = getPrototypeOf(superProto);
  }
}
function superSelect(superClass, self, propName) {
  var desc = resolveSuperRef(superClass, propName);
  if (desc !== (void 0)) {
    var getter = desc.get;
    return getter !== (void 0) ? getter.call(self) : getter.value;
  }
}
function superSelectSet(superClass, self, propName, value) {
  var desc = resolveSuperRef(superClass, propName);
  if (desc !== (void 0)) {
    var setter = desc.set;
    if (setter !== (void 0)) {
      setter.call(self, value);
      return;
    }
  }
  throw new TypeError("super has no setter '" + propName + "'.");
}

const scalaJSHelpers = {
  // JSTag
  JSTag: WebAssembly.JSTag,

  // BinaryOp.===
  is: Object.is,

  // undefined
  undef: void 0,
  isUndef: (x) => x === (void 0),

  // Constant boxes
  bFalse: false,
  bTrue: true,

  // Boxes (upcast) -- most are identity at the JS level but with different types in Wasm
  bIFallback: (x) => x,
  bF: (x) => x,
  bD: (x) => x,

  // Unboxes (downcast, null is converted to the zero of the type as part of ToWebAssemblyValue)
  uZ: (x) => x, // ToInt32 turns false into 0 and true into 1, so this is also an identity
  uIFallback: (x) => x,
  uF: (x) => x,
  uD: (x) => x,

  // Type tests
  tZ: (x) => typeof x === 'boolean',
  tI: (x) => typeof x === 'number' && Object.is(x | 0, x),
  tF: (x) => typeof x === 'number' && (Math.fround(x) === x || x !== x),
  tD: (x) => typeof x === 'number',

  // fmod, to implement Float_% and Double_% (it is apparently quite hard to implement fmod otherwise)
  fmod: (x, y) => x % y,

  // Strings
  emptyString: "",
  jsValueToString: (x) => (x === void 0) ? "undefined" : x.toString(),
  jsValueToStringForConcat: (x) => "" + x,
  booleanToString: (b) => b ? "true" : "false",
  intToString: (i) => "" + i,
  longToString: (l) => "" + l, // l must be a bigint here
  doubleToString: (d) => "" + d,

  // Get the type of JS value of `x` in a single JS helper call, for the purpose of dispatch.
  jsValueType: (x) => {
    if (typeof x === 'number')
      return $JSValueTypeNumber;
    if (typeof x === 'string')
      return $JSValueTypeString;
    if (typeof x === 'boolean')
      return x | 0; // JSValueTypeFalse or JSValueTypeTrue
    if (typeof x === 'undefined')
      return $JSValueTypeUndefined;
    if (typeof x === 'bigint')
      return $JSValueTypeBigInt;
    if (typeof x === 'symbol')
      return $JSValueTypeSymbol;
    return $JSValueTypeOther;
  },

  // JS side of the `valueDescription` helper
  // TODO: only emit this when required by checked behaviors
  jsValueDescription: ((x) =>
    (typeof x === 'number')
      ? (Object.is(x, -0) ? "number(-0)" : ("number(" + x + ")"))
      : (typeof x)
  ),

  // Identity hash code
  bigintHashCode,
  symbolDescription: (x) => {
    var desc = x.description;
    return (desc === void 0) ? null : desc;
  },
  idHashCodeGet: (map, obj) => map.get(obj) | 0, // undefined becomes 0
  idHashCodeSet: (map, obj, value) => map.set(obj, value),

  // Some support functions for CoreWasmLib
  makeTypeError: (msg) => new TypeError(msg),

  // JS interop
  jsNewArray: () => [],
  jsNewObject: () => ({}),
  jsSelect: (o, p) => o[p],
  jsSelectSet: (o, p, v) => o[p] = v,
  jsNewNoArg: (constr) => new constr(),
  jsImportCall: (s) => import(s),
  jsImportMeta: () => import.meta,
  jsDelete: (o, p) => { delete o[p]; },
  jsForInStart: function*(o) { for (var k in o) yield k; },
  jsForInNext: (g) => { var r = g.next(); return [r.value, r.done]; },
  jsIsTruthy: (x) => !!x,

  // Non-native JS class support
  newSymbol: Symbol,
  jsSuperSelect: superSelect,
  jsSuperSelectSet: superSelectSet,
}

const stringBuiltinPolyfills = {
  test: (x) => typeof x === 'string',
  fromCharCode: (c) => String.fromCharCode(c),
  fromCodePoint: (cp) => String.fromCodePoint(cp),
  charCodeAt: (s, i) => s.charCodeAt(i),
  codePointAt: (s, i) => s.codePointAt(i),
  length: (s) => s.length,
  concat: (a, b) => "" + a + b, // "" tells the JIT that this is *always* a string concat operation
  substring: (str, start, end) => str.substring(start >>> 0, end >>> 0),
  equals: (a, b) => a === b,
};

export async function load(wasmFileURL, linkingInfo, exportSetters, customJSHelpers) {
  const myScalaJSHelpers = {
    ...scalaJSHelpers,
    jsLinkingInfo: linkingInfo,
    idHashCodeMap: new WeakMap()
  };
  const importsObj = {
    "__scalaJSHelpers": myScalaJSHelpers,
    "__scalaJSExportSetters": exportSetters,
    "__scalaJSCustomHelpers": customJSHelpers,
    "wasm:js-string": stringBuiltinPolyfills,
  };
  const options = {
    builtins: ["js-string"],
  };
  const resolvedURL = new URL(wasmFileURL, import.meta.url);
  if (resolvedURL.protocol === 'file:') {
    const { fileURLToPath } = await import("node:url");
    const { readFile } = await import("node:fs/promises");
    const wasmPath = fileURLToPath(resolvedURL);
    const body = await readFile(wasmPath);
    return WebAssembly.instantiate(body, importsObj, options);
  } else {
    return await WebAssembly.instantiateStreaming(fetch(resolvedURL), importsObj, options);
  }
}
    """
  }
}
