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

import org.scalajs.ir.ScalaJSVersions

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

function installJSField(instance, name, value) {
  Object.defineProperty(instance, name, {
    value,
    configurable: true,
    enumerable: true,
    writable: true,
  });
}

// FIXME We need to adapt this to the correct values
const linkingInfo = Object.freeze({
  "esVersion": 6,
  "assumingES6": true,
  "isWebAssembly": true,
  "productionMode": false,
  "linkerVersion": "${ScalaJSVersions.current}",
  "fileLevelThis": this
});

const scalaJSHelpers = {
  // JSTag
  JSTag: WebAssembly.JSTag,

  // BinaryOp.===
  is: Object.is,

  // undefined
  undef: void 0,
  isUndef: (x) => x === (void 0),

  // Zero boxes
  bFalse: false,
  bZero: 0,

  // Boxes (upcast) -- most are identity at the JS level but with different types in Wasm
  bZ: (x) => x !== 0,
  bB: (x) => x,
  bS: (x) => x,
  bI: (x) => x,
  bF: (x) => x,
  bD: (x) => x,

  // Unboxes (downcast, null is converted to the zero of the type as part of ToWebAssemblyValue)
  uZ: (x) => x, // ToInt32 turns false into 0 and true into 1, so this is also an identity
  uB: (x) => x,
  uS: (x) => x,
  uI: (x) => x,
  uF: (x) => x,
  uD: (x) => x,

  // Type tests
  tZ: (x) => typeof x === 'boolean',
  tB: (x) => typeof x === 'number' && Object.is((x << 24) >> 24, x),
  tS: (x) => typeof x === 'number' && Object.is((x << 16) >> 16, x),
  tI: (x) => typeof x === 'number' && Object.is(x | 0, x),
  tF: (x) => typeof x === 'number' && (Math.fround(x) === x || x !== x),
  tD: (x) => typeof x === 'number',

  // fmod, to implement Float_% and Double_% (it is apparently quite hard to implement fmod otherwise)
  fmod: (x, y) => x % y,

  // Closure
  closure: (f, data) => f.bind(void 0, data),
  closureThis: (f, data) => function(...args) { return f(data, this, ...args); },
  closureRest: (f, data, n) => ((...args) => f(data, ...args.slice(0, n), args.slice(n))),
  closureThisRest: (f, data, n) => function(...args) { return f(data, this, ...args.slice(0, n), args.slice(n)); },

  // Top-level exported defs -- they must be `function`s but have no actual `this` nor `data`
  makeExportedDef: (f) => function(...args) { return f(...args); },
  makeExportedDefRest: (f, n) => function(...args) { return f(...args.slice(0, n), args.slice(n)); },

  // Strings
  emptyString: "",
  stringLength: (s) => s.length,
  stringCharAt: (s, i) => s.charCodeAt(i),
  jsValueToString: (x) => (x === void 0) ? "undefined" : x.toString(),
  jsValueToStringForConcat: (x) => "" + x,
  booleanToString: (b) => b ? "true" : "false",
  charToString: (c) => String.fromCharCode(c),
  intToString: (i) => "" + i,
  longToString: (l) => "" + l, // l must be a bigint here
  doubleToString: (d) => "" + d,
  stringConcat: (x, y) => ("" + x) + y, // the added "" is for the case where x === y === null
  isString: (x) => typeof x === 'string',

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

  // JS interop
  jsGlobalRefGet: (globalRefName) => (new Function("return " + globalRefName))(),
  jsGlobalRefSet: (globalRefName, v) => {
    var argName = globalRefName === 'v' ? 'w' : 'v';
    (new Function(argName, globalRefName + " = " + argName))(v);
  },
  jsGlobalRefTypeof: (globalRefName) => (new Function("return typeof " + globalRefName))(),
  jsNewArray: () => [],
  jsArrayPush: (a, v) => (a.push(v), a),
  jsArraySpreadPush: (a, vs) => (a.push(...vs), a),
  jsNewObject: () => ({}),
  jsObjectPush: (o, p, v) => (o[p] = v, o),
  jsSelect: (o, p) => o[p],
  jsSelectSet: (o, p, v) => o[p] = v,
  jsNew: (constr, args) => new constr(...args),
  jsFunctionApply: (f, args) => f(...args),
  jsMethodApply: (o, m, args) => o[m](...args),
  jsImportCall: (s) => import(s),
  jsImportMeta: () => import.meta,
  jsDelete: (o, p) => { delete o[p]; },
  jsForInSimple: (o, f) => { for (var k in o) f(k); },
  jsIsTruthy: (x) => !!x,
  jsLinkingInfo: linkingInfo,

  // Excruciating list of all the JS operators
  jsUnaryPlus: (a) => +a,
  jsUnaryMinus: (a) => -a,
  jsUnaryTilde: (a) => ~a,
  jsUnaryBang: (a) => !a,
  jsUnaryTypeof: (a) => typeof a,
  jsStrictEquals: (a, b) => a === b,
  jsNotStrictEquals: (a, b) => a !== b,
  jsPlus: (a, b) => a + b,
  jsMinus: (a, b) => a - b,
  jsTimes: (a, b) => a * b,
  jsDivide: (a, b) => a / b,
  jsModulus: (a, b) => a % b,
  jsBinaryOr: (a, b) => a | b,
  jsBinaryAnd: (a, b) => a & b,
  jsBinaryXor: (a, b) => a ^ b,
  jsShiftLeft: (a, b) => a << b,
  jsArithmeticShiftRight: (a, b) => a >> b,
  jsLogicalShiftRight: (a, b) => a >>> b,
  jsLessThan: (a, b) => a < b,
  jsLessEqual: (a, b) => a <= b,
  jsGreaterThan: (a, b) => a > b,
  jsGreaterEqual: (a, b) => a >= b,
  jsIn: (a, b) => a in b,
  jsInstanceof: (a, b) => a instanceof b,
  jsExponent: (a, b) => a ** b,

  // Non-native JS class support
  newSymbol: Symbol,
  createJSClass: (data, superClass, preSuperStats, superArgs, postSuperStats, fields) => {
    // fields is an array where even indices are field names and odd indices are initial values
    return class extends superClass {
      constructor(...args) {
        var preSuperEnv = preSuperStats(data, new.target, ...args);
        super(...superArgs(data, preSuperEnv, new.target, ...args));
        for (var i = 0; i != fields.length; i = (i + 2) | 0)
          installJSField(this, fields[i], fields[(i + 1) | 0]);
        postSuperStats(data, preSuperEnv, new.target, this, ...args);
      }
    };
  },
  createJSClassRest: (data, superClass, preSuperStats, superArgs, postSuperStats, fields, n) => {
    // fields is an array where even indices are field names and odd indices are initial values
    return class extends superClass {
      constructor(...args) {
        var fixedArgs = args.slice(0, n);
        var restArg = args.slice(n);
        var preSuperEnv = preSuperStats(data, new.target, ...fixedArgs, restArg);
        super(...superArgs(data, preSuperEnv, new.target, ...fixedArgs, restArg));
        for (var i = 0; i != fields.length; i = (i + 2) | 0)
          installJSField(this, fields[i], fields[(i + 1) | 0]);
        postSuperStats(data, preSuperEnv, new.target, this, ...fixedArgs, restArg);
      }
    };
  },
  installJSField,
  installJSMethod: (data, jsClass, name, func, fixedArgCount) => {
    var closure = fixedArgCount < 0
      ? (function(...args) { return func(data, this, ...args); })
      : (function(...args) { return func(data, this, ...args.slice(0, fixedArgCount), args.slice(fixedArgCount))});
    jsClass.prototype[name] = closure;
  },
  installJSStaticMethod: (data, jsClass, name, func, fixedArgCount) => {
    var closure = fixedArgCount < 0
      ? (function(...args) { return func(data, ...args); })
      : (function(...args) { return func(data, ...args.slice(0, fixedArgCount), args.slice(fixedArgCount))});
    jsClass[name] = closure;
  },
  installJSProperty: (data, jsClass, name, getter, setter) => {
    var getterClosure = getter
      ? (function() { return getter(data, this) })
      : (void 0);
    var setterClosure = setter
      ? (function(arg) { setter(data, this, arg) })
      : (void 0);
    Object.defineProperty(jsClass.prototype, name, {
      get: getterClosure,
      set: setterClosure,
      configurable: true,
    });
  },
  installJSStaticProperty: (data, jsClass, name, getter, setter) => {
    var getterClosure = getter
      ? (function() { return getter(data) })
      : (void 0);
    var setterClosure = setter
      ? (function(arg) { setter(data, arg) })
      : (void 0);
    Object.defineProperty(jsClass, name, {
      get: getterClosure,
      set: setterClosure,
      configurable: true,
    });
  },
  jsSuperSelect: superSelect,
  jsSuperSelectSet: superSelectSet,
  jsSuperCall: (superClass, receiver, method, args) => {
    return superClass.prototype[method].apply(receiver, args);
  },
}

export async function load(wasmFileURL, importedModules, exportSetters) {
  const myScalaJSHelpers = { ...scalaJSHelpers, idHashCodeMap: new WeakMap() };
  const importsObj = {
    "__scalaJSHelpers": myScalaJSHelpers,
    "__scalaJSImports": importedModules,
    "__scalaJSExportSetters": exportSetters,
  };
  const resolvedURL = new URL(wasmFileURL, import.meta.url);
  if (resolvedURL.protocol === 'file:') {
    const { fileURLToPath } = await import("node:url");
    const { readFile } = await import("node:fs/promises");
    const wasmPath = fileURLToPath(resolvedURL);
    const body = await readFile(wasmPath);
    return WebAssembly.instantiate(body, importsObj);
  } else {
    return await WebAssembly.instantiateStreaming(fetch(resolvedURL), importsObj);
  }
}
    """
  }
}
