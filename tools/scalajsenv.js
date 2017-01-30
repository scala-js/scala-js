/* Scala.js runtime support
 * Copyright 2013 LAMP/EPFL
 * Author: Sébastien Doeraene
 */

/* ---------------------------------- *
 * The top-level Scala.js environment *
 * ---------------------------------- */

//!if outputMode == ECMAScript51Global
var ScalaJS = {};
//!endif

// Get the environment info
ScalaJS.env = (typeof __ScalaJSEnv === "object" && __ScalaJSEnv) ? __ScalaJSEnv : {};

// Global scope
ScalaJS.g =
  (typeof ScalaJS.env["global"] === "object" && ScalaJS.env["global"])
    ? ScalaJS.env["global"]
    : ((typeof global === "object" && global && global["Object"] === Object) ? global : this);
ScalaJS.env["global"] = ScalaJS.g;

// Where to send exports
//!if moduleKind == CommonJSModule
ScalaJS.e = exports;
//!else
ScalaJS.e =
  (typeof ScalaJS.env["exportsNamespace"] === "object" && ScalaJS.env["exportsNamespace"])
    ? ScalaJS.env["exportsNamespace"] : ScalaJS.g;
//!endif
ScalaJS.env["exportsNamespace"] = ScalaJS.e;

// Freeze the environment info
ScalaJS.g["Object"]["freeze"](ScalaJS.env);

// Linking info - must be in sync with scala.scalajs.runtime.LinkingInfo
ScalaJS.linkingInfo = {
  "envInfo": ScalaJS.env,
  "semantics": {
//!if asInstanceOfs == Compliant
    "asInstanceOfs": 0,
//!else
//!if asInstanceOfs == Fatal
    "asInstanceOfs": 1,
//!else
    "asInstanceOfs": 2,
//!endif
//!endif
//!if arrayIndexOutOfBounds == Compliant
    "arrayIndexOutOfBounds": 0,
//!else
//!if arrayIndexOutOfBounds == Fatal
    "arrayIndexOutOfBounds": 1,
//!else
    "arrayIndexOutOfBounds": 2,
//!endif
//!endif
//!if moduleInit == Compliant
    "moduleInit": 0,
//!else
//!if moduleInit == Fatal
    "moduleInit": 1,
//!else
    "moduleInit": 2,
//!endif
//!endif
//!if floats == Strict
    "strictFloats": true,
//!else
    "strictFloats": false,
//!endif
//!if productionMode == true
    "productionMode": true
//!else
    "productionMode": false
//!endif
  },
//!if outputMode == ECMAScript6
  "assumingES6": true,
//!else
  "assumingES6": false,
//!endif
  "linkerVersion": "{{LINKER_VERSION}}"
};
ScalaJS.g["Object"]["freeze"](ScalaJS.linkingInfo);
ScalaJS.g["Object"]["freeze"](ScalaJS.linkingInfo["semantics"]);

// Snapshots of builtins and polyfills

//!if outputMode == ECMAScript6
ScalaJS.imul = ScalaJS.g["Math"]["imul"];
ScalaJS.fround = ScalaJS.g["Math"]["fround"];
ScalaJS.clz32 = ScalaJS.g["Math"]["clz32"];
//!else
ScalaJS.imul = ScalaJS.g["Math"]["imul"] || (function(a, b) {
  // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
  const ah = (a >>> 16) & 0xffff;
  const al = a & 0xffff;
  const bh = (b >>> 16) & 0xffff;
  const bl = b & 0xffff;
  // the shift by 0 fixes the sign on the high part
  // the final |0 converts the unsigned value into a signed value
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0);
});

ScalaJS.fround = ScalaJS.g["Math"]["fround"] ||
//!if floats == Strict
  (ScalaJS.g["Float32Array"] ? (function(v) {
    const array = new ScalaJS.g["Float32Array"](1);
    array[0] = v;
    return array[0];
  }) : (function(v) {
    return ScalaJS.m.sjsr_package$().froundPolyfill__D__D(+v);
  }));
//!else
  (function(v) {
    return +v;
  });
//!endif

ScalaJS.clz32 = ScalaJS.g["Math"]["clz32"] || (function(i) {
  // See Hacker's Delight, Section 5-3
  if (i === 0) return 32;
  let r = 1;
  if ((i & 0xffff0000) === 0) { i <<= 16; r += 16; };
  if ((i & 0xff000000) === 0) { i <<= 8; r += 8; };
  if ((i & 0xf0000000) === 0) { i <<= 4; r += 4; };
  if ((i & 0xc0000000) === 0) { i <<= 2; r += 2; };
  return r + (i >> 31);
});
//!endif

// Other fields
//!if outputMode == ECMAScript51Global
ScalaJS.d = {};         // Data for types
ScalaJS.a = {};         // Scala.js-defined JS class value accessors
ScalaJS.b = {};         // Scala.js-defined JS class value fields
ScalaJS.c = {};         // Scala.js constructors
ScalaJS.h = {};         // Inheritable constructors (without initialization code)
ScalaJS.s = {};         // Static methods
ScalaJS.t = {};         // Static fields
ScalaJS.f = {};         // Default methods
ScalaJS.n = {};         // Module instances
ScalaJS.m = {};         // Module accessors
ScalaJS.is = {};        // isInstanceOf methods
ScalaJS.isArrayOf = {}; // isInstanceOfArrayOf methods
//!if asInstanceOfs != Unchecked
ScalaJS.as = {};        // asInstanceOf methods
ScalaJS.asArrayOf = {}; // asInstanceOfArrayOf methods
//!endif
ScalaJS.lastIDHash = 0; // last value attributed to an id hash code
ScalaJS.idHashCodeMap = ScalaJS.g["WeakMap"] ? new ScalaJS.g["WeakMap"]() : null;
//!else
let $lastIDHash = 0; // last value attributed to an id hash code
//!if outputMode == ECMAScript6
const $idHashCodeMap = new ScalaJS.g["WeakMap"]();
//!else
const $idHashCodeMap = ScalaJS.g["WeakMap"] ? new ScalaJS.g["WeakMap"]() : null;
//!endif
//!endif

// Core mechanism

ScalaJS.makeIsArrayOfPrimitive = function(primitiveData) {
  return function(obj, depth) {
    return !!(obj && obj.$classData &&
      (obj.$classData.arrayDepth === depth) &&
      (obj.$classData.arrayBase === primitiveData));
  }
};

//!if asInstanceOfs != Unchecked
ScalaJS.makeAsArrayOfPrimitive = function(isInstanceOfFunction, arrayEncodedName) {
  return function(obj, depth) {
    if (isInstanceOfFunction(obj, depth) || (obj === null))
      return obj;
    else
      ScalaJS.throwArrayCastException(obj, arrayEncodedName, depth);
  }
};
//!endif

/** Encode a property name for runtime manipulation
  *  Usage:
  *    env.propertyName({someProp:0})
  *  Returns:
  *    "someProp"
  *  Useful when the property is renamed by a global optimizer (like Closure)
  *  but we must still get hold of a string of that name for runtime
  * reflection.
  */
ScalaJS.propertyName = function(obj) {
  for (const prop in obj)
    return prop;
};

// Runtime functions

ScalaJS.isScalaJSObject = function(obj) {
  return !!(obj && obj.$classData);
};

//!if asInstanceOfs != Unchecked
ScalaJS.throwClassCastException = function(instance, classFullName) {
//!if asInstanceOfs == Compliant
  throw new ScalaJS.c.jl_ClassCastException().init___T(
    instance + " is not an instance of " + classFullName);
//!else
  throw new ScalaJS.c.sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new ScalaJS.c.jl_ClassCastException().init___T(
      instance + " is not an instance of " + classFullName));
//!endif
};

ScalaJS.throwArrayCastException = function(instance, classArrayEncodedName, depth) {
  for (; depth; --depth)
    classArrayEncodedName = "[" + classArrayEncodedName;
  ScalaJS.throwClassCastException(instance, classArrayEncodedName);
};
//!endif

//!if arrayIndexOutOfBounds != Unchecked
ScalaJS.throwArrayIndexOutOfBoundsException = function(i) {
  const msg = (i === null) ? null : ("" + i);
//!if arrayIndexOutOfBounds == Compliant
  throw new ScalaJS.c.jl_ArrayIndexOutOfBoundsException().init___T(msg);
//!else
  throw new ScalaJS.c.sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new ScalaJS.c.jl_ArrayIndexOutOfBoundsException().init___T(msg));
//!endif
};
//!endif

ScalaJS.noIsInstance = function(instance) {
  throw new ScalaJS.g["TypeError"](
    "Cannot call isInstance() on a Class representing a raw JS trait/object");
};

ScalaJS.makeNativeArrayWrapper = function(arrayClassData, nativeArray) {
  return new arrayClassData.constr(nativeArray);
};

ScalaJS.newArrayObject = function(arrayClassData, lengths) {
  return ScalaJS.newArrayObjectInternal(arrayClassData, lengths, 0);
};

ScalaJS.newArrayObjectInternal = function(arrayClassData, lengths, lengthIndex) {
  const result = new arrayClassData.constr(lengths[lengthIndex]);

  if (lengthIndex < lengths.length-1) {
    const subArrayClassData = arrayClassData.componentData;
    const subLengthIndex = lengthIndex+1;
    const underlying = result.u;
    for (let i = 0; i < underlying.length; i++) {
      underlying[i] = ScalaJS.newArrayObjectInternal(
        subArrayClassData, lengths, subLengthIndex);
    }
  }

  return result;
};

ScalaJS.objectToString = function(instance) {
  if (instance === void 0)
    return "undefined";
  else
    return instance.toString();
};

ScalaJS.objectGetClass = function(instance) {
  switch (typeof instance) {
    case "string":
      return ScalaJS.d.T.getClassOf();
    case "number": {
      const v = instance | 0;
      if (v === instance) { // is the value integral?
        if (ScalaJS.isByte(v))
          return ScalaJS.d.jl_Byte.getClassOf();
        else if (ScalaJS.isShort(v))
          return ScalaJS.d.jl_Short.getClassOf();
        else
          return ScalaJS.d.jl_Integer.getClassOf();
      } else {
        if (ScalaJS.isFloat(instance))
          return ScalaJS.d.jl_Float.getClassOf();
        else
          return ScalaJS.d.jl_Double.getClassOf();
      }
    }
    case "boolean":
      return ScalaJS.d.jl_Boolean.getClassOf();
    case "undefined":
      return ScalaJS.d.sr_BoxedUnit.getClassOf();
    default:
      if (instance === null)
        return instance.getClass__jl_Class();
      else if (ScalaJS.is.sjsr_RuntimeLong(instance))
        return ScalaJS.d.jl_Long.getClassOf();
      else if (ScalaJS.isScalaJSObject(instance))
        return instance.$classData.getClassOf();
      else
        return null; // Exception?
  }
};

ScalaJS.objectClone = function(instance) {
  if (ScalaJS.isScalaJSObject(instance) || (instance === null))
    return instance.clone__O();
  else
    throw new ScalaJS.c.jl_CloneNotSupportedException().init___();
};

ScalaJS.objectNotify = function(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notify__V();
};

ScalaJS.objectNotifyAll = function(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notifyAll__V();
};

ScalaJS.objectFinalize = function(instance) {
  if (ScalaJS.isScalaJSObject(instance) || (instance === null))
    instance.finalize__V();
  // else no-op
};

ScalaJS.objectEquals = function(instance, rhs) {
  if (ScalaJS.isScalaJSObject(instance) || (instance === null))
    return instance.equals__O__Z(rhs);
  else if (typeof instance === "number")
    return typeof rhs === "number" && ScalaJS.numberEquals(instance, rhs);
  else
    return instance === rhs;
};

ScalaJS.numberEquals = function(lhs, rhs) {
  return (lhs === rhs) ? (
    // 0.0.equals(-0.0) must be false
    lhs !== 0 || 1/lhs === 1/rhs
  ) : (
    // are they both NaN?
    (lhs !== lhs) && (rhs !== rhs)
  );
};

ScalaJS.objectHashCode = function(instance) {
  switch (typeof instance) {
    case "string":
      return ScalaJS.m.sjsr_RuntimeString$().hashCode__T__I(instance);
    case "number":
      return ScalaJS.m.sjsr_Bits$().numberHashCode__D__I(instance);
    case "boolean":
      return instance ? 1231 : 1237;
    case "undefined":
      return 0;
    default:
      if (ScalaJS.isScalaJSObject(instance) || instance === null)
        return instance.hashCode__I();
//!if outputMode != ECMAScript6
      else if (ScalaJS.idHashCodeMap === null)
        return 42;
//!endif
      else
        return ScalaJS.systemIdentityHashCode(instance);
  }
};

ScalaJS.comparableCompareTo = function(instance, rhs) {
  switch (typeof instance) {
    case "string":
//!if asInstanceOfs != Unchecked
      ScalaJS.as.T(rhs);
//!endif
      return instance === rhs ? 0 : (instance < rhs ? -1 : 1);
    case "number":
//!if asInstanceOfs != Unchecked
      ScalaJS.as.jl_Number(rhs);
//!endif
      return ScalaJS.m.jl_Double$().compare__D__D__I(instance, rhs);
    case "boolean":
//!if asInstanceOfs != Unchecked
      ScalaJS.asBoolean(rhs);
//!endif
      return instance - rhs; // yes, this gives the right result
    default:
      return instance.compareTo__O__I(rhs);
  }
};

ScalaJS.charSequenceLength = function(instance) {
  if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
    return ScalaJS.uI(instance["length"]);
//!else
    return instance["length"] | 0;
//!endif
  else
    return instance.length__I();
};

ScalaJS.charSequenceCharAt = function(instance, index) {
  if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
    return ScalaJS.uI(instance["charCodeAt"](index)) & 0xffff;
//!else
    return instance["charCodeAt"](index) & 0xffff;
//!endif
  else
    return instance.charAt__I__C(index);
};

ScalaJS.charSequenceSubSequence = function(instance, start, end) {
  if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
    return ScalaJS.as.T(instance["substring"](start, end));
//!else
    return instance["substring"](start, end);
//!endif
  else
    return instance.subSequence__I__I__jl_CharSequence(start, end);
};

ScalaJS.booleanBooleanValue = function(instance) {
  if (typeof instance === "boolean") return instance;
  else                               return instance.booleanValue__Z();
};

ScalaJS.numberByteValue = function(instance) {
  if (typeof instance === "number") return (instance << 24) >> 24;
  else                              return instance.byteValue__B();
};
ScalaJS.numberShortValue = function(instance) {
  if (typeof instance === "number") return (instance << 16) >> 16;
  else                              return instance.shortValue__S();
};
ScalaJS.numberIntValue = function(instance) {
  if (typeof instance === "number") return instance | 0;
  else                              return instance.intValue__I();
};
ScalaJS.numberLongValue = function(instance) {
  if (typeof instance === "number")
    return ScalaJS.m.sjsr_RuntimeLong$().fromDouble__D__sjsr_RuntimeLong(instance);
  else
    return instance.longValue__J();
};
ScalaJS.numberFloatValue = function(instance) {
  if (typeof instance === "number") return ScalaJS.fround(instance);
  else                              return instance.floatValue__F();
};
ScalaJS.numberDoubleValue = function(instance) {
  if (typeof instance === "number") return instance;
  else                              return instance.doubleValue__D();
};

ScalaJS.isNaN = function(instance) {
  return instance !== instance;
};

ScalaJS.isInfinite = function(instance) {
  return !ScalaJS.g["isFinite"](instance) && !ScalaJS.isNaN(instance);
};

ScalaJS.doubleToInt = function(x) {
  return (x > 2147483647) ? (2147483647) : ((x < -2147483648) ? -2147483648 : (x | 0));
};

/** Instantiates a JS object with variadic arguments to the constructor. */
ScalaJS.newJSObjectWithVarargs = function(ctor, args) {
  // This basically emulates the ECMAScript specification for 'new'.
  const instance = ScalaJS.g["Object"]["create"](ctor.prototype);
  const result = ctor["apply"](instance, args);
  switch (typeof result) {
    case "string": case "number": case "boolean": case "undefined": case "symbol":
      return instance;
    default:
      return result === null ? instance : result;
  }
};

ScalaJS.resolveSuperRef = function(initialProto, propName) {
  const getPrototypeOf = ScalaJS.g["Object"]["getPrototypeOf"];
  const getOwnPropertyDescriptor = ScalaJS.g["Object"]["getOwnPropertyDescriptor"];

  let superProto = getPrototypeOf(initialProto);
  while (superProto !== null) {
    const desc = getOwnPropertyDescriptor(superProto, propName);
    if (desc !== void 0)
      return desc;
    superProto = getPrototypeOf(superProto);
  }

  return void 0;
};

ScalaJS.superGet = function(initialProto, self, propName) {
  const desc = ScalaJS.resolveSuperRef(initialProto, propName);
  if (desc !== void 0) {
    const getter = desc["get"];
    if (getter !== void 0)
      return getter["call"](self);
    else
      return desc["value"];
  }
  return void 0;
};

ScalaJS.superSet = function(initialProto, self, propName, value) {
  const desc = ScalaJS.resolveSuperRef(initialProto, propName);
  if (desc !== void 0) {
    const setter = desc["set"];
    if (setter !== void 0) {
      setter["call"](self, value);
      return void 0;
    }
  }
  throw new ScalaJS.g["TypeError"]("super has no setter '" + propName + "'.");
};

//!if moduleKind == CommonJSModule
ScalaJS.moduleDefault = function(m) {
  return (m && (typeof m === "object") && "default" in m) ? m["default"] : m;
};
//!endif

ScalaJS.propertiesOf = function(obj) {
  const result = [];
  for (const prop in obj)
    result["push"](prop);
  return result;
};

ScalaJS.systemArraycopy = function(src, srcPos, dest, destPos, length) {
  const srcu = src.u;
  const destu = dest.u;

//!if arrayIndexOutOfBounds != Unchecked
  if (srcPos < 0 || destPos < 0 || length < 0 ||
      (srcPos > ((srcu.length - length) | 0)) ||
      (destPos > ((destu.length - length) | 0))) {
    ScalaJS.throwArrayIndexOutOfBoundsException(null);
  }
//!endif

  if (srcu !== destu || destPos < srcPos || (((srcPos + length) | 0) < destPos)) {
    for (let i = 0; i < length; i = (i + 1) | 0)
      destu[(destPos + i) | 0] = srcu[(srcPos + i) | 0];
  } else {
    for (let i = (length - 1) | 0; i >= 0; i = (i - 1) | 0)
      destu[(destPos + i) | 0] = srcu[(srcPos + i) | 0];
  }
};

ScalaJS.systemIdentityHashCode =
//!if outputMode != ECMAScript6
  (ScalaJS.idHashCodeMap !== null) ?
//!endif
  (function(obj) {
    switch (typeof obj) {
      case "string": case "number": case "boolean": case "undefined":
        return ScalaJS.objectHashCode(obj);
      default:
        if (obj === null) {
          return 0;
        } else {
          let hash = ScalaJS.idHashCodeMap["get"](obj);
          if (hash === void 0) {
            hash = (ScalaJS.lastIDHash + 1) | 0;
            ScalaJS.lastIDHash = hash;
            ScalaJS.idHashCodeMap["set"](obj, hash);
          }
          return hash;
        }
    }
//!if outputMode != ECMAScript6
  }) :
  (function(obj) {
    if (ScalaJS.isScalaJSObject(obj)) {
      let hash = obj["$idHashCode$0"];
      if (hash !== void 0) {
        return hash;
      } else if (!ScalaJS.g["Object"]["isSealed"](obj)) {
        hash = (ScalaJS.lastIDHash + 1) | 0;
        ScalaJS.lastIDHash = hash;
        obj["$idHashCode$0"] = hash;
        return hash;
      } else {
        return 42;
      }
    } else if (obj === null) {
      return 0;
    } else {
      return ScalaJS.objectHashCode(obj);
    }
//!endif
  });

// is/as for hijacked boxed classes (the non-trivial ones)

ScalaJS.isByte = function(v) {
  return (v << 24 >> 24) === v && 1/v !== 1/-0;
};

ScalaJS.isShort = function(v) {
  return (v << 16 >> 16) === v && 1/v !== 1/-0;
};

ScalaJS.isInt = function(v) {
  return (v | 0) === v && 1/v !== 1/-0;
};

ScalaJS.isFloat = function(v) {
//!if floats == Strict
  return v !== v || ScalaJS.fround(v) === v;
//!else
  return typeof v === "number";
//!endif
};

//!if asInstanceOfs != Unchecked
ScalaJS.asUnit = function(v) {
  if (v === void 0 || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "scala.runtime.BoxedUnit");
};

ScalaJS.asBoolean = function(v) {
  if (typeof v === "boolean" || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Boolean");
};

ScalaJS.asByte = function(v) {
  if (ScalaJS.isByte(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Byte");
};

ScalaJS.asShort = function(v) {
  if (ScalaJS.isShort(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Short");
};

ScalaJS.asInt = function(v) {
  if (ScalaJS.isInt(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Integer");
};

ScalaJS.asFloat = function(v) {
  if (ScalaJS.isFloat(v) || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Float");
};

ScalaJS.asDouble = function(v) {
  if (typeof v === "number" || v === null)
    return v;
  else
    ScalaJS.throwClassCastException(v, "java.lang.Double");
};
//!endif

// Unboxes

//!if asInstanceOfs != Unchecked
ScalaJS.uZ = function(value) {
  return !!ScalaJS.asBoolean(value);
};
ScalaJS.uB = function(value) {
  return ScalaJS.asByte(value) | 0;
};
ScalaJS.uS = function(value) {
  return ScalaJS.asShort(value) | 0;
};
ScalaJS.uI = function(value) {
  return ScalaJS.asInt(value) | 0;
};
ScalaJS.uJ = function(value) {
  return null === value ? ScalaJS.m.sjsr_RuntimeLong$().Zero$1
                        : ScalaJS.as.sjsr_RuntimeLong(value);
};
ScalaJS.uF = function(value) {
  /* Here, it is fine to use + instead of fround, because asFloat already
   * ensures that the result is either null or a float.
   */
  return +ScalaJS.asFloat(value);
};
ScalaJS.uD = function(value) {
  return +ScalaJS.asDouble(value);
};
//!else
ScalaJS.uJ = function(value) {
  return null === value ? ScalaJS.m.sjsr_RuntimeLong$().Zero$1 : value;
};
//!endif

// TypeArray conversions

ScalaJS.byteArray2TypedArray = function(value) { return new ScalaJS.g["Int8Array"](value.u); };
ScalaJS.shortArray2TypedArray = function(value) { return new ScalaJS.g["Int16Array"](value.u); };
ScalaJS.charArray2TypedArray = function(value) { return new ScalaJS.g["Uint16Array"](value.u); };
ScalaJS.intArray2TypedArray = function(value) { return new ScalaJS.g["Int32Array"](value.u); };
ScalaJS.floatArray2TypedArray = function(value) { return new ScalaJS.g["Float32Array"](value.u); };
ScalaJS.doubleArray2TypedArray = function(value) { return new ScalaJS.g["Float64Array"](value.u); };

ScalaJS.typedArray2ByteArray = function(value) {
  const arrayClassData = ScalaJS.d.B.getArrayOf();
  return new arrayClassData.constr(new ScalaJS.g["Int8Array"](value));
};
ScalaJS.typedArray2ShortArray = function(value) {
  const arrayClassData = ScalaJS.d.S.getArrayOf();
  return new arrayClassData.constr(new ScalaJS.g["Int16Array"](value));
};
ScalaJS.typedArray2CharArray = function(value) {
  const arrayClassData = ScalaJS.d.C.getArrayOf();
  return new arrayClassData.constr(new ScalaJS.g["Uint16Array"](value));
};
ScalaJS.typedArray2IntArray = function(value) {
  const arrayClassData = ScalaJS.d.I.getArrayOf();
  return new arrayClassData.constr(new ScalaJS.g["Int32Array"](value));
};
ScalaJS.typedArray2FloatArray = function(value) {
  const arrayClassData = ScalaJS.d.F.getArrayOf();
  return new arrayClassData.constr(new ScalaJS.g["Float32Array"](value));
};
ScalaJS.typedArray2DoubleArray = function(value) {
  const arrayClassData = ScalaJS.d.D.getArrayOf();
  return new arrayClassData.constr(new ScalaJS.g["Float64Array"](value));
};

// TypeData class

//!if outputMode != ECMAScript6
/** @constructor */
ScalaJS.TypeData = function() {
//!else
class $TypeData {
constructor() {
//!endif
  // Runtime support
  this.constr = void 0;
  this.parentData = void 0;
  this.ancestors = null;
  this.componentData = null;
  this.arrayBase = null;
  this.arrayDepth = 0;
  this.zero = null;
  this.arrayEncodedName = "";
  this._classOf = void 0;
  this._arrayOf = void 0;
  this.isArrayOf = void 0;

  // java.lang.Class support
  this["name"] = "";
  this["isPrimitive"] = false;
  this["isInterface"] = false;
  this["isArrayClass"] = false;
  this["isRawJSType"] = false;
  this["isInstance"] = void 0;
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype.initPrim = function(
//!else
initPrim(
//!endif
    zero, arrayEncodedName, displayName) {
  // Runtime support
  this.ancestors = {};
  this.componentData = null;
  this.zero = zero;
  this.arrayEncodedName = arrayEncodedName;
  this.isArrayOf = function(obj, depth) { return false; };

  // java.lang.Class support
  this["name"] = displayName;
  this["isPrimitive"] = true;
  this["isInstance"] = function(obj) { return false; };

  return this;
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype.initClass = function(
//!else
initClass(
//!endif
    internalNameObj, isInterface, fullName,
    ancestors, isRawJSType, parentData, isInstance, isArrayOf) {
  const internalName = ScalaJS.propertyName(internalNameObj);

  isInstance = isInstance || function(obj) {
    return !!(obj && obj.$classData && obj.$classData.ancestors[internalName]);
  };

  isArrayOf = isArrayOf || function(obj, depth) {
    return !!(obj && obj.$classData && (obj.$classData.arrayDepth === depth)
      && obj.$classData.arrayBase.ancestors[internalName])
  };

  // Runtime support
  this.parentData = parentData;
  this.ancestors = ancestors;
  this.arrayEncodedName = "L"+fullName+";";
  this.isArrayOf = isArrayOf;

  // java.lang.Class support
  this["name"] = fullName;
  this["isInterface"] = isInterface;
  this["isRawJSType"] = !!isRawJSType;
  this["isInstance"] = isInstance;

  return this;
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype.initArray = function(
//!else
initArray(
//!endif
    componentData) {
  // The constructor

  const componentZero0 = componentData.zero;

  // The zero for the Long runtime representation
  // is a special case here, since the class has not
  // been defined yet, when this file is read
  const componentZero = (componentZero0 == "longZero")
    ? ScalaJS.m.sjsr_RuntimeLong$().Zero$1
    : componentZero0;

//!if outputMode != ECMAScript6
  /** @constructor */
  const ArrayClass = function(arg) {
    if (typeof(arg) === "number") {
      // arg is the length of the array
      this.u = new Array(arg);
      for (let i = 0; i < arg; i++)
        this.u[i] = componentZero;
    } else {
      // arg is a native array that we wrap
      this.u = arg;
    }
  }
  ArrayClass.prototype = new ScalaJS.h.O;
  ArrayClass.prototype.constructor = ArrayClass;

//!if arrayIndexOutOfBounds != Unchecked
  ArrayClass.prototype.get = function(i) {
    if (i < 0 || i >= this.u.length)
      ScalaJS.throwArrayIndexOutOfBoundsException(i);
    return this.u[i];
  };
  ArrayClass.prototype.set = function(i, v) {
    if (i < 0 || i >= this.u.length)
      ScalaJS.throwArrayIndexOutOfBoundsException(i);
    this.u[i] = v;
  };
//!endif

  ArrayClass.prototype.clone__O = function() {
    if (this.u instanceof Array)
      return new ArrayClass(this.u["slice"](0));
    else
      // The underlying Array is a TypedArray
      return new ArrayClass(new this.u.constructor(this.u));
  };
//!else
  class ArrayClass extends ScalaJS.c.O {
    constructor(arg) {
      super();
      if (typeof(arg) === "number") {
        // arg is the length of the array
        this.u = new Array(arg);
        for (let i = 0; i < arg; i++)
          this.u[i] = componentZero;
      } else {
        // arg is a native array that we wrap
        this.u = arg;
      }
    };

//!if arrayIndexOutOfBounds != Unchecked
    get(i) {
      if (i < 0 || i >= this.u.length)
        ScalaJS.throwArrayIndexOutOfBoundsException(i);
      return this.u[i];
    };
    set(i, v) {
      if (i < 0 || i >= this.u.length)
        ScalaJS.throwArrayIndexOutOfBoundsException(i);
      this.u[i] = v;
    };
//!endif

    clone__O() {
      if (this.u instanceof Array)
        return new ArrayClass(this.u["slice"](0));
      else
        // The underlying Array is a TypedArray
        return new ArrayClass(new this.u.constructor(this.u));
    };
  };
//!endif

  ArrayClass.prototype.$classData = this;

  // Don't generate reflective call proxies. The compiler special cases
  // reflective calls to methods on scala.Array

  // The data

  const encodedName = "[" + componentData.arrayEncodedName;
  const componentBase = componentData.arrayBase || componentData;
  const arrayDepth = componentData.arrayDepth + 1;

  const isInstance = function(obj) {
    return componentBase.isArrayOf(obj, arrayDepth);
  }

  // Runtime support
  this.constr = ArrayClass;
  this.parentData = ScalaJS.d.O;
  this.ancestors = {O: 1, jl_Cloneable: 1, Ljava_io_Serializable: 1};
  this.componentData = componentData;
  this.arrayBase = componentBase;
  this.arrayDepth = arrayDepth;
  this.zero = null;
  this.arrayEncodedName = encodedName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isArrayOf = undefined;

  // java.lang.Class support
  this["name"] = encodedName;
  this["isPrimitive"] = false;
  this["isInterface"] = false;
  this["isArrayClass"] = true;
  this["isInstance"] = isInstance;

  return this;
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype.getClassOf = function() {
//!else
getClassOf() {
//!endif
  if (!this._classOf)
    this._classOf = new ScalaJS.c.jl_Class().init___jl_ScalaJSClassData(this);
  return this._classOf;
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype.getArrayOf = function() {
//!else
getArrayOf() {
//!endif
  if (!this._arrayOf)
    this._arrayOf = new ScalaJS.TypeData().initArray(this);
  return this._arrayOf;
};

// java.lang.Class support

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype["getFakeInstance"] = function() {
//!else
"getFakeInstance"() {
//!endif
  if (this === ScalaJS.d.T)
    return "some string";
  else if (this === ScalaJS.d.jl_Boolean)
    return false;
  else if (this === ScalaJS.d.jl_Byte ||
           this === ScalaJS.d.jl_Short ||
           this === ScalaJS.d.jl_Integer ||
           this === ScalaJS.d.jl_Float ||
           this === ScalaJS.d.jl_Double)
    return 0;
  else if (this === ScalaJS.d.jl_Long)
    return ScalaJS.m.sjsr_RuntimeLong$().Zero$1;
  else if (this === ScalaJS.d.sr_BoxedUnit)
    return void 0;
  else
    return {$classData: this};
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype["getSuperclass"] = function() {
//!else
"getSuperclass"() {
//!endif
  return this.parentData ? this.parentData.getClassOf() : null;
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype["getComponentType"] = function() {
//!else
"getComponentType"() {
//!endif
  return this.componentData ? this.componentData.getClassOf() : null;
};

//!if outputMode != ECMAScript6
ScalaJS.TypeData.prototype["newArrayOfThisClass"] = function(lengths) {
//!else
"newArrayOfThisClass"(lengths) {
//!endif
  let arrayClassData = this;
  for (let i = 0; i < lengths.length; i++)
    arrayClassData = arrayClassData.getArrayOf();
  return ScalaJS.newArrayObject(arrayClassData, lengths);
};
//!if outputMode == ECMAScript6
};
//!endif

// Create primitive types

ScalaJS.d.V = new ScalaJS.TypeData().initPrim(undefined, "V", "void");
ScalaJS.d.Z = new ScalaJS.TypeData().initPrim(false, "Z", "boolean");
ScalaJS.d.C = new ScalaJS.TypeData().initPrim(0, "C", "char");
ScalaJS.d.B = new ScalaJS.TypeData().initPrim(0, "B", "byte");
ScalaJS.d.S = new ScalaJS.TypeData().initPrim(0, "S", "short");
ScalaJS.d.I = new ScalaJS.TypeData().initPrim(0, "I", "int");
ScalaJS.d.J = new ScalaJS.TypeData().initPrim("longZero", "J", "long");
ScalaJS.d.F = new ScalaJS.TypeData().initPrim(0.0, "F", "float");
ScalaJS.d.D = new ScalaJS.TypeData().initPrim(0.0, "D", "double");

// Instance tests for array of primitives

ScalaJS.isArrayOf.Z = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.Z);
ScalaJS.d.Z.isArrayOf = ScalaJS.isArrayOf.Z;

ScalaJS.isArrayOf.C = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.C);
ScalaJS.d.C.isArrayOf = ScalaJS.isArrayOf.C;

ScalaJS.isArrayOf.B = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.B);
ScalaJS.d.B.isArrayOf = ScalaJS.isArrayOf.B;

ScalaJS.isArrayOf.S = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.S);
ScalaJS.d.S.isArrayOf = ScalaJS.isArrayOf.S;

ScalaJS.isArrayOf.I = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.I);
ScalaJS.d.I.isArrayOf = ScalaJS.isArrayOf.I;

ScalaJS.isArrayOf.J = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.J);
ScalaJS.d.J.isArrayOf = ScalaJS.isArrayOf.J;

ScalaJS.isArrayOf.F = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.F);
ScalaJS.d.F.isArrayOf = ScalaJS.isArrayOf.F;

ScalaJS.isArrayOf.D = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.d.D);
ScalaJS.d.D.isArrayOf = ScalaJS.isArrayOf.D;

//!if asInstanceOfs != Unchecked
// asInstanceOfs for array of primitives
ScalaJS.asArrayOf.Z = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.Z, "Z");
ScalaJS.asArrayOf.C = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.C, "C");
ScalaJS.asArrayOf.B = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.B, "B");
ScalaJS.asArrayOf.S = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.S, "S");
ScalaJS.asArrayOf.I = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.I, "I");
ScalaJS.asArrayOf.J = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.J, "J");
ScalaJS.asArrayOf.F = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.F, "F");
ScalaJS.asArrayOf.D = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.D, "D");
//!endif
