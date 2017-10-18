/* Scala.js runtime support
 * Copyright 2013 LAMP/EPFL
 * Author: Sébastien Doeraene
 */

/* ---------------------------------- *
 * The top-level Scala.js environment *
 * ---------------------------------- */

// Where to send exports
//!if moduleKind == CommonJSModule
const $e = exports;
//!else
// TODO Do not use global object detection, and rather export with actual `var` declarations
const $e = (typeof global === "object" && global && global["Object"] === Object) ? global : this;
// #3036 - convince GCC that $e must not be dce'ed away
this["__ScalaJSWorkaroundToRetainExportsInGCC"] = $e;
//!endif

// Linking info - must be in sync with scala.scalajs.runtime.LinkingInfo
const $linkingInfo = {
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
  "linkerVersion": "{{LINKER_VERSION}}",
  "globalThis": this
};
Object["freeze"]($linkingInfo);
Object["freeze"]($linkingInfo["semantics"]);

// Snapshots of builtins and polyfills

//!if outputMode == ECMAScript6
const $imul = Math["imul"];
const $fround = Math["fround"];
const $clz32 = Math["clz32"];
//!else
const $imul = Math["imul"] || (function(a, b) {
  // See https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math/imul
  const ah = (a >>> 16) & 0xffff;
  const al = a & 0xffff;
  const bh = (b >>> 16) & 0xffff;
  const bl = b & 0xffff;
  // the shift by 0 fixes the sign on the high part
  // the final |0 converts the unsigned value into a signed value
  return ((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0);
});

const $fround = Math["fround"] ||
//!if floats == Strict
  (typeof Float32Array !== "undefined" ? (function(v) {
    const array = new Float32Array(1);
    array[0] = v;
    return array[0];
  }) : (function(v) {
    return $m_sjsr_package$().froundPolyfill__D__D(+v);
  }));
//!else
  (function(v) {
    return +v;
  });
//!endif

const $clz32 = Math["clz32"] || (function(i) {
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

// Cached instance of RuntimeLong for 0L
let $L0;

// identityHashCode support
let $lastIDHash = 0; // last value attributed to an id hash code
//!if outputMode == ECMAScript6
const $idHashCodeMap = new WeakMap();
//!else
const $idHashCodeMap = typeof WeakMap !== "undefined" ? new WeakMap() : null;
//!endif

// Core mechanism

function $makeIsArrayOfPrimitive(primitiveData) {
  return function(obj, depth) {
    return !!(obj && obj.$classData &&
      (obj.$classData.arrayDepth === depth) &&
      (obj.$classData.arrayBase === primitiveData));
  }
};

//!if asInstanceOfs != Unchecked
function $makeAsArrayOfPrimitive(isInstanceOfFunction, arrayEncodedName) {
  return function(obj, depth) {
    if (isInstanceOfFunction(obj, depth) || (obj === null))
      return obj;
    else
      $throwArrayCastException(obj, arrayEncodedName, depth);
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
function $propertyName(obj) {
  for (const prop in obj)
    return prop;
};

// Boxed Char

//!if outputMode == ECMAScript6
class $Char {
  constructor(c) {
    this.c = c;
  }
  toString() {
    return String["fromCharCode"](this.c);
  }
}
//!else
function $Char(c) {
  this.c = c;
};
$Char.prototype.toString = (function() {
  return String["fromCharCode"](this.c);
});
//!endif

// Runtime functions

function $isScalaJSObject(obj) {
  return !!(obj && obj.$classData);
};

//!if asInstanceOfs != Unchecked
function $throwClassCastException(instance, classFullName) {
//!if asInstanceOfs == Compliant
  throw new $c_jl_ClassCastException().init___T(
    instance + " is not an instance of " + classFullName);
//!else
  throw new $c_sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new $c_jl_ClassCastException().init___T(
      instance + " is not an instance of " + classFullName));
//!endif
};

function $throwArrayCastException(instance, classArrayEncodedName, depth) {
  for (; depth; --depth)
    classArrayEncodedName = "[" + classArrayEncodedName;
  $throwClassCastException(instance, classArrayEncodedName);
};
//!endif

//!if arrayIndexOutOfBounds != Unchecked
function $throwArrayIndexOutOfBoundsException(i) {
  const msg = (i === null) ? null : ("" + i);
//!if arrayIndexOutOfBounds == Compliant
  throw new $c_jl_ArrayIndexOutOfBoundsException().init___T(msg);
//!else
  throw new $c_sjsr_UndefinedBehaviorError().init___jl_Throwable(
    new $c_jl_ArrayIndexOutOfBoundsException().init___T(msg));
//!endif
};
//!endif

function $noIsInstance(instance) {
  throw new TypeError(
    "Cannot call isInstance() on a Class representing a raw JS trait/object");
};

function $makeNativeArrayWrapper(arrayClassData, nativeArray) {
  return new arrayClassData.constr(nativeArray);
};

function $newArrayObject(arrayClassData, lengths) {
  return $newArrayObjectInternal(arrayClassData, lengths, 0);
};

function $newArrayObjectInternal(arrayClassData, lengths, lengthIndex) {
  const result = new arrayClassData.constr(lengths[lengthIndex]);

  if (lengthIndex < lengths.length-1) {
    const subArrayClassData = arrayClassData.componentData;
    const subLengthIndex = lengthIndex+1;
    const underlying = result.u;
    for (let i = 0; i < underlying.length; i++) {
      underlying[i] = $newArrayObjectInternal(
        subArrayClassData, lengths, subLengthIndex);
    }
  }

  return result;
};

function $objectToString(instance) {
  if (instance === void 0)
    return "undefined";
  else
    return instance.toString();
};

function $objectGetClass(instance) {
  switch (typeof instance) {
    case "string":
      return $d_T.getClassOf();
    case "number": {
      const v = instance | 0;
      if (v === instance) { // is the value integral?
        if ($isByte(v))
          return $d_jl_Byte.getClassOf();
        else if ($isShort(v))
          return $d_jl_Short.getClassOf();
        else
          return $d_jl_Integer.getClassOf();
      } else {
        if ($isFloat(instance))
          return $d_jl_Float.getClassOf();
        else
          return $d_jl_Double.getClassOf();
      }
    }
    case "boolean":
      return $d_jl_Boolean.getClassOf();
    case "undefined":
      return $d_sr_BoxedUnit.getClassOf();
    default:
      if (instance === null)
        return instance.getClass__jl_Class();
      else if ($is_sjsr_RuntimeLong(instance))
        return $d_jl_Long.getClassOf();
      else if ($isChar(instance))
        return $d_jl_Character.getClassOf();
      else if ($isScalaJSObject(instance))
        return instance.$classData.getClassOf();
      else
        return null; // Exception?
  }
};

function $objectClone(instance) {
  if ($isScalaJSObject(instance) || (instance === null))
    return instance.clone__O();
  else
    throw new $c_jl_CloneNotSupportedException().init___();
};

function $objectNotify(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notify__V();
};

function $objectNotifyAll(instance) {
  // final and no-op in java.lang.Object
  if (instance === null)
    instance.notifyAll__V();
};

function $objectFinalize(instance) {
  if ($isScalaJSObject(instance) || (instance === null))
    instance.finalize__V();
  // else no-op
};

function $objectEquals(instance, rhs) {
  if ($isScalaJSObject(instance) || (instance === null))
    return instance.equals__O__Z(rhs);
  else if (typeof instance === "number")
    return typeof rhs === "number" && $numberEquals(instance, rhs);
  else if ($isChar(instance))
    return $isChar(rhs) && instance.c === rhs.c;
  else
    return instance === rhs;
};

function $numberEquals(lhs, rhs) {
  return (lhs === rhs) ? (
    // 0.0.equals(-0.0) must be false
    lhs !== 0 || 1/lhs === 1/rhs
  ) : (
    // are they both NaN?
    (lhs !== lhs) && (rhs !== rhs)
  );
};

function $objectHashCode(instance) {
  switch (typeof instance) {
    case "string":
      return $m_sjsr_RuntimeString$().hashCode__T__I(instance);
    case "number":
      return $m_sjsr_Bits$().numberHashCode__D__I(instance);
    case "boolean":
      return instance ? 1231 : 1237;
    case "undefined":
      return 0;
    default:
      if ($isScalaJSObject(instance) || instance === null)
        return instance.hashCode__I();
      else if ($isChar(instance))
        return instance.c;
//!if outputMode != ECMAScript6
      else if ($idHashCodeMap === null)
        return 42;
//!endif
      else
        return $systemIdentityHashCode(instance);
  }
};

function $comparableCompareTo(instance, rhs) {
  switch (typeof instance) {
    case "string":
//!if asInstanceOfs != Unchecked
      $as_T(rhs);
//!endif
      return instance === rhs ? 0 : (instance < rhs ? -1 : 1);
    case "number":
//!if asInstanceOfs != Unchecked
      $as_jl_Number(rhs);
//!endif
      return $m_jl_Double$().compare__D__D__I(instance, rhs);
    case "boolean":
//!if asInstanceOfs != Unchecked
      $asBoolean(rhs);
//!endif
      return instance - rhs; // yes, this gives the right result
    default:
      if ($isChar(instance)) {
//!if asInstanceOfs != Unchecked
        $asChar(rhs);
//!endif
        return instance.c - rhs.c;
      } else {
        return instance.compareTo__O__I(rhs);
      }
  }
};

function $charSequenceLength(instance) {
  if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
    return $uI(instance["length"]);
//!else
    return instance["length"] | 0;
//!endif
  else
    return instance.length__I();
};

function $charSequenceCharAt(instance, index) {
  if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
    return $uI(instance["charCodeAt"](index)) & 0xffff;
//!else
    return instance["charCodeAt"](index) & 0xffff;
//!endif
  else
    return instance.charAt__I__C(index);
};

function $charSequenceSubSequence(instance, start, end) {
  if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
    return $as_T(instance["substring"](start, end));
//!else
    return instance["substring"](start, end);
//!endif
  else
    return instance.subSequence__I__I__jl_CharSequence(start, end);
};

function $booleanBooleanValue(instance) {
  if (typeof instance === "boolean") return instance;
  else                               return instance.booleanValue__Z();
};

function $characterCharValue(instance) {
  return instance.c;
}

function $numberByteValue(instance) {
  if (typeof instance === "number") return (instance << 24) >> 24;
  else                              return instance.byteValue__B();
};
function $numberShortValue(instance) {
  if (typeof instance === "number") return (instance << 16) >> 16;
  else                              return instance.shortValue__S();
};
function $numberIntValue(instance) {
  if (typeof instance === "number") return instance | 0;
  else                              return instance.intValue__I();
};
function $numberLongValue(instance) {
  if (typeof instance === "number")
    return $m_sjsr_RuntimeLong$().fromDouble__D__sjsr_RuntimeLong(instance);
  else
    return instance.longValue__J();
};
function $numberFloatValue(instance) {
  if (typeof instance === "number") return $fround(instance);
  else                              return instance.floatValue__F();
};
function $numberDoubleValue(instance) {
  if (typeof instance === "number") return instance;
  else                              return instance.doubleValue__D();
};

function $isNaN(instance) {
  return instance !== instance;
};

function $isInfinite(instance) {
  return !isFinite(instance) && !$isNaN(instance);
};

function $doubleToInt(x) {
  return (x > 2147483647) ? (2147483647) : ((x < -2147483648) ? -2147483648 : (x | 0));
};

/** Instantiates a JS object with variadic arguments to the constructor. */
function $newJSObjectWithVarargs(ctor, args) {
  // This basically emulates the ECMAScript specification for 'new'.
  const instance = Object["create"](ctor.prototype);
  const result = ctor["apply"](instance, args);
  switch (typeof result) {
    case "string": case "number": case "boolean": case "undefined": case "symbol":
      return instance;
    default:
      return result === null ? instance : result;
  }
};

function $resolveSuperRef(superClass, propName) {
  const getPrototypeOf = Object["getPrototypeOf"];
  const getOwnPropertyDescriptor = Object["getOwnPropertyDescriptor"];

  let superProto = superClass.prototype;
  while (superProto !== null) {
    const desc = getOwnPropertyDescriptor(superProto, propName);
    if (desc !== void 0)
      return desc;
    superProto = getPrototypeOf(superProto);
  }

  return void 0;
};

function $superGet(superClass, self, propName) {
  const desc = $resolveSuperRef(superClass, propName);
  if (desc !== void 0) {
    const getter = desc["get"];
    if (getter !== void 0)
      return getter["call"](self);
    else
      return desc["value"];
  }
  return void 0;
};

function $superSet(superClass, self, propName, value) {
  const desc = $resolveSuperRef(superClass, propName);
  if (desc !== void 0) {
    const setter = desc["set"];
    if (setter !== void 0) {
      setter["call"](self, value);
      return void 0;
    }
  }
  throw new TypeError("super has no setter '" + propName + "'.");
};

//!if moduleKind == CommonJSModule
function $moduleDefault(m) {
  return (m && (typeof m === "object") && "default" in m) ? m["default"] : m;
};
//!endif

function $propertiesOf(obj) {
  const result = [];
  for (const prop in obj)
    result["push"](prop);
  return result;
};

function $systemArraycopy(src, srcPos, dest, destPos, length) {
  const srcu = src.u;
  const destu = dest.u;

//!if arrayIndexOutOfBounds != Unchecked
  if (srcPos < 0 || destPos < 0 || length < 0 ||
      (srcPos > ((srcu.length - length) | 0)) ||
      (destPos > ((destu.length - length) | 0))) {
    $throwArrayIndexOutOfBoundsException(null);
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

const $systemIdentityHashCode =
//!if outputMode != ECMAScript6
  ($idHashCodeMap !== null) ?
//!endif
  (function(obj) {
    switch (typeof obj) {
      case "string": case "number": case "boolean": case "undefined":
        return $objectHashCode(obj);
      default:
        if (obj === null) {
          return 0;
        } else {
          let hash = $idHashCodeMap["get"](obj);
          if (hash === void 0) {
            hash = ($lastIDHash + 1) | 0;
            $lastIDHash = hash;
            $idHashCodeMap["set"](obj, hash);
          }
          return hash;
        }
    }
//!if outputMode != ECMAScript6
  }) :
  (function(obj) {
    if ($isScalaJSObject(obj)) {
      let hash = obj["$idHashCode$0"];
      if (hash !== void 0) {
        return hash;
      } else if (!Object["isSealed"](obj)) {
        hash = ($lastIDHash + 1) | 0;
        $lastIDHash = hash;
        obj["$idHashCode$0"] = hash;
        return hash;
      } else {
        return 42;
      }
    } else if (obj === null) {
      return 0;
    } else {
      return $objectHashCode(obj);
    }
//!endif
  });

// is/as for hijacked boxed classes (the non-trivial ones)

function $isChar(v) {
  return v instanceof $Char;
};

function $isByte(v) {
  return typeof v === "number" && (v << 24 >> 24) === v && 1/v !== 1/-0;
};

function $isShort(v) {
  return typeof v === "number" && (v << 16 >> 16) === v && 1/v !== 1/-0;
};

function $isInt(v) {
  return typeof v === "number" && (v | 0) === v && 1/v !== 1/-0;
};

function $isFloat(v) {
//!if floats == Strict
  return typeof v === "number" && (v !== v || $fround(v) === v);
//!else
  return typeof v === "number";
//!endif
};

//!if asInstanceOfs != Unchecked
function $asUnit(v) {
  if (v === void 0 || v === null)
    return v;
  else
    $throwClassCastException(v, "scala.runtime.BoxedUnit");
};

function $asBoolean(v) {
  if (typeof v === "boolean" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Boolean");
};

function $asChar(v) {
  if (v instanceof $Char || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Character");
};

function $asByte(v) {
  if ($isByte(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Byte");
};

function $asShort(v) {
  if ($isShort(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Short");
};

function $asInt(v) {
  if ($isInt(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Integer");
};

function $asFloat(v) {
  if ($isFloat(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Float");
};

function $asDouble(v) {
  if (typeof v === "number" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Double");
};
//!endif

// Boxes

function $bC(c) {
  return new $Char(c);
}
const $bC0 = $bC(0);

// Unboxes

//!if asInstanceOfs != Unchecked
function $uZ(value) {
  return !!$asBoolean(value);
};
function $uC(value) {
  return null === value ? 0 : $asChar(value).c;
};
function $uB(value) {
  return $asByte(value) | 0;
};
function $uS(value) {
  return $asShort(value) | 0;
};
function $uI(value) {
  return $asInt(value) | 0;
};
function $uJ(value) {
  return null === value ? $L0 : $as_sjsr_RuntimeLong(value);
};
function $uF(value) {
  /* Here, it is fine to use + instead of fround, because asFloat already
   * ensures that the result is either null or a float.
   */
  return +$asFloat(value);
};
function $uD(value) {
  return +$asDouble(value);
};
//!else
function $uC(value) {
  return null === value ? 0 : value.c;
}
function $uJ(value) {
  return null === value ? $L0 : value;
};
//!endif

// TypeArray conversions

function $byteArray2TypedArray(value) { return new Int8Array(value.u); };
function $shortArray2TypedArray(value) { return new Int16Array(value.u); };
function $charArray2TypedArray(value) { return new Uint16Array(value.u); };
function $intArray2TypedArray(value) { return new Int32Array(value.u); };
function $floatArray2TypedArray(value) { return new Float32Array(value.u); };
function $doubleArray2TypedArray(value) { return new Float64Array(value.u); };

function $typedArray2ByteArray(value) {
  const arrayClassData = $d_B.getArrayOf();
  return new arrayClassData.constr(new Int8Array(value));
};
function $typedArray2ShortArray(value) {
  const arrayClassData = $d_S.getArrayOf();
  return new arrayClassData.constr(new Int16Array(value));
};
function $typedArray2CharArray(value) {
  const arrayClassData = $d_C.getArrayOf();
  return new arrayClassData.constr(new Uint16Array(value));
};
function $typedArray2IntArray(value) {
  const arrayClassData = $d_I.getArrayOf();
  return new arrayClassData.constr(new Int32Array(value));
};
function $typedArray2FloatArray(value) {
  const arrayClassData = $d_F.getArrayOf();
  return new arrayClassData.constr(new Float32Array(value));
};
function $typedArray2DoubleArray(value) {
  const arrayClassData = $d_D.getArrayOf();
  return new arrayClassData.constr(new Float64Array(value));
};

// TypeData class

//!if outputMode != ECMAScript6
/** @constructor */
function $TypeData() {
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
$TypeData.prototype.initPrim = function(
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
$TypeData.prototype.initClass = function(
//!else
initClass(
//!endif
    internalNameObj, isInterface, fullName,
    ancestors, isRawJSType, parentData, isInstance, isArrayOf) {
  const internalName = $propertyName(internalNameObj);

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
$TypeData.prototype.initArray = function(
//!else
initArray(
//!endif
    componentData) {
  // The constructor

  const componentZero0 = componentData.zero;

  // The zero for the Long runtime representation
  // is a special case here, since the class has not
  // been defined yet when this constructor is called.
  const componentZero = (componentZero0 == "longZero") ? $L0 : componentZero0;

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
  ArrayClass.prototype = new $h_O;
  ArrayClass.prototype.constructor = ArrayClass;

//!if arrayIndexOutOfBounds != Unchecked
  ArrayClass.prototype.get = function(i) {
    if (i < 0 || i >= this.u.length)
      $throwArrayIndexOutOfBoundsException(i);
    return this.u[i];
  };
  ArrayClass.prototype.set = function(i, v) {
    if (i < 0 || i >= this.u.length)
      $throwArrayIndexOutOfBoundsException(i);
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
  class ArrayClass extends $c_O {
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
        $throwArrayIndexOutOfBoundsException(i);
      return this.u[i];
    };
    set(i, v) {
      if (i < 0 || i >= this.u.length)
        $throwArrayIndexOutOfBoundsException(i);
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
  this.parentData = $d_O;
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
$TypeData.prototype.getClassOf = function() {
//!else
getClassOf() {
//!endif
  if (!this._classOf)
    this._classOf = new $c_jl_Class(this);
  return this._classOf;
};

//!if outputMode != ECMAScript6
$TypeData.prototype.getArrayOf = function() {
//!else
getArrayOf() {
//!endif
  if (!this._arrayOf)
    this._arrayOf = new $TypeData().initArray(this);
  return this._arrayOf;
};

// java.lang.Class support

//!if outputMode != ECMAScript6
$TypeData.prototype["getFakeInstance"] = function() {
//!else
"getFakeInstance"() {
//!endif
  if (this === $d_T)
    return "some string";
  else if (this === $d_jl_Boolean)
    return false;
  else if (this === $d_jl_Byte ||
           this === $d_jl_Short ||
           this === $d_jl_Integer ||
           this === $d_jl_Float ||
           this === $d_jl_Double)
    return 0;
  else if (this === $d_jl_Long)
    return $L0;
  else if (this === $d_sr_BoxedUnit)
    return void 0;
  else
    return {$classData: this};
};

//!if outputMode != ECMAScript6
$TypeData.prototype["getSuperclass"] = function() {
//!else
"getSuperclass"() {
//!endif
  return this.parentData ? this.parentData.getClassOf() : null;
};

//!if outputMode != ECMAScript6
$TypeData.prototype["getComponentType"] = function() {
//!else
"getComponentType"() {
//!endif
  return this.componentData ? this.componentData.getClassOf() : null;
};

//!if outputMode != ECMAScript6
$TypeData.prototype["newArrayOfThisClass"] = function(lengths) {
//!else
"newArrayOfThisClass"(lengths) {
//!endif
  let arrayClassData = this;
  for (let i = 0; i < lengths.length; i++)
    arrayClassData = arrayClassData.getArrayOf();
  return $newArrayObject(arrayClassData, lengths);
};
//!if outputMode == ECMAScript6
};
//!endif

// Create primitive types

const $d_V = new $TypeData().initPrim(undefined, "V", "void");
const $d_Z = new $TypeData().initPrim(false, "Z", "boolean");
const $d_C = new $TypeData().initPrim(0, "C", "char");
const $d_B = new $TypeData().initPrim(0, "B", "byte");
const $d_S = new $TypeData().initPrim(0, "S", "short");
const $d_I = new $TypeData().initPrim(0, "I", "int");
const $d_J = new $TypeData().initPrim("longZero", "J", "long");
const $d_F = new $TypeData().initPrim(0.0, "F", "float");
const $d_D = new $TypeData().initPrim(0.0, "D", "double");

// Instance tests for array of primitives

const $isArrayOf_Z = $makeIsArrayOfPrimitive($d_Z);
$d_Z.isArrayOf = $isArrayOf_Z;

const $isArrayOf_C = $makeIsArrayOfPrimitive($d_C);
$d_C.isArrayOf = $isArrayOf_C;

const $isArrayOf_B = $makeIsArrayOfPrimitive($d_B);
$d_B.isArrayOf = $isArrayOf_B;

const $isArrayOf_S = $makeIsArrayOfPrimitive($d_S);
$d_S.isArrayOf = $isArrayOf_S;

const $isArrayOf_I = $makeIsArrayOfPrimitive($d_I);
$d_I.isArrayOf = $isArrayOf_I;

const $isArrayOf_J = $makeIsArrayOfPrimitive($d_J);
$d_J.isArrayOf = $isArrayOf_J;

const $isArrayOf_F = $makeIsArrayOfPrimitive($d_F);
$d_F.isArrayOf = $isArrayOf_F;

const $isArrayOf_D = $makeIsArrayOfPrimitive($d_D);
$d_D.isArrayOf = $isArrayOf_D;

//!if asInstanceOfs != Unchecked
// asInstanceOfs for array of primitives
const $asArrayOf_Z = $makeAsArrayOfPrimitive($isArrayOf_Z, "Z");
const $asArrayOf_C = $makeAsArrayOfPrimitive($isArrayOf_C, "C");
const $asArrayOf_B = $makeAsArrayOfPrimitive($isArrayOf_B, "B");
const $asArrayOf_S = $makeAsArrayOfPrimitive($isArrayOf_S, "S");
const $asArrayOf_I = $makeAsArrayOfPrimitive($isArrayOf_I, "I");
const $asArrayOf_J = $makeAsArrayOfPrimitive($isArrayOf_J, "J");
const $asArrayOf_F = $makeAsArrayOfPrimitive($isArrayOf_F, "F");
const $asArrayOf_D = $makeAsArrayOfPrimitive($isArrayOf_D, "D");
//!endif
