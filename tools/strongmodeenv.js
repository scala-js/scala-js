// Get the environment info
const $env = (typeof __ScalaJSEnv === "object" && __ScalaJSEnv) ? __ScalaJSEnv : {};

// Global scope
const $g =
  (typeof $env["global"] === "object" && $env["global"])
    ? $env["global"]
    : ((typeof __global === "object" && __global && __global["Object"] === Object) ? __global : __this);
$env["global"] = $g;

// Where to send exports
const $e =
  (typeof $env["exportsNamespace"] === "object" && $env["exportsNamespace"])
    ? $env["exportsNamespace"] : $g;
$env["exportsNamespace"] = $e;

// Freeze the environment info
$g["Object"]["freeze"]($env);

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
    "strictFloats": true
//!else
    "strictFloats": false
//!endif
  },
  "assumingES6": true
};
$g["Object"]["freeze"]($linkingInfo);
$g["Object"]["freeze"]($linkingInfo["semantics"]);

// Snapshots of builtins

const $imul = $g["Math"]["imul"];
const $fround = $g["Math"]["fround"];

// Other fields
let $lastIDHash = 0; // last value attributed to an id hash code
const $idHashCodeMap = new $g["WeakMap"]();
let $longZero = null; // reassigned at the end

//!if asInstanceOfs != Unchecked
let $throwClassCastException = null; // reassigned at the end
let $throwArrayCastException = null; // reassigned at the end
//!endif

// A few helper functions unrelated to Scala

/** Encode a property name for runtime manipulation
 *  Usage:
 *    env.propertyName({someProp:0})
 *  Returns:
 *    "someProp"
 *  Useful when the property is renamed by a global optimizer (like Closure)
 *  but we must still get hold of a string of that name for runtime
 *  reflection.
 */
function $propertyName(obj) {
  return $g["Object"]["keys"](obj)[0];
}

function $noIsInstance(instance) {
  throw new $g["TypeError"](
    "Cannot call isInstance() on a Class representing a raw JS trait/object");
}

/** Emulates `new ctor(...args)`. */
function $newJSObjectWithVarargs(ctor, args) {
  const instance = $g["Object"]["create"](ctor.prototype);
  const result = ctor["apply"](instance, args);
  switch (typeof result) {
    case "string": case "number": case "boolean": case "undefined": case "symbol":
      return instance;
    default:
      return result === null ? instance : result;
  }
}

/** Converts a value to a string for concatenation. */
function $toString(x) {
  return `${x}`;
}

// Declaration of type data

let $d_V = null;
let $d_Z = null;
let $d_C = null;
let $d_B = null;
let $d_S = null;
let $d_I = null;
let $d_J = null;
let $d_F = null;
let $d_D = null;
///INSERT DECLARE TYPE DATA HERE///

// Declaration of module singletons

///INSERT DECLARE MODULES HERE///

// isInstanceOf and asInstanceOf tests

function $isScalaJSObject(obj) {
  // TODO Not Closure-friendly
  return (typeof obj === "object") && (obj !== null) && ("$classData" in obj);
}

// is/as for hijacked boxed classes (the non-trivial ones)

function $isByte(v) {
  return (typeof v === "number") && (v << 24 >> 24) === v && 1/v !== 1/-0;
}

function $isShort(v) {
  return (typeof v === "number") && (v << 16 >> 16) === v && 1/v !== 1/-0;
}

function $isInt(v) {
  return (typeof v === "number") && (v | 0) === v && 1/v !== 1/-0;
}

function $isFloat(v) {
  return (typeof v === "number") && (v !== v || $fround(v) === v);
}

//!if asInstanceOfs != Unchecked
function $asUnit(v) {
  if (v === void 0 || v === null)
    return v;
  else
    $throwClassCastException(v, "scala.runtime.BoxedUnit");
}

function $asBoolean(v) {
  if (typeof v === "boolean" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Boolean");
}

function $asByte(v) {
  if ($isByte(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Byte");
}

function $asShort(v) {
  if ($isShort(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Short");
}

function $asInt(v) {
  if ($isInt(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Integer");
}

function $asFloat(v) {
  if ($isFloat(v) || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Float");
}

function $asDouble(v) {
  if (typeof v === "number" || v === null)
    return v;
  else
    $throwClassCastException(v, "java.lang.Double");
}
//!endif

function $isArrayOf_Z(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_Z);
}

function $isArrayOf_C(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_C);
}

function $isArrayOf_B(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_B);
}

function $isArrayOf_S(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_S);
}

function $isArrayOf_I(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_I);
}

function $isArrayOf_J(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_J);
}

function $isArrayOf_F(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_F);
}

function $isArrayOf_D(obj, depth) {
  return $isScalaJSObject(obj) &&
    (obj.$classData.arrayDepth === depth) &&
    (obj.$classData.arrayBase === $d_D);
}

//!if asInstanceOfs != Unchecked
function $asArrayOf_Z(obj, depth) {
  if ($isArrayOf_Z(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "Z", depth);
}

function $asArrayOf_C(obj, depth) {
  if ($isArrayOf_C(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "C", depth);
}

function $asArrayOf_B(obj, depth) {
  if ($isArrayOf_B(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "B", depth);
}

function $asArrayOf_S(obj, depth) {
  if ($isArrayOf_S(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "S", depth);
}

function $asArrayOf_I(obj, depth) {
  if ($isArrayOf_I(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "I", depth);
}

function $asArrayOf_J(obj, depth) {
  if ($isArrayOf_J(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "J", depth);
}

function $asArrayOf_F(obj, depth) {
  if ($isArrayOf_F(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "F", depth);
}

function $asArrayOf_D(obj, depth) {
  if ($isArrayOf_D(obj, depth) || (obj === null))
    return obj;
  else
    $throwArrayCastException(obj, "D", depth);
}
//!endif

///INSERT IS AND AS FUNCTIONS HERE///

// Unboxes

//!if asInstanceOfs != Unchecked
function $uZ(value) {
  return !!$asBoolean(value);
}
function $uB(value) {
  return ($asByte(value) || 0) | 0;
}
function $uS(value) {
  return ($asShort(value) || 0) | 0;
}
function $uI(value) {
  return ($asInt(value) || 0) | 0;
}
function $uJ(value) {
  return $as_sjsr_RuntimeLong(value) || $longZero;
}
function $uF(value) {
  return value === null ? 0 : $asFloat(value);
}
function $uD(value) {
  return value === null ? 0 : $asDouble(value);
}
//!else
function $uF(value) {
  return value === null ? 0 : $fround(value);
}
function $uD(value) {
  return value === null ? 0 : +value;
}
//!endif

// Support functions

class $ {
  /* These are static methods rather than top-level functions so that they
   * can see the Scala classes defined below.
   */

//!if asInstanceOfs != Unchecked
  static throwClassCastException(instance, classFullName) {
//!if asInstanceOfs == Compliant
    throw new $c_jl_ClassCastException().init___T(
      instance + " is not an instance of " + classFullName);
//!else
    throw new $c_sjsr_UndefinedBehaviorError().init___jl_Throwable(
      new $c_jl_ClassCastException().init___T(
        instance + " is not an instance of " + classFullName));
//!endif
  };

  static throwArrayCastException(instance, classArrayEncodedName, depth) {
    for (; depth; --depth)
      classArrayEncodedName = "[" + classArrayEncodedName;
    $.throwClassCastException(instance, classArrayEncodedName);
  };
//!endif

  static makeNativeArrayWrapper(arrayClassData, nativeArray) {
    return new arrayClassData.constr(nativeArray);
  };

  static newArrayObject(arrayClassData, lengths) {
    return $.newArrayObjectInternal(arrayClassData, lengths, 0);
  };

  static newArrayObjectInternal(arrayClassData, lengths, lengthIndex) {
    const result = new arrayClassData.constr(lengths[lengthIndex]);

    if (lengthIndex < lengths.length-1) {
      const subArrayClassData = arrayClassData.componentData;
      const subLengthIndex = lengthIndex+1;
      const underlying = result.u;
      for (let i = 0; i < underlying.length; i++) {
        underlying[i] = $.newArrayObjectInternal(
          subArrayClassData, lengths, subLengthIndex);
      }
    }

    return result;
  };

  static checkNonNull(obj) {
    return obj !== null ? obj : $.throwNullPointerException();
  };

  static throwNullPointerException() {
    throw new $c_jl_NullPointerException().init___();
  };

  static objectToString(instance) {
    if (instance === void 0)
      return "undefined";
    else
      return instance.toString();
  };

  static objectGetClass(instance) {
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
          $.throwNullPointerException();
        else if ($is_sjsr_RuntimeLong(instance))
          return $d_jl_Long.getClassOf();
        else if ($isScalaJSObject(instance))
          return instance.$classData.getClassOf();
        else
          return null;
    }
  };

  static objectClone(instance) {
    if ($isScalaJSObject(instance) || (instance === null))
      return instance.clone__O();
    else
      throw new $c_jl_CloneNotSupportedException().init___();
  };

  static objectNotify(instance) {
    // final and no-op in java.lang.Object
    if (instance === null)
      instance.notify__V();
  };

  static objectNotifyAll(instance) {
    // final and no-op in java.lang.Object
    if (instance === null)
      instance.notifyAll__V();
  };

  static objectFinalize(instance) {
    if ($isScalaJSObject(instance) || (instance === null))
      instance.finalize__V();
    // else no-op
  };

  static objectEquals(instance, rhs) {
    if ($isScalaJSObject(instance) || (instance === null))
      return instance.equals__O__Z(rhs);
    else if (typeof instance === "number")
      return typeof rhs === "number" && $.numberEquals(instance, rhs);
    else
      return instance === rhs;
  };

  static numberEquals(lhs, rhs) {
    return (lhs === rhs) ? (
      // 0.0.equals(-0.0) must be false
      lhs !== 0 || 1/lhs === 1/rhs
    ) : (
      // are they both NaN?
      (lhs !== lhs) && (rhs !== rhs)
    );
  };

  static objectHashCode(instance) {
    switch (typeof instance) {
      case "string":
        return $c_sjsr_RuntimeString$.__M().hashCode__T__I(instance);
      case "number":
        return $c_sjsr_Bits$.__M().numberHashCode__D__I(instance);
      case "boolean":
        return instance ? 1231 : 1237;
      case "undefined":
        return 0;
      default:
        if ($isScalaJSObject(instance) || instance === null)
          return instance.hashCode__I();
        else
          return $.systemIdentityHashCode(instance);
    }
  };

  static comparableCompareTo(instance, rhs) {
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
        return $c_jl_Double$.__M().compare__D__D__I(instance, rhs);
      case "boolean":
//!if asInstanceOfs != Unchecked
        $asBoolean(rhs);
//!endif
        return instance - rhs; // yes, this gives the right result
      default:
        return instance.compareTo__O__I(rhs);
    }
  };

  static charSequenceLength(instance) {
    if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
      return $uI(instance["length"]);
//!else
      return (instance["length"] || 0) | 0;
//!endif
    else
      return instance.length__I();
  };

  static charSequenceCharAt(instance, index) {
    if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
      return $uI(instance["charCodeAt"](index)) & 0xffff;
//!else
      return (instance["charCodeAt"](index) || 0) & 0xffff;
//!endif
    else
      return instance.charAt__I__C(index);
  };

  static charSequenceSubSequence(instance, start, end) {
    if (typeof(instance) === "string")
//!if asInstanceOfs != Unchecked
      return $as_T(instance["substring"](start, end));
//!else
      return instance["substring"](start, end);
//!endif
    else
      return instance.subSequence__I__I__jl_CharSequence(start, end);
  };

  static booleanBooleanValue(instance) {
    if (typeof instance === "boolean") return instance;
    else                               return instance.booleanValue__Z();
  };

  static numberByteValue(instance) {
    if (typeof instance === "number") return (instance << 24) >> 24;
    else                              return instance.byteValue__B();
  };
  static numberShortValue(instance) {
    if (typeof instance === "number") return (instance << 16) >> 16;
    else                              return instance.shortValue__S();
  };
  static numberIntValue(instance) {
    if (typeof instance === "number") return instance | 0;
    else                              return instance.intValue__I();
  };
  static numberLongValue(instance) {
    if (typeof instance === "number")
      return $c_sjsr_RuntimeLong$.__M().fromDouble__D__sjsr_RuntimeLong(instance);
    else
      return instance.longValue__J();
  };
  static numberFloatValue(instance) {
    if (typeof instance === "number") return $fround(instance);
    else                              return instance.floatValue__F();
  };
  static numberDoubleValue(instance) {
    if (typeof instance === "number") return instance;
    else                              return instance.doubleValue__D();
  };

  static isNaN(instance) {
    return instance !== instance;
  };

  static isInfinite(instance) {
    return !$g["isFinite"](instance) && !$.isNaN(instance);
  };

  static doubleToInt(x) {
    return (x > 2147483647) ? (2147483647) : ((x < -2147483648) ? -2147483648 : (x | 0));
  };

  static systemArraycopy(src, srcPos, dest, destPos, length) {
    const srcu = src.u;
    const destu = dest.u;
    if (srcu !== destu || destPos < srcPos || srcPos + length < destPos) {
      for (let i = 0; i < length; i++)
        destu[destPos+i] = srcu[srcPos+i];
    } else {
      for (let i = length-1; i >= 0; i--)
        destu[destPos+i] = srcu[srcPos+i];
    }
  };

  static systemIdentityHashCode(obj) {
    switch (typeof obj) {
      case "string": case "number": case "boolean": case "undefined":
        return $.objectHashCode(obj);
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
  };

  // TypeArray conversions

  static byteArray2TypedArray(value) { return new $g["Int8Array"](value.u); };
  static shortArray2TypedArray(value) { return new $g["Int16Array"](value.u); };
  static charArray2TypedArray(value) { return new $g["Uint16Array"](value.u); };
  static intArray2TypedArray(value) { return new $g["Int32Array"](value.u); };
  static floatArray2TypedArray(value) { return new $g["Float32Array"](value.u); };
  static doubleArray2TypedArray(value) { return new $g["Float64Array"](value.u); };

  static typedArray2ByteArray(value) {
    return new ($d_B.getArrayOf()).constr(new $g["Int8Array"](value));
  };
  static typedArray2ShortArray(value) {
    return new ($d_S.getArrayOf()).constr(new $g["Int16Array"](value));
  };
  static typedArray2CharArray(value) {
    return new ($d_C.getArrayOf()).constr(new $g["Uint16Array"](value));
  };
  static typedArray2IntArray(value) {
    return new ($d_I.getArrayOf()).constr(new $g["Int32Array"](value));
  };
  static typedArray2FloatArray(value) {
    return new ($d_F.getArrayOf()).constr(new $g["Float32Array"](value));
  };
  static typedArray2DoubleArray(value) {
    return new ($d_D.getArrayOf()).constr(new $g["Float64Array"](value));
  };

} // class $

// Scala.js classes

///INSERT CLASSES HERE///

// TypeData class

class $TypeData {

  constructor() {
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

  initPrim(zero, arrayEncodedName, displayName) {
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

  initClass(internalNameObj, isInterface, fullName,
      ancestors, isRawJSType, parentData, isInstance, isArrayOf) { // TODO Default params
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

  initArray(componentData) {
    // The constructor

    const componentZero0 = componentData.zero;

    // The zero for the Long runtime representation
    // is a special case here, since the class has not
    // been defined yet, when this file is read
    const componentZero = (componentZero0 === "longZero")
      ? $longZero
      : componentZero0;

    // TODO Declare only one ArrayClass at the top-level?
    const self = this;
    class ArrayClass extends $c_O {
      constructor(arg) {
        super();
        let u;
        if (typeof arg === 'number') {
          if (arg < 0)
            throw new $g["RangeError"]("invalid array length");
          u = [];
          for (let i = 0; i < arg; i++)
            u[i] = componentZero;
        } else {
          u = arg;
        }
        this.$classData = self;
        this.u = u;
      };

      clone__O() {
        if (this.u instanceof $g["Array"])
          return new ArrayClass(this.u["slice"](0));
        else
          // The underlying Array is a TypedArray
          return new ArrayClass(new this.u.constructor(this.u));
      };
    };

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
    this.ancestors = {O: 1};
    this.componentData = componentData;
    this.arrayBase = componentBase;
    this.arrayDepth = arrayDepth;
    this.zero = null;
    this.arrayEncodedName = encodedName;
    this._classOf = void 0;
    this._arrayOf = void 0;
    this.isArrayOf = void 0;

    // java.lang.Class support
    this["name"] = encodedName;
    this["isPrimitive"] = false;
    this["isInterface"] = false;
    this["isArrayClass"] = true;
    this["isInstance"] = isInstance;

    return this;
  };

  getClassOf() {
    if (!this._classOf)
      this._classOf = new $c_jl_Class().init___jl_ScalaJSClassData(this);
    return this._classOf;
  };

  getArrayOf() {
    if (!this._arrayOf)
      this._arrayOf = new $TypeData().initArray(this);
    return this._arrayOf;
  };

  // java.lang.Class support

  "getFakeInstance"() {
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
      return $longZero;
    else if (this === $d_sr_BoxedUnit)
      return void 0;
    else
      return {$classData: this};
  };

  "getSuperclass"() {
    return this.parentData ? this.parentData.getClassOf() : null;
  };

  "getComponentType"() {
    return this.componentData ? this.componentData.getClassOf() : null;
  };

  "newArrayOfThisClass"(lengths) {
    let arrayClassData = this;
    for (let i = 0; i < lengths.length; i++)
      arrayClassData = arrayClassData.getArrayOf();
    return $.newArrayObject(arrayClassData, lengths);
  };

} // class $TypeData

// Create primitive types

$d_V = new $TypeData().initPrim(void 0, "V", "void", function() { return false; });
$d_Z = new $TypeData().initPrim(false, "Z", "boolean", $isArrayOf_Z);
$d_C = new $TypeData().initPrim(0, "C", "char", $isArrayOf_C);
$d_B = new $TypeData().initPrim(0, "B", "byte", $isArrayOf_B);
$d_S = new $TypeData().initPrim(0, "S", "short", $isArrayOf_S);
$d_I = new $TypeData().initPrim(0, "I", "int", $isArrayOf_I);
$d_J = new $TypeData().initPrim("longZero", "J", "long", $isArrayOf_J);
$d_F = new $TypeData().initPrim(0.0, "F", "float", $isArrayOf_F);
$d_D = new $TypeData().initPrim(0.0, "D", "double", $isArrayOf_D);

///INSERT CREATE TYPE DATA HERE///

// Exports

function $export(nsParts, name, thing) {
  let ns = $e;
  for (let i = 0; i < nsParts["length"]; i = (i+1) | 0) {
    const part = nsParts[i];
    const nextNS = $jsSelect(ns, part) || new $g["Object"]();
    $jsAssign(ns, part, nextNS);
    ns = nextNS;
  }
  $jsAssign(ns, name, thing);
}

function $exportCtor(nsParts, name, ctor, proto) {
  const f = $weakFun(ctor);
  f.prototype = proto;
  $export(nsParts, name, f);
}

///INSERT EXPORTS HERE///

// Final reassignments

//!if asInstanceOfs != Unchecked
$throwClassCastException = $.throwClassCastException;
$throwArrayCastException = $.throwArrayCastException;
//!endif
$longZero = $c_sjsr_RuntimeLong$.__M().Zero$1;
