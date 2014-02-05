/* Scala.js runtime support
 * Copyright 2013 LAMP/EPFL
 * Author: Sébastien Doeraene
 */

/* ---------------------------------- *
 * The top-level Scala.js environment *
 * ---------------------------------- */

var ScalaJS = {
  // Fields
  g: (typeof global === "object" && global && global["Object"] === Object) ? global : this, // Global scope
  data: {},            // Data for types
  c: {},               // Scala.js constructors
  inheritable: {},     // Inheritable constructors (without initialization code)
  classes: {},         // JavaScript constructors
  impls: {},           // Implementation class modules
  moduleInstances: {}, // Module instances
  modules: {},         // Module accessors
  is: {},              // isInstanceOf methods
  as: {},              // asInstanceOf methods
  isArrayOf: {},       // isInstanceOfArrayOf methods
  asArrayOf: {},       // asInstanceOfArrayOf methods

  // Core mechanism

  makeIsArrayOfPrimitive: function(primitiveData) {
    return function(obj, depth) {
      return !!(obj && obj.$classData &&
        (obj.$classData.arrayDepth === depth) &&
        (obj.$classData.arrayBase === primitiveData));
    }
  },

  makeAsArrayOfPrimitive: function(isInstanceOfFunction, arrayEncodedName) {
    return function(obj, depth) {
      if (isInstanceOfFunction(obj, depth) || (obj === null))
        return obj;
      else
        ScalaJS.throwArrayCastException(obj, arrayEncodedName, depth);
    }
  },

  /** Encode a property name for runtime manipulation
   *  Usage:
   *    env.propertyName({someProp:0})
   *  Returns:
   *    "someProp"
   *  Useful when the property is renamed by a global optimizer (like Closure)
   *  but we must still get hold of a string of that name for runtime
   * reflection.
   */
  propertyName: function(obj) {
    var result;
    for (var prop in obj)
      result = prop;
    return result;
  },

  // Runtime functions

  isScalaJSObject: function(obj) {
    return !!(obj && obj.$classData);
  },

  dynamicIsInstanceOf: function(obj, data) {
    return data.isInstance(obj);
  },

  dynamicIsAssignableFrom: function(lhsData, rhsData) {
    if (lhsData.isPrimitive || rhsData.isPrimitive)
      return lhsData === rhsData;
    if (rhsData === ScalaJS.data.java_lang_String)
      return ScalaJS.dynamicIsInstanceOf("some string", lhsData);
    else
      return ScalaJS.dynamicIsInstanceOf({$classData: rhsData}, lhsData);
  },

  throwClassCastException: function(instance, classFullName) {
    throw new ScalaJS.c.java_lang_ClassCastException()
      .init___T(
        instance + " is not an instance of " + classFullName);
  },

  throwArrayCastException: function(instance, classArrayEncodedName, depth) {
    for (; depth; --depth)
      classArrayEncodedName = "[" + classArrayEncodedName;
    ScalaJS.throwClassCastException(instance, classArrayEncodedName);
  },

  wrapJavaScriptException: function(exception) {
    if (ScalaJS.isScalaJSObject(exception))
      return exception;
    else
      return new ScalaJS.c.scala_scalajs_js_JavaScriptException()
        .init___Lscala_scalajs_js_Any(exception);
  },

  makeNativeArrayWrapper: function(arrayClassData, nativeArray) {
    return new arrayClassData.constr(nativeArray);
  },

  newArrayObject: function(arrayClassData, lengths) {
    return ScalaJS.newArrayObjectInternal(arrayClassData, lengths, 0);
  },

  newArrayObjectInternal: function(arrayClassData, lengths, lengthIndex) {
    var result = new arrayClassData.constr(lengths[lengthIndex]);

    if (lengthIndex < lengths.length-1) {
      var subArrayClassData = arrayClassData.componentData;
      var subLengthIndex = lengthIndex+1;
      var underlying = result.underlying;
      for (var i = 0; i < underlying.length; i++) {
        underlying[i] = ScalaJS.newArrayObjectInternal(
          subArrayClassData, lengths, subLengthIndex);
      }
    }

    return result;
  },

  /** Protect the argument against `this` forgery (see genPrimitiveJSCall()) */
  protect: function(x) {
    return x;
  },

  applyMethodWithVarargs: function(instance, methodName, argArray) {
    // Note: cannot be inlined because `instance` would be evaluated twice
    return instance[methodName].apply(instance, argArray);
  },

  newInstanceWithVarargs: function(constructor, argArray) {
    // Not really "possible" in JavaScript, so we emulate what it would be
    function c() {};
    c.prototype = constructor.prototype;
    var instance = new c;
    var result = constructor.apply(instance, argArray);
    switch (typeof result) {
      case "undefined":
      case "number":
      case "boolean":
      case "string":
        return instance;
      default:
        if (result === null)
          return instance;
        else
          return result;
    }
  },

  anyEqEq: function(lhs, rhs) {
    if (ScalaJS.isScalaJSObject(lhs)) {
      return ScalaJS.modules.scala_runtime_BoxesRunTime()
        .equals__O__O__Z(lhs, rhs);
    } else {
      return lhs === rhs;
    }
  },

  anyRefEqEq: function(lhs, rhs) {
    if (ScalaJS.isScalaJSObject(lhs))
      return lhs.equals__O__Z(rhs);
    else
      return lhs === rhs;
  },

  objectGetClass: function(instance) {
    if (ScalaJS.isScalaJSObject(instance) || (instance === null))
      return instance.getClass__java_lang_Class();
    else if (typeof(instance) === "string")
      return ScalaJS.data.java_lang_String.getClassOf();
    else
      return null; // Exception?
  },

  objectClone: function(instance) {
    if (ScalaJS.isScalaJSObject(instance) || (instance === null))
      return instance.clone__O();
    else
      throw new ScalaJS.c.java_lang_CloneNotSupportedException().init___();
  },

  objectFinalize: function(instance) {
    // TODO?
  },

  objectNotify: function(instance) {
    // TODO?
  },

  objectNotifyAll: function(instance) {
    // TODO?
  },

  objectEquals: function(instance, rhs) {
    if (ScalaJS.isScalaJSObject(instance) || (instance === null))
      return instance.equals__O__Z(rhs);
    else
      return instance === rhs;
  },

  objectHashCode: function(instance) {
    if (ScalaJS.isScalaJSObject(instance))
      return instance.hashCode__I();
    else if (typeof(instance) === "string") {
      // calculate hash of String as specified by JavaDoc
      var n = instance.length;
      var res = 0;
      var mul = 1; // holds pow(31, n-i-1)
      // multiplications with `mul` do never overflow the 52 bits of precision:
      // - we truncate `mul` to 32 bits on each operation
      // - 31 has 5 significant bits only
      // - s[i] has 16 significant bits max
      // 32 + max(5, 16) = 48 < 52 => no overflow
      for (var i = n-1; i >= 0; --i) {
        // calculate s[i] * pow(31, n-i-1)
        res = res + (instance.charCodeAt(i) * mul | 0) | 0
        // update mul for next iteration
        mul = mul * 31 | 0
      }

      return res;
    } else {
      return 42; // TODO
    }
  },

  comparableCompareTo: function(instance, rhs) {
    if (typeof(instance) === "string") {
      ScalaJS.as.java_lang_String(rhs);
      return instance === rhs ? 0 : (instance < rhs ? -1 : 1);
    } else {
      return instance.compareTo__O__I(rhs);
    }
  },

  charSequenceLength: function(instance) {
    if (typeof(instance) === "string")
      return instance["length"];
    else
      return instance.length__I();
  },

  charSequenceCharAt: function(instance, index) {
    if (typeof(instance) === "string")
      return instance["charCodeAt"](index);
    else
      return instance.charAt__I__C(index);
  },

  charSequenceSubSequence: function(instance, start, end) {
    if (typeof(instance) === "string")
      return instance["substring"](start, end);
    else
      return instance.subSequence__I__I__Ljava_lang_CharSequence(start, end);
  },

  truncateToLong: function(value) {
    return value < 0 ? ScalaJS.g["Math"]["ceil"](value)
                     : ScalaJS.g["Math"]["floor"](value);
  },

  /** convert a number to a char (unsigned 16bit value) */
  num2char: function(value) {
    var x = value | 0;
    while (x > 65535)
      x -= 65536;
    while (x < 0)
      x += 65536;
    return x;
  },

  propertiesOf: function(obj) {
    var result = new Array();
    for (var prop in obj)
      result["push"](prop.toString());
    return result;
  },

  // Boxes - inline all the way through java.lang.X.valueOf()

  bV: function() {
    return ScalaJS.modules.scala_runtime_BoxedUnit().UNIT$1;
  },
  bZ: function(value) {
    if (value)
      return ScalaJS.modules.java_lang_Boolean().TRUE$1;
    else
      return ScalaJS.modules.java_lang_Boolean().FALSE$1;
  },
  bC: function(value) {
    return new ScalaJS.c.java_lang_Character().init___C(value);
  },
  bB: function(value) {
    return new ScalaJS.c.java_lang_Byte().init___B(value);
  },
  bS: function(value) {
    return new ScalaJS.c.java_lang_Short().init___S(value);
  },
  bI: function(value) {
    return new ScalaJS.c.java_lang_Integer().init___I(value);
  },
  bJ: function(value) {
    return new ScalaJS.c.java_lang_Long().init___J(value);
  },
  bF: function(value) {
    return new ScalaJS.c.java_lang_Float().init___F(value);
  },
  bD: function(value) {
    return new ScalaJS.c.java_lang_Double().init___D(value);
  },

  // Unboxes - inline all the way through obj.xValue()

  uV: function(value) {
    return undefined;
  },
  uZ: function(value) {
    return null === value ? false : ScalaJS.as.java_lang_Boolean(value).value$1;
  },
  uC: function(value) {
    return null === value ? 0 : ScalaJS.as.java_lang_Character(value).value$1;
  },
  uB: function(value) {
    return null === value ? 0 : ScalaJS.as.java_lang_Byte(value).value$2;
  },
  uS: function(value) {
    return null === value ? 0 : ScalaJS.as.java_lang_Short(value).value$2;
  },
  uI: function(value) {
    return null === value ? 0 : ScalaJS.as.java_lang_Integer(value).value$2;
  },
  uJ: function(value) {
    return null === value ? 0 : ScalaJS.as.java_lang_Long(value).value$2;
  },
  uF: function(value) {
    return null === value ? 0.0 : ScalaJS.as.java_lang_Float(value).value$2;
  },
  uD: function(value) {
    return null === value ? 0.0 : ScalaJS.as.java_lang_Double(value).value$2;
  }
}

// Type data constructors

/** @constructor */
ScalaJS.PrimitiveTypeData = function(zero, arrayEncodedName, displayName) {
  this.constr = undefined;
  this.parentData = undefined;
  this.ancestors = {};
  this.isPrimitive = true;
  this.isInterface = false;
  this.isArrayClass = false;
  this.componentData = null;
  this.zero = zero;
  this.arrayEncodedName = arrayEncodedName;
  this.displayName = displayName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isInstance = function(obj) { return false; };
  this.isArrayOf = function(obj, depth) { return false; };
},

/** @constructor */
ScalaJS.ClassTypeData = function(internalNameObj, isInterface, fullName,
                                 parentData, ancestors, isInstance, isArrayOf) {
  var internalName = ScalaJS.propertyName(internalNameObj);

  isInstance = isInstance || function(obj) {
    return !!(obj && obj.$classData && obj.$classData.ancestors[internalName]);
  };

  isArrayOf = isArrayOf || function(obj, depth) {
    return !!(obj && obj.$classData && (obj.$classData.arrayDepth === depth)
      && obj.$classData.arrayBase.ancestors[internalName])
  };

  this.constr = undefined;
  this.parentData = parentData;
  this.ancestors = ancestors;
  this.isPrimitive = false;
  this.isInterface = isInterface;
  this.isArrayClass = false;
  this.componentData = null;
  this.zero = null;
  this.arrayEncodedName = "L"+fullName+";";
  this.displayName = fullName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isInstance = isInstance;
  this.isArrayOf = isArrayOf;
};

/** @constructor */
ScalaJS.ArrayTypeData = function(componentData) {
  // The constructor

  var componentZero = componentData.zero;

  // The zero for the Long runtime representation
  // is a special case here, since the class has not
  // been defined yet, when this file is read
  if (componentZero == "longZero") {
    componentZero = ScalaJS.modules.scala_scalajs_runtime_Long().
      zero__Lscala_scalajs_runtime_Long();
  }

  /** @constructor */
  var ArrayClass = function(arg) {
    ScalaJS.c.java_lang_Object.call(this);
    ScalaJS.c.java_lang_Object.prototype.init___.call(this);

    if (typeof(arg) === "number") {
      // arg is the length of the array
      this.underlying = new Array(arg);
      for (var i = 0; i < arg; i++)
        this.underlying[i] = componentZero;
    } else {
      // arg is a native array that we wrap
      this.underlying = arg;
    }
  }
  ArrayClass.prototype = new ScalaJS.inheritable.java_lang_Object;
  ArrayClass.prototype.constructor = ArrayClass;
  ArrayClass.prototype.$classData = this;

  ArrayClass.prototype.clone__O = function() {
    return new ArrayClass(this.underlying["slice"](0));
  };

  // The data

  var encodedName = "[" + componentData.arrayEncodedName;
  var componentBase = componentData.arrayBase || componentData;
  var componentDepth = componentData.arrayDepth || 0;
  var arrayDepth = componentDepth + 1;

  var isInstance = function(obj) {
    return componentBase.isArrayOf(obj, arrayDepth);
  }

  this.constr = ArrayClass;
  this.parentData = ScalaJS.data.java_lang_Object;
  this.ancestors = {java_lang_Object: 1};
  this.isPrimitive = false;
  this.isInterface = false;
  this.isArrayClass = true;
  this.componentData = componentData;
  this.arrayBase = componentBase;
  this.arrayDepth = arrayDepth;
  this.zero = null;
  this.arrayEncodedName = encodedName;
  this.displayName = encodedName;
  this._classOf = undefined;
  this._arrayOf = undefined;
  this.isInstance = isInstance;
  this.isArrayOf = undefined;
};

ScalaJS.ClassTypeData.prototype.getClassOf = function() {
  if (!this._classOf)
    this._classOf =
      new ScalaJS.c.java_lang_Class()
        .init___Lscala_scalajs_js_Dynamic(this);
  return this._classOf;
};

ScalaJS.ClassTypeData.prototype.getArrayOf = function() {
  if (!this._arrayOf)
    this._arrayOf = new ScalaJS.ArrayTypeData(this);
  return this._arrayOf;
};

ScalaJS.PrimitiveTypeData.prototype = ScalaJS.ClassTypeData.prototype;
ScalaJS.ArrayTypeData.prototype = ScalaJS.ClassTypeData.prototype;

// Create primitive types

ScalaJS.data.scala_Unit    = new ScalaJS.PrimitiveTypeData(undefined, "V", "void");
ScalaJS.data.scala_Boolean = new ScalaJS.PrimitiveTypeData(false, "Z", "boolean");
ScalaJS.data.scala_Char    = new ScalaJS.PrimitiveTypeData(0, "C", "char");
ScalaJS.data.scala_Byte    = new ScalaJS.PrimitiveTypeData(0, "B", "byte");
ScalaJS.data.scala_Short   = new ScalaJS.PrimitiveTypeData(0, "S", "short");
ScalaJS.data.scala_Int     = new ScalaJS.PrimitiveTypeData(0, "I", "int");
ScalaJS.data.scala_Long    = new ScalaJS.PrimitiveTypeData("longZero", "J", "long");
ScalaJS.data.scala_Float   = new ScalaJS.PrimitiveTypeData(0.0, "F", "float");
ScalaJS.data.scala_Double  = new ScalaJS.PrimitiveTypeData(0.0, "D", "double");

// Instance tests for array of primitives

ScalaJS.isArrayOf.scala_Boolean = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Boolean);
ScalaJS.asArrayOf.scala_Boolean = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Boolean, "Z");
ScalaJS.data.scala_Boolean.isArrayOf = ScalaJS.isArrayOf.scala_Boolean;

ScalaJS.isArrayOf.scala_Char = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Char);
ScalaJS.asArrayOf.scala_Char = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Char, "C");
ScalaJS.data.scala_Char.isArrayOf = ScalaJS.isArrayOf.scala_Char;

ScalaJS.isArrayOf.scala_Byte = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Byte);
ScalaJS.asArrayOf.scala_Byte = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Byte, "B");
ScalaJS.data.scala_Byte.isArrayOf = ScalaJS.isArrayOf.scala_Byte;

ScalaJS.isArrayOf.scala_Short = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Short);
ScalaJS.asArrayOf.scala_Short = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Short, "S");
ScalaJS.data.scala_Short.isArrayOf = ScalaJS.isArrayOf.scala_Short;

ScalaJS.isArrayOf.scala_Int = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Int);
ScalaJS.asArrayOf.scala_Int = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Int, "I");
ScalaJS.data.scala_Int.isArrayOf = ScalaJS.isArrayOf.scala_Int;

ScalaJS.isArrayOf.scala_Long = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Long);
ScalaJS.asArrayOf.scala_Long = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Long, "J");
ScalaJS.data.scala_Long.isArrayOf = ScalaJS.isArrayOf.scala_Long;

ScalaJS.isArrayOf.scala_Float = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Float);
ScalaJS.asArrayOf.scala_Float = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Float, "F");
ScalaJS.data.scala_Float.isArrayOf = ScalaJS.isArrayOf.scala_Float;

ScalaJS.isArrayOf.scala_Double = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala_Double);
ScalaJS.asArrayOf.scala_Double = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala_Double, "D");
ScalaJS.data.scala_Double.isArrayOf = ScalaJS.isArrayOf.scala_Double;
