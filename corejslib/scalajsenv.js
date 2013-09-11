/* ---------------------------------- *
 * The top-level Scala.js environment *
 * ---------------------------------- */

var ScalaJS = {
  // Fields
  g: this,             // Global scope
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
    if (rhsData === ScalaJS.data.java\ufe33lang\ufe33String)
      return ScalaJS.dynamicIsInstanceOf("some string", lhsData);
    else
      return ScalaJS.dynamicIsInstanceOf({$classData: rhsData}, lhsData);
  },

  throwClassCastException: function(instance, classFullName) {
    throw new ScalaJS.c.java\ufe33lang\ufe33ClassCastException()
      .init\ufe33\ufe34T(
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
      return new ScalaJS.c.scala\ufe33js\ufe33JavaScriptException()
        .init\ufe33\ufe34Lscala\ufe33js\ufe33Any(exception);
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
      return ScalaJS.modules.scala\ufe33runtime\ufe33BoxesRunTime()
        .equals\ufe34O\ufe34O\ufe34Z(lhs, rhs);
    } else {
      return lhs === rhs;
    }
  },

  anyRefEqEq: function(lhs, rhs) {
    if (ScalaJS.isScalaJSObject(lhs))
      return lhs.equals\ufe34O\ufe34Z(rhs);
    else
      return lhs === rhs;
  },

  objectGetClass: function(instance) {
    if (ScalaJS.isScalaJSObject(instance) || (instance === null))
      return instance.getClass\ufe34java\ufe33lang\ufe33Class();
    else if (typeof(instance) === "string")
      return ScalaJS.data.java\ufe33lang\ufe33String.getClassOf();
    else
      return null; // Exception?
  },

  objectClone: function(instance) {
    // TODO
    throw new ScalaJS.c.scala\ufe33NotImplementedError().init\ufe33\ufe34();
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
      return instance.equals\ufe34O\ufe34Z(rhs);
    else
      return instance === rhs;
  },

  objectHashCode: function(instance) {
    if (ScalaJS.isScalaJSObject(instance))
      return instance.hashCode\ufe34I();
    else
      return 42; // TODO
  },

  comparableCompareTo: function(instance, rhs) {
    if (typeof(instance) === "string") {
      ScalaJS.as.java\ufe33lang\ufe33String(rhs);
      return instance === rhs ? 0 : (instance < rhs ? -1 : 1);
    } else {
      return instance.compareTo\ufe34O\ufe34I(rhs);
    }
  },

  charSequenceLength: function(instance) {
    if (typeof(instance) === "string")
      return instance["length"];
    else
      return instance.length\ufe34I();
  },

  charSequenceCharAt: function(instance, index) {
    if (typeof(instance) === "string")
      return instance["charCodeAt"](index);
    else
      return instance.charAt\ufe34I\ufe34C(index);
  },

  charSequenceSubSequence: function(instance, start, end) {
    if (typeof(instance) === "string")
      return instance["substring"](start, end);
    else
      return instance.subSequence\ufe34I\ufe34I\ufe34java\ufe33lang\ufe33CharSequence(start, end);
  },

  truncateToLong: function(value) {
    return value < 0 ? Math.ceil(value) : Math.floor(value);
  },

  propertiesOf: function(obj) {
    var result = new Array();
    for (var prop in obj)
      result["push"](prop.toString());
    return result;
  },

  // Boxes - inline all the way through java.lang.X.valueOf()

  bV: function() {
    return ScalaJS.modules.scala\ufe33runtime\ufe33BoxedUnit().UNIT$1;
  },
  bZ: function(value) {
    if (value)
      return ScalaJS.modules.java\ufe33lang\ufe33Boolean().TRUE$1;
    else
      return ScalaJS.modules.java\ufe33lang\ufe33Boolean().FALSE$1;
  },
  bC: function(value) {
    return new ScalaJS.c.java\ufe33lang\ufe33Character().init\ufe33\ufe34C(value);
  },
  bB: function(value) {
    return new ScalaJS.c.java\ufe33lang\ufe33Byte().init\ufe33\ufe34B(value);
  },
  bS: function(value) {
    return new ScalaJS.c.java\ufe33lang\ufe33Short().init\ufe33\ufe34S(value);
  },
  bI: function(value) {
    return new ScalaJS.c.java\ufe33lang\ufe33Integer().init\ufe33\ufe34I(value);
  },
  bJ: function(value) {
    return new ScalaJS.c.java\ufe33lang\ufe33Long().init\ufe33\ufe34J(value);
  },
  bF: function(value) {
    return new ScalaJS.c.java\ufe33lang\ufe33Float().init\ufe33\ufe34F(value);
  },
  bD: function(value) {
    return new ScalaJS.c.java\ufe33lang\ufe33Double().init\ufe33\ufe34D(value);
  },

  // Unboxes - inline all the way through obj.xValue()

  uV: function(value) {
    return undefined;
  },
  uZ: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Boolean(value).value$1;
  },
  uC: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Character(value).value$1;
  },
  uB: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Byte(value).value$1;
  },
  uS: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Short(value).value$2;
  },
  uI: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Integer(value).value$2;
  },
  uJ: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Long(value).value$2;
  },
  uF: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Float(value).value$2;
  },
  uD: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Double(value).value$2;
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
  /** @constructor */
  var ArrayClass = function(arg) {
    ScalaJS.c.java\ufe33lang\ufe33Object.call(this);
    ScalaJS.c.java\ufe33lang\ufe33Object.prototype.init\ufe33\ufe34.call(this);

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
  ArrayClass.prototype = new ScalaJS.inheritable.java\ufe33lang\ufe33Object;
  ArrayClass.prototype.constructor = ArrayClass;
  ArrayClass.prototype.$classData = this;

  // The data

  var encodedName = "[" + componentData.arrayEncodedName;
  var componentBase = componentData.arrayBase || componentData;
  var componentDepth = componentData.arrayDepth || 0;
  var arrayDepth = componentDepth + 1;

  var isInstance = function(obj) {
    return componentBase.isArrayOf(obj, arrayDepth);
  }

  this.constr = ArrayClass;
  this.parentData = ScalaJS.data.java\ufe33lang\ufe33Object;
  this.ancestors = {java\ufe33lang\ufe33Object: true};
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
      new ScalaJS.c.java\ufe33lang\ufe33Class()
        .init\uFE33\uFE34Lscala\uFE33js\uFE33Dynamic(this);
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

ScalaJS.data.scala\ufe33Unit    = new ScalaJS.PrimitiveTypeData(undefined, "V", "void");
ScalaJS.data.scala\ufe33Boolean = new ScalaJS.PrimitiveTypeData(false, "Z", "boolean");
ScalaJS.data.scala\ufe33Char    = new ScalaJS.PrimitiveTypeData(0, "C", "char");
ScalaJS.data.scala\ufe33Byte    = new ScalaJS.PrimitiveTypeData(0, "B", "byte");
ScalaJS.data.scala\ufe33Short   = new ScalaJS.PrimitiveTypeData(0, "S", "short");
ScalaJS.data.scala\ufe33Int     = new ScalaJS.PrimitiveTypeData(0, "I", "int");
ScalaJS.data.scala\ufe33Long    = new ScalaJS.PrimitiveTypeData(0, "J", "long");
ScalaJS.data.scala\ufe33Float   = new ScalaJS.PrimitiveTypeData(0.0, "F", "float");
ScalaJS.data.scala\ufe33Double  = new ScalaJS.PrimitiveTypeData(0.0, "D", "double");

// Instance tests for array of primitives

ScalaJS.isArrayOf.scala\ufe33Boolean = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Boolean);
ScalaJS.asArrayOf.scala\ufe33Boolean = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Boolean, "Z");
ScalaJS.data.scala\ufe33Boolean.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Boolean;

ScalaJS.isArrayOf.scala\ufe33Char = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Char);
ScalaJS.asArrayOf.scala\ufe33Char = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Char, "C");
ScalaJS.data.scala\ufe33Char.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Char;

ScalaJS.isArrayOf.scala\ufe33Byte = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Byte);
ScalaJS.asArrayOf.scala\ufe33Byte = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Byte, "B");
ScalaJS.data.scala\ufe33Byte.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Byte;

ScalaJS.isArrayOf.scala\ufe33Short = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Short);
ScalaJS.asArrayOf.scala\ufe33Short = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Short, "S");
ScalaJS.data.scala\ufe33Short.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Short;

ScalaJS.isArrayOf.scala\ufe33Int = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Int);
ScalaJS.asArrayOf.scala\ufe33Int = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Int, "I");
ScalaJS.data.scala\ufe33Int.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Int;

ScalaJS.isArrayOf.scala\ufe33Long = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Long);
ScalaJS.asArrayOf.scala\ufe33Long = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Long, "J");
ScalaJS.data.scala\ufe33Long.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Long;

ScalaJS.isArrayOf.scala\ufe33Float = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Float);
ScalaJS.asArrayOf.scala\ufe33Float = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Float, "F");
ScalaJS.data.scala\ufe33Float.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Float;

ScalaJS.isArrayOf.scala\ufe33Double = ScalaJS.makeIsArrayOfPrimitive(ScalaJS.data.scala\ufe33Double);
ScalaJS.asArrayOf.scala\ufe33Double = ScalaJS.makeAsArrayOfPrimitive(ScalaJS.isArrayOf.scala\ufe33Double, "D");
ScalaJS.data.scala\ufe33Double.isArrayOf = ScalaJS.isArrayOf.scala\ufe33Double;
