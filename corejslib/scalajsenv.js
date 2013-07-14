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
  moduleInstances: {}, // Module instances
  modules: {},         // Module accessors
  is: {},              // isInstanceOf methods
  as: {},              // asInstanceOf methods
  isArrayOf: {},       // isInstanceOfArrayOf methods
  asArrayOf: {},       // asInstanceOfArrayOf methods
  natives: {},         // Native methods registry

  // Core mechanism

  createPrimitiveTypeData: function(zero, arrayEncodedName, displayName) {
    return {
      constr: undefined,
      jsconstr: undefined,
      parentData: null,
      ancestors: {},
      isPrimitive: true,
      isInterface: false,
      isArrayClass: false,
      componentData: null,
      zero: zero,
      arrayEncodedName: arrayEncodedName,
      displayName: displayName,
      _classOf: undefined,
      getClassOf: ScalaJS.classOfGetter,
      _arrayOf: undefined,
      getArrayOf: ScalaJS.arrayOfGetter,
      isInstance: function(obj) { return false; },
      isArrayOf: function(obj, depth) { return false; }
    };
  },

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

  classOfGetter: function() {
    if (!this._classOf)
      this._classOf = ScalaJS.createClassInstance(this);
    return this._classOf;
  },

  arrayOfGetter: function() {
    if (!this._arrayOf)
      this._arrayOf = ScalaJS.createArrayClassData(this);
    return this._arrayOf;
  },

  createArrayClassData: function(componentData) {
    var encodedName = "[" + componentData.arrayEncodedName;

    // The constructor

    var zero = componentData.zero;
    /** @constructor */
    function ArrayClass(arg) {
      ScalaJS.c.java\ufe33lang\ufe33Object.call(this);
      ScalaJS.c.java\ufe33lang\ufe33Object.prototype.init\ufe33\ufe34.call(this);

      if (typeof(arg) === "number") {
        // arg is the length of the array
        this.underlying = new Array(arg);
        for (var i = 0; i < arg; i++)
          this.underlying[i] = zero;
      } else {
        // arg is a native array that we wrap
        this.underlying = arg;
      }
    }
    ArrayClass.prototype = new ScalaJS.inheritable.java\ufe33lang\ufe33Object;
    ArrayClass.prototype.constructor = ArrayClass;

    // The data

    var componentBase = componentData.arrayBase || componentData;
    var componentDepth = componentData.arrayDepth || 0;
    var arrayDepth = componentDepth + 1;

    //var compAncestors = componentData.ancestors;
    var ancestors = {java\ufe33lang\ufe33Object: true};
    //for (var compAncestor in compAncestors)
    //  ancestors[compAncestor+"[]"] = true;
    
    var isInstance = function(obj) {
      return componentBase.isArrayOf(obj, arrayDepth);
    }

    var data = {
      constr: ArrayClass,
      jsconstr: ArrayClass,
      parentData: ScalaJS.data.java\ufe33lang\ufe33Object,
      ancestors: ancestors,
      isPrimitive: true,
      isInterface: false,
      isArrayClass: true,
      componentData: componentData,
      arrayBase: componentBase,
      arrayDepth: arrayDepth,
      zero: zero,
      arrayEncodedName: encodedName,
      displayName: encodedName,
      _classOf: undefined,
      getClassOf: ScalaJS.classOfGetter,
      _arrayOf: undefined,
      getArrayOf: ScalaJS.arrayOfGetter,
      isInstance: isInstance,
      isArrayOf: undefined
    };

    ArrayClass.prototype.$classData = data;
    return data;
  },

  createClassInstance: function(data) {
    // <init>(scala.js.Dynamic)
    return new ScalaJS.c.java\ufe33lang\ufe33Class()
      .init\uFE33\uFE34Lscala\uFE33js\uFE33Dynamic(data);
  },

  registerNative: function(fullName, nativeFunction) {
    ScalaJS.natives[fullName] = nativeFunction;
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
      return ScalaJS.dynamicIsInstanceOf(lhsData, "some string");
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
      for (var i = 0; i < result.length(); i++) {
        result.set(i, ScalaJS.newArrayObjectInternal(
          subArrayClassData, lengths, subLengthIndex));
      }
    }

    return result;
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
      return instance.equals\ufe34O\ufe34Z();
    else
      return instance === rhs;
  },

  objectHashCode: function(instance) {
    if (ScalaJS.isScalaJSObject(instance))
      return instance.hashCode\ufe34I();
    else
      return 42; // TODO
  },

  truncateToLong: function(value) {
    return value < 0 ? Math.ceil(value) : Math.floor(value);
  },

  // Boxes - inline all the way through java.lang.X.valueOf()

  bV: function() {
    return ScalaJS.modules.scala\ufe33runtime\ufe33BoxedUnit().$jsfield$UNIT;
  },
  bZ: function(value) {
    if (value)
      return ScalaJS.modules.java\ufe33lang\ufe33Boolean().$jsfield$TRUE;
    else
      return ScalaJS.modules.java\ufe33lang\ufe33Boolean().$jsfield$FALSE;
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
    return ScalaJS.as.java\ufe33lang\ufe33Boolean(value).$jsfield$value;
  },
  uC: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Character(value).$jsfield$value;
  },
  uB: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Byte(value).$jsfield$value;
  },
  uS: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Short(value).$jsfield$value;
  },
  uI: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Integer(value).$jsfield$value;
  },
  uJ: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Long(value).$jsfield$value;
  },
  uF: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Float(value).$jsfield$value;
  },
  uD: function(value) {
    return ScalaJS.as.java\ufe33lang\ufe33Double(value).$jsfield$value;
  }
}

function asInstanceOfSomeArray(obj) {
  return obj;
}

// Create primitive types

ScalaJS.data.scala\ufe33Unit    = ScalaJS.createPrimitiveTypeData(undefined, "V", "void");
ScalaJS.data.scala\ufe33Boolean = ScalaJS.createPrimitiveTypeData(false, "Z", "boolean");
ScalaJS.data.scala\ufe33Char    = ScalaJS.createPrimitiveTypeData(0, "C", "char");
ScalaJS.data.scala\ufe33Byte    = ScalaJS.createPrimitiveTypeData(0, "B", "byte");
ScalaJS.data.scala\ufe33Short   = ScalaJS.createPrimitiveTypeData(0, "S", "short");
ScalaJS.data.scala\ufe33Int     = ScalaJS.createPrimitiveTypeData(0, "I", "int");
ScalaJS.data.scala\ufe33Long    = ScalaJS.createPrimitiveTypeData(0, "J", "long");
ScalaJS.data.scala\ufe33Float   = ScalaJS.createPrimitiveTypeData(0.0, "F", "float");
ScalaJS.data.scala\ufe33Double  = ScalaJS.createPrimitiveTypeData(0.0, "D", "double");

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
