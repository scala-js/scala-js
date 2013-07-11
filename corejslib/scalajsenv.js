/* ---------------------------------- *
 * The top-level Scala.js environment *
 * ---------------------------------- */

var ScalaJS = {
  // Fields
  global: this,
  primitives: {},
  classes: {},
  modules: {},
  natives: {},

  // Short fields used a lot by the codegen
  g: this, // Global scope
  c: {},   // Constructors
  m: {},   // Module instances

  // Core mechanism

  defineLazyField: function(obj, propName, computeFun) {
    Object.defineProperty(obj, propName, {
      __proto__: null,
      enumerable: true,
      configurable: true,
      get: function() {
        var value = computeFun.call(obj);
        Object.defineProperty(obj, propName, {
          __proto__: null,
          enumerable: true,
          configurable: false,
          writable: false,
          value: value
        });
        return value;
      }
    });
  },

  createType: function(name, constructor, jsconstructor,
                       parent, ancestors, isPrimitive,
                       isInterface, isArray, componentData, zero,
                       arrayEncodedName, displayName) {
    var data = {
      name: name,
      constructor: constructor,
      jsconstructor: jsconstructor,
      parent: parent,
      parentData: parent === null ? null : ScalaJS.classes[parent],
      ancestors: ancestors,
      isPrimitive: isPrimitive,
      isInterface: isInterface,
      isArray: isArray,
      componentData: componentData,
      zero: zero,
      arrayEncodedName: arrayEncodedName,
      displayName: displayName,
      _class: undefined,
      get cls() {
        if (this._class === undefined)
          this._class = ScalaJS.createClassInstance(this);
        return this._class;
      },
      _array: undefined,
      get array() {
        if (this._array === undefined)
          this._array = ScalaJS.createArrayClass(this);
        return this._array;
      }
    };

    if (constructor !== undefined)
      constructor.prototype.$classData = data;

    if (!isPrimitive && !isArray) {
      Object.defineProperty(ScalaJS.classes, name, {
        __proto__: null,
        enumerable: true,
        configurable: false,
        writable: false,
        value: data
      });
    } else if (isPrimitive) {
      Object.defineProperty(ScalaJS.primitives, name, {
        __proto__: null,
        enumerable: true,
        configurable: false,
        writable: false,
        value: data
      });
    }

    return data;
  },

  createClass: function(name, constructor, jsconstructor,
                        parent, ancestors) {
    return ScalaJS.createType(name, constructor, jsconstructor,
                              parent, ancestors,
                              false, false, false, null, null,
                              "L" + name + ";", name);
  },

  createPrimitiveType: function(name, zero, arrayEncodedName, displayName) {
    var ancestors = {};
    ancestors[name] = true;
    return ScalaJS.createType(name, undefined, undefined,
                              null, ancestors,
                              true, false, false, null, zero,
                              arrayEncodedName, displayName);
  },

  createArrayClass: function(componentData) {
    var name = componentData.name + "[]";
    var encodedName = "[" + componentData.arrayEncodedName;
    var constructor = ScalaJS.createArrayTypeFunction(name, componentData);

    var compAncestors = componentData.ancestors;
    var ancestors = {"java.lang.Object": true};
    for (var compAncestor in compAncestors)
      ancestors[compAncestor+"[]"] = true;

    return ScalaJS.createType(name, constructor, constructor,
                              "java.lang.Object", ancestors,
                              false, false, true, componentData, null,
                              encodedName, encodedName);
  },

  createInterface: function(name, ancestors) {
    return ScalaJS.createType(name, undefined, undefined,
                              null, ancestors,
                              false, true, false, null, null,
                              "L" + name + ";", name);
  },

  registerClass: function(name, propNameObj, createFunction) {
    var propName = ScalaJS.propertyName(propNameObj);
    Object.defineProperty(ScalaJS.classes, name, {
      __proto__: null,
      enumerable: true,
      configurable: true,
      get: function() {
        createFunction();             // hopefully this calls createClass(name) ...
        return ScalaJS.classes[name]; // ... otherwise this will recurse infinitely
      }
    });

    ScalaJS.defineLazyField(ScalaJS.c, propName, function() {
      return ScalaJS.classes[name].constructor;
    });

    if (name !== propName) {
      ScalaJS.defineLazyField(ScalaJS.c, name, function() {
        return ScalaJS.classes[name].constructor;
      });
    }
  },

  registerModule: function(name, propNameObj, className) {
    var propName = ScalaJS.propertyName(propNameObj);
    var data = {
      _instance: undefined,
      get instance() {
        if (this._instance === undefined)
          this._instance = new ScalaJS.c[className]().init\ufe33\ufe34();
        return this._instance;
      }
    };
    ScalaJS.modules[name] = data;

    ScalaJS.defineLazyField(ScalaJS.m, propName, function() {
      return ScalaJS.modules[name].instance;
    });

    if (name !== propName) {
      ScalaJS.defineLazyField(ScalaJS.m, name, function() {
        return ScalaJS.modules[name].instance;
      });
    }
  },

  createClassInstance: function(data) {
    // <init>(scala.js.Dynamic, scala.js.Dynamic)
    return new ScalaJS.c["java.lang.Class"]()
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

  // Array type factory

  createArrayTypeFunction: function(name, componentData) {
    var ObjectClass = ScalaJS.c["java.lang.Object"];
    var zero = componentData.zero;

    function ArrayClass(arg) {
      ObjectClass.call(this);
      ObjectClass.prototype.init\ufe33\ufe34.call(this);

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
    ArrayClass.prototype = Object.create(ObjectClass.prototype);
    ArrayClass.prototype.constructor = ArrayClass;

    return ArrayClass;
  },

  // Runtime functions

  isScalaJSObject: function(instance) {
    return (typeof(instance) === "object") && (instance !== null) &&
      !!instance.$classData;
  },

  StringAncestors: {
    "java.lang.String": true,
    "java.io.Serializable": true,
    "java.lang.CharSequence": true,
    "java.lang.Comparable": true,
    "java.lang.Object": true
  },

  isInstance: function(instance, classFullName) {
    if (ScalaJS.isScalaJSObject(instance)) {
      return !!instance.$classData.ancestors[classFullName];
    } else if (typeof(instance) === "string") {
      return !!ScalaJS.StringAncestors[classFullName];
    } else {
      return false;
    }
  },

  asInstance: function(instance, classFullName) {
    if ((instance === null) || ScalaJS.isInstance(instance, classFullName))
      return instance;
    else
      ScalaJS.throwClassCastException(instance, classFullName);
  },

  asInstanceString: function(instance) {
    if ((instance === null) || (typeof(instance) === "string"))
      return instance;
    else
      ScalaJS.throwClassCastException(instance, "java.lang.String");
  },

  throwClassCastException: function(instance, classFullName) {
    throw new ScalaJS.c["java.lang.ClassCastException"]().init\ufe33\ufe34T(
      instance + " is not an instance of " + classFullName);
  },

  makeNativeArrayWrapper: function(arrayClassData, nativeArray) {
    return new arrayClassData.constructor(nativeArray);
  },

  newArrayObject: function(arrayClassData, lengths) {
    return ScalaJS.newArrayObjectInternal(arrayClassData, lengths, 0);
  },

  newArrayObjectInternal: function(arrayClassData, lengths, lengthIndex) {
    var result = new arrayClassData.constructor(lengths[lengthIndex]);

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
      return ScalaJS.m["scala.runtime.BoxesRunTime"].equals\ufe34O\ufe34O\ufe34Z(lhs, rhs);
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
      return ScalaJS.classes["java.lang.String"].cls;
    else
      return null; // Exception?
  },

  objectClone: function(instance) {
    // TODO
    throw new ScalaJS.c["scala.NotImplementedError"]().init\ufe33\ufe34();
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
    return ScalaJS.m["scala.runtime.BoxedUnit"].$jsfield$UNIT;
  },
  bZ: function(value) {
    if (value)
      return ScalaJS.m["java.lang.Boolean"].$jsfield$TRUE;
    else
      return ScalaJS.m["java.lang.Boolean"].$jsfield$FALSE;
  },
  bC: function(value) {
    return new ScalaJS.c["java.lang.Character"]().init\ufe33\ufe34C(value);
  },
  bB: function(value) {
    return new ScalaJS.c["java.lang.Byte"]().init\ufe33\ufe34B(value);
  },
  bS: function(value) {
    return new ScalaJS.c["java.lang.Short"]().init\ufe33\ufe34S(value);
  },
  bI: function(value) {
    return new ScalaJS.c["java.lang.Integer"]().init\ufe33\ufe34I(value);
  },
  bJ: function(value) {
    return new ScalaJS.c["java.lang.Long"]().init\ufe33\ufe34J(value);
  },
  bF: function(value) {
    return new ScalaJS.c["java.lang.Float"]().init\ufe33\ufe34F(value);
  },
  bD: function(value) {
    return new ScalaJS.c["java.lang.Double"]().init\ufe33\ufe34D(value);
  },

  // Unboxes - inline all the way through obj.xValue()

  uV: function(value) {
    return undefined;
  },
  uZ: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Boolean").$jsfield$value;
  },
  uC: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Character").$jsfield$value;
  },
  uB: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Byte").$jsfield$value;
  },
  uS: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Short").$jsfield$value;
  },
  uI: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Integer").$jsfield$value;
  },
  uJ: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Long").$jsfield$value;
  },
  uF: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Float").$jsfield$value;
  },
  uD: function(value) {
    return ScalaJS.asInstance(value, "java.lang.Double").$jsfield$value;
  }
}

// Create primitive types

ScalaJS.createPrimitiveType("scala.Unit", undefined, "V", "void");
ScalaJS.createPrimitiveType("scala.Boolean", false, "Z", "boolean");
ScalaJS.createPrimitiveType("scala.Char", 0, "C", "char");
ScalaJS.createPrimitiveType("scala.Byte", 0, "B", "byte");
ScalaJS.createPrimitiveType("scala.Short", 0, "S", "short");
ScalaJS.createPrimitiveType("scala.Int", 0, "I", "int");
ScalaJS.createPrimitiveType("scala.Long", 0, "J", "long");
ScalaJS.createPrimitiveType("scala.Float", 0.0, "F", "float");
ScalaJS.createPrimitiveType("scala.Double", 0.0, "D", "double");

// Create dummy class for java.lang.String

ScalaJS.registerClass("java.lang.String", {java\ufe33lang\ufe33String:0}, function() {
  function StringClass() {
    throw "The pseudo StringClass constructor should never be called"
  }

  ScalaJS.createClass("java.lang.String", StringClass, undefined, "java.lang.Object", {
    "java.lang.Object": true,
    "java.lang.String": true
  });
});
