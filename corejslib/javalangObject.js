/* ------------------
 * java.lang.Object
 * ------------------ */

(function() {
  /** @constructor */
  ScalaJS.c.java\ufe33lang\ufe33Object = function() {
  };

  /** @constructor */
  ScalaJS.inheritable.java\ufe33lang\ufe33Object = function() {};
  ScalaJS.inheritable.java\ufe33lang\ufe33Object.prototype =
    ScalaJS.c.java\ufe33lang\ufe33Object.prototype;

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.init\ufe33\ufe34 = function() {
    return this;
  }

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.getClass\ufe34java\ufe33lang\ufe33Class = function() {
    return this.$classData.getClassOf();
  }

  // Bridge for getClass()
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.getClass = function() {
    return this.getClass\ufe34java\ufe33lang\ufe33Class();
  }

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.hashCode\ufe34I = function() {
    // TODO
    return 42;
  }

  // Bridge for hashCode()
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.hashCode = function() {
    return this.hashCode\ufe34I();
  }

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.equals\ufe34O\ufe34Z = function(rhs) {
    return this === rhs;
  }

  // Bridge for equals(Object)
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.equals = function(that) {
    return this.equals\ufe34O\ufe34Z(that);
  }

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.clone\ufe34O = function() {
    if (ScalaJS.is.java\ufe33lang\ufe33Cloneable(this)) {
      throw new ScalaJS.c.scala\ufe33NotImplementedError().init\ufe33\ufe34();
    } else {
      throw new ScalaJS.c.java\ufe33lang\ufe33CloneNotSupportedException().init\ufe33\ufe34();
    }
  }

  // Bridge for clone()
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.clone = function() {
    return this.clone\ufe34O();
  }

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.toString\ufe34T = function() {
    // getClass().getName() + "@" + Integer.toHexString(hashCode())
    var className = this.getClass\ufe34java\ufe33lang\ufe33Class().getName\ufe34T();
    var hashCode = this.hashCode\ufe34I();
    return className + '@' + hashCode.toString(16);
  }

  // Bridge for toString()
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.toString = function() {
    return this.toString\ufe34T();
  }

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.notify\ufe34V = function() {}
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.notifyAll\ufe34V = function() {}
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.wait\ufe34J\ufe34V = function() {}
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.wait\ufe34J\ufe34I\ufe34V = function() {}
  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.wait\ufe34V = function() {}

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.finalize\ufe34V = function() {}

  // Constructor bridge

  /** @constructor */
  ScalaJS.classes.java\ufe33lang\ufe33Object = function() {
    ScalaJS.c.java\ufe33lang\ufe33Object.call(this);
    return this.init\ufe33\ufe34();
  }
  ScalaJS.classes.java\ufe33lang\ufe33Object.prototype =
    ScalaJS.c.java\ufe33lang\ufe33Object.prototype;

  // Instance tests

  ScalaJS.is.java\ufe33lang\ufe33Object = function(obj) {
    return !!((obj && obj.$classData &&
      obj.$classData.ancestors.java\ufe33lang\ufe33Object) ||
      (typeof(obj) === "string"));
  };

  ScalaJS.as.java\ufe33lang\ufe33Object = function(obj) {
    if (ScalaJS.is.java\ufe33lang\ufe33Object(obj) || obj === null)
      return obj;
    else
      ScalaJS.throwClassCastException(obj, "java.lang.Object");
  };

  ScalaJS.isArrayOf.java\ufe33lang\ufe33Object = (function(obj, depth) {
    var data = obj && obj.$classData;
    if (!data)
      return false;
    var arrayDepth = data.arrayDepth || 0;
    
    if (arrayDepth < depth)
      return false; // because Array[A] </: Array[Array[A]]
    else if (arrayDepth > depth)
      return true; // because Array[Array[A]] <: Array[Object]
    else
      return !data.arrayBase.isPrimitive; // because Array[Int] </: Array[Object]
  });

  ScalaJS.asArrayOf.java\ufe33lang\ufe33Object = (function(obj, depth) {
    if ((ScalaJS.isArrayOf.java\ufe33lang\ufe33Object(obj, depth) || (obj === null))) {
      return obj
    } else {
      ScalaJS.throwArrayCastException(obj, "Ljava.lang.Object;", depth)
    }
  });

  // Data

  ScalaJS.data.java\ufe33lang\ufe33Object = {
    constr: ScalaJS.c.java\ufe33lang\ufe33Object,
    jsconstr: ScalaJS.classes.java\ufe33lang\ufe33Object,
    parentData: null,
    ancestors: {
      java\ufe33lang\ufe33Object: true
    },
    isPrimitive: false,
    isInterface: false,
    isArrayClass: false,
    componentData: null,
    zero: null,
    arrayEncodedName: "Ljava.lang.Object;",
    displayName: "java.lang.Object",
    _classOf: undefined,
    getClassOf: ScalaJS.classOfGetter,
    _arrayOf: undefined,
    getArrayOf: ScalaJS.arrayOfGetter,
    isInstance: ScalaJS.is.java\ufe33lang\ufe33Object,
    isArrayOf: ScalaJS.isArrayOf.java\ufe33lang\ufe33Object
  };

  ScalaJS.c.java\ufe33lang\ufe33Object.prototype.$classData =
    ScalaJS.data.java\ufe33lang\ufe33Object;
})();
