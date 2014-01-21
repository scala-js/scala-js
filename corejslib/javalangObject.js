/* Scala.js runtime support
 * Copyright 2013 LAMP/EPFL
 * Author: Sébastien Doeraene
 */

/* ------------------
 * java.lang.Object
 * ------------------ */

/** @constructor */
ScalaJS.c.java_lang_Object = function() {
};

/** @constructor */
ScalaJS.inheritable.java_lang_Object = function() {};
ScalaJS.inheritable.java_lang_Object.prototype =
  ScalaJS.c.java_lang_Object.prototype;

ScalaJS.c.java_lang_Object.prototype.init___ = function() {
  return this;
}

ScalaJS.c.java_lang_Object.prototype.getClass__java_lang_Class = function() {
  return this.$classData.getClassOf();
}

// Bridge for getClass()
ScalaJS.c.java_lang_Object.prototype.getClass = function() {
  return this.getClass__java_lang_Class();
}

ScalaJS.c.java_lang_Object.prototype.hashCode__I = function() {
  // TODO
  return 42;
}

// Bridge for hashCode()
ScalaJS.c.java_lang_Object.prototype.hashCode = function() {
  return this.hashCode__I();
}

ScalaJS.c.java_lang_Object.prototype.equals__O__Z = function(rhs) {
  return this === rhs;
}

// Bridge for equals(Object)
ScalaJS.c.java_lang_Object.prototype.equals = function(that) {
  return this.equals__O__Z(that);
}

ScalaJS.c.java_lang_Object.prototype.clone__O = function() {
  if (ScalaJS.is.java_lang_Cloneable(this)) {
    function Clone(from) {
      for (var field in from)
        if (from["hasOwnProperty"](field))
          this[field] = from[field];
    }
    Clone.prototype = ScalaJS.g["Object"]["getPrototypeOf"](this);
    return new Clone(this);
  } else {
    throw new ScalaJS.c.java_lang_CloneNotSupportedException().init___();
  }
}

// Bridge for clone()
ScalaJS.c.java_lang_Object.prototype.clone = function() {
  return this.clone__O();
}

ScalaJS.c.java_lang_Object.prototype.toString__T = function() {
  // getClass().getName() + "@" + Integer.toHexString(hashCode())
  var className = this.getClass__java_lang_Class().getName__T();
  var hashCode = this.hashCode__I();
  return className + '@' + hashCode.toString(16);
}

// Bridge for toString()
ScalaJS.c.java_lang_Object.prototype.toString = function() {
  return this.toString__T();
}

ScalaJS.c.java_lang_Object.prototype.notify__V = function() {}
ScalaJS.c.java_lang_Object.prototype.notifyAll__V = function() {}
ScalaJS.c.java_lang_Object.prototype.wait__J__V = function() {}
ScalaJS.c.java_lang_Object.prototype.wait__J__I__V = function() {}
ScalaJS.c.java_lang_Object.prototype.wait__V = function() {}

ScalaJS.c.java_lang_Object.prototype.finalize__V = function() {}

// Constructor bridge

/** @constructor */
ScalaJS.classes.java_lang_Object = function() {
  ScalaJS.c.java_lang_Object.call(this);
  return this.init___();
}
ScalaJS.classes.java_lang_Object.prototype =
  ScalaJS.c.java_lang_Object.prototype;

// Instance tests

ScalaJS.is.java_lang_Object = function(obj) {
  return !!((obj && obj.$classData &&
    obj.$classData.ancestors.java_lang_Object) ||
    (typeof(obj) === "string"));
};

ScalaJS.as.java_lang_Object = function(obj) {
  if (ScalaJS.is.java_lang_Object(obj) || obj === null)
    return obj;
  else
    ScalaJS.throwClassCastException(obj, "java.lang.Object");
};

ScalaJS.isArrayOf.java_lang_Object = (function(obj, depth) {
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

ScalaJS.asArrayOf.java_lang_Object = (function(obj, depth) {
  if ((ScalaJS.isArrayOf.java_lang_Object(obj, depth) || (obj === null))) {
    return obj
  } else {
    ScalaJS.throwArrayCastException(obj, "Ljava.lang.Object;", depth)
  }
});

// Data

ScalaJS.data.java_lang_Object =
  new ScalaJS.ClassTypeData(
    {java_lang_Object:0},
    false, "java.lang.Object", null,
    {java_lang_Object: 1},
    ScalaJS.is.java_lang_Object,
    ScalaJS.isArrayOf.java_lang_Object);

ScalaJS.c.java_lang_Object.prototype.$classData =
  ScalaJS.data.java_lang_Object;
