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

ScalaJS.c.java_lang_Object.prototype.getClass__Ljava_lang_Class = function() {
  return this.$classData.getClassOf();
}

ScalaJS.c.java_lang_Object.prototype.hashCode__I = function() {
  // TODO
  return 42;
}

ScalaJS.c.java_lang_Object.prototype.equals__O__Z = function(rhs) {
  return this === rhs;
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

ScalaJS.c.java_lang_Object.prototype.toString__T = function() {
  // getClass().getName() + "@" + Integer.toHexString(hashCode())
  var className = this.getClass__Ljava_lang_Class().getName__T();
  var hashCode = this.hashCode__I();
  return className + '@' + hashCode.toString(16);
}

// JSExport for toString(). We always need to export this, since we
// rely on JS calling it automatically when we do things like:
// `"" + obj`
ScalaJS.c.java_lang_Object.prototype.toString = function() {
  return this.toString__T();
}

// Notify and notify all:
// Although we do not support wait on Object as it does not make sense in JS, we
// allow notify to be called in order to support code that calls only notify but
// never wait. Further, note that these methods are not in the sjsinfo file.
// Therefore, dce will complain about them not being reachable, but the code
// will still work.
ScalaJS.c.java_lang_Object.prototype.notify__V = function() {}
ScalaJS.c.java_lang_Object.prototype.notifyAll__V = function() {}

ScalaJS.c.java_lang_Object.prototype.finalize__V = function() {}

// Reflective call proxies for methods on java.lang.Object
// Note that we do not need to proxy the following methods, since they are
// defined on Any in the Scala hierarchy and therefore a reflective call is
// never issued:
// - equals
// - getClass
// - hashCode
// - toString
ScalaJS.c.java_lang_Object.prototype.clone__ = function() {
  return this.clone__O()
}
ScalaJS.c.java_lang_Object.prototype.notify__ = function() {
  return this.notify__V()
}
ScalaJS.c.java_lang_Object.prototype.notifyAll__ = function() {
  return this.notifyAll__V()
}
ScalaJS.c.java_lang_Object.prototype.finalize__ = function() {
  return this.finalize__V()
}

// Instance tests

ScalaJS.is.java_lang_Object = function(obj) {
  return !!((obj && obj.$classData &&
    obj.$classData.ancestors.java_lang_Object) ||
    (typeof(obj) === "string") ||
    (typeof(obj) === "number") ||
    (typeof(obj) === "boolean") ||
    obj === void 0);
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
