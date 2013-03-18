/* ------------------
 * java.lang.Object
 * ------------------ */

(function ($env) {
  function ObjectClass() {
  }
  ObjectClass.prototype.constructor = ObjectClass;

  ObjectClass.prototype["<init>():java.lang.Object"] = function() {
    return this;
  }

  ObjectClass.prototype["getClass():java.lang.Class"] = function() {
    return this.$classData.class;
  }

  ObjectClass.prototype.getClass = ObjectClass.prototype["getClass():java.lang.Class"];

  /** In the JavaDoc, defined as:
   *  getClass().getName() + '@' + Integer.toHexString(hashCode())
   */
  ObjectClass.prototype["toString():java.lang.String"] = function() {
    var nativeClassName = this.getClass().getName();
    var hashCode = this["hashCode():scala.Int"]();
    return nativeClassName + '@' + hashCode;
  }

  ObjectClass.prototype.toString = function() {
    return this["toString():java.lang.String"]();
  }

  ObjectClass.prototype["equals(java.lang.Object):scala.Boolean"] = function(rhs) {
    return this === rhs;
  }

  ObjectClass.prototype["hashCode():scala.Int"] = function() {
    // TODO
    return 42;
  }

  $env.createClass("java.lang.Object", ObjectClass, null, {
    "java.lang.Object": true
  });
})($ScalaJSEnvironment);
