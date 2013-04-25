/* ------------------
 * java.lang.Object
 * ------------------ */

(function ($env) {
  $env.registerClass("java.lang.Object", function($env) {
    function ObjectClass() {
      return this["<init>():java.lang.Object"]();
    }
    ObjectClass.prototype.constructor = ObjectClass;

    ObjectClass.prototype["<init>():java.lang.Object"] = function() {
      return this;
    }

    ObjectClass.prototype["getClass():java.lang.Class"] = function() {
      return this.$classData.class;
    }

    // Bridge for getClass()
    ObjectClass.prototype.getClass = function() {
      return this["getClass():java.lang.Class"]();
    }

    ObjectClass.prototype["hashCode():scala.Int"] = function() {
      // TODO
      return 42;
    }

    // Bridge for hashCode()
    ObjectClass.prototype.hashCode = function() {
      return this["hashCode():scala.Int"]();
    }

    ObjectClass.prototype["equals(java.lang.Object):scala.Boolean"] = function(rhs) {
      return this === rhs;
    }

    // Bridge for equals(Object)
    ObjectClass.prototype.equals = function(that) {
      return this["equals(java.lang.Object):scala.Boolean"](that);
    }

    ObjectClass.prototype["clone():java.lang.Object"] = function() {
      if ($env.isInstance(this, "java.lang.Cloneable")) {
        throw new this.classes["scala.NotImplementedError"].type();
      } else {
        throw new this.classes["java.lang.CloneNotSupportedException"].type();
      }
    }

    // Bridge for clone()
    ObjectClass.prototype.clone = function() {
      return this["clone():java.lang.Object"]();
    }

    ObjectClass.prototype["toString():java.lang.String"] = function() {
      // getClass().getName() + "@" + Integer.toHexString(hashCode())
      var className = this["getClass():java.lang.Class"]()["getName():java.lang.String"]();
      var hashCode = this["hashCode():scala.Int"]();
      return className + '@' + hashCode.toString(16);
    }

    // Bridge for toString()
    ObjectClass.prototype.toString = function() {
      return this["toString():java.lang.String"]();
    }

    ObjectClass.prototype["notify():scala.Unit"] = function() {}
    ObjectClass.prototype["notifyAll():scala.Unit"] = function() {}
    ObjectClass.prototype["wait(scala.Long):scala.Unit"] = function() {}
    ObjectClass.prototype["wait(scala.Long,scala.Int):scala.Unit"] = function() {}
    ObjectClass.prototype["wait():scala.Unit"] = function() {}

    ObjectClass.prototype["finalize():scala.Unit"] = function() {}

    $env.createClass("java.lang.Object", ObjectClass, null, {
      "java.lang.Object": true
    });
  });
})($ScalaJSEnvironment);
