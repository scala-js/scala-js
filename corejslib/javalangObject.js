/* ------------------
 * java.lang.Object
 * ------------------ */

(function ($env) {
  $env.registerClass("java.lang.Object", function($env) {
    function ObjectClass() {
    }
    ObjectClass.prototype.constructor = ObjectClass;

    ObjectClass.prototype["<init>()O"] = function() {
      return this;
    }

    ObjectClass.prototype["getClass()Ljava.lang.Class;"] = function() {
      return this.$classData.class;
    }

    // Bridge for getClass()
    ObjectClass.prototype.getClass = function() {
      return this["getClass()Ljava.lang.Class;"]();
    }

    ObjectClass.prototype["hashCode()I"] = function() {
      // TODO
      return 42;
    }

    // Bridge for hashCode()
    ObjectClass.prototype.hashCode = function() {
      return this["hashCode()I"]();
    }

    ObjectClass.prototype["equals(O)Z"] = function(rhs) {
      return this === rhs;
    }

    // Bridge for equals(Object)
    ObjectClass.prototype.equals = function(that) {
      return this["equals(O)Z"](that);
    }

    ObjectClass.prototype["clone()O"] = function() {
      if ($env.isInstance(this, "java.lang.Cloneable")) {
        throw new this.classes["scala.NotImplementedError"].jsconstructor();
      } else {
        throw new this.classes["java.lang.CloneNotSupportedException"].jsconstructor();
      }
    }

    // Bridge for clone()
    ObjectClass.prototype.clone = function() {
      return this["clone()O"]();
    }

    ObjectClass.prototype["toString()T"] = function() {
      // getClass().getName() + "@" + Integer.toHexString(hashCode())
      var className = this["getClass()Ljava.lang.Class;"]()["getName()T"]();
      var hashCode = this["hashCode()I"]();
      return className + '@' + hashCode.toString(16);
    }

    // Bridge for toString()
    ObjectClass.prototype.toString = function() {
      return this["toString()T"]();
    }

    ObjectClass.prototype["notify()V"] = function() {}
    ObjectClass.prototype["notifyAll()V"] = function() {}
    ObjectClass.prototype["wait(J)V"] = function() {}
    ObjectClass.prototype["wait(JI)V"] = function() {}
    ObjectClass.prototype["wait()V"] = function() {}

    ObjectClass.prototype["finalize()V"] = function() {}

    // Constructor bridge
    function JSObjectClass() {
      ObjectClass.call(this);
      return this["<init>()O"]();
    }
    JSObjectClass.prototype = ObjectClass.prototype;

    $env.createClass("java.lang.Object", ObjectClass, JSObjectClass, null, {
      "java.lang.Object": true
    });
  });
})($ScalaJSEnvironment);
