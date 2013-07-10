/* ------------------
 * java.lang.Object
 * ------------------ */

(function ($env) {
  $env.registerClass("java.lang.Object", {java\ufe33lang\ufe33Object:0}, function($env) {
    function ObjectClass() {
    }
    ObjectClass.prototype.constructor = ObjectClass;

    ObjectClass.prototype.init\ufe33\ufe34 = function() {
      return this;
    }

    ObjectClass.prototype.getClass\ufe34java\ufe33lang\ufe33Class = function() {
      return this.$classData.cls;
    }

    // Bridge for getClass()
    ObjectClass.prototype.getClass = function() {
      return this.getClass\ufe34java\ufe33lang\ufe33Class();
    }

    ObjectClass.prototype.hashCode\ufe34I = function() {
      // TODO
      return 42;
    }

    // Bridge for hashCode()
    ObjectClass.prototype.hashCode = function() {
      return this.hashCode\ufe34I();
    }

    ObjectClass.prototype.equals\ufe34O\ufe34Z = function(rhs) {
      return this === rhs;
    }

    // Bridge for equals(Object)
    ObjectClass.prototype.equals = function(that) {
      return this.equals\ufe34O\ufe34Z(that);
    }

    ObjectClass.prototype.clone\ufe34O = function() {
      if ($env.isInstance(this, "java.lang.Cloneable")) {
        throw new this.c["scala.NotImplementedError"]().init\ufe33\ufe34;
      } else {
        throw new this.c["java.lang.CloneNotSupportedException"]().init\ufe33\ufe34;
      }
    }

    // Bridge for clone()
    ObjectClass.prototype.clone = function() {
      return this.clone\ufe34O();
    }

    ObjectClass.prototype.toString\ufe34T = function() {
      // getClass().getName() + "@" + Integer.toHexString(hashCode())
      var className = this.getClass\ufe34java\ufe33lang\ufe33Class().getName\ufe34T();
      var hashCode = this.hashCode\ufe34I();
      return className + '@' + hashCode.toString(16);
    }

    // Bridge for toString()
    ObjectClass.prototype.toString = function() {
      return this.toString\ufe34T();
    }

    ObjectClass.prototype.notify\ufe34V = function() {}
    ObjectClass.prototype.notifyAll\ufe34V = function() {}
    ObjectClass.prototype.wait\ufe34J\ufe34V = function() {}
    ObjectClass.prototype.wait\ufe34J\ufe34I\ufe34V = function() {}
    ObjectClass.prototype.wait\ufe34V = function() {}

    ObjectClass.prototype.finalize\ufe34V = function() {}

    // Constructor bridge
    function JSObjectClass() {
      ObjectClass.call(this);
      return this.init\ufe33\ufe34();
    }
    JSObjectClass.prototype = ObjectClass.prototype;

    $env.createClass("java.lang.Object", ObjectClass, JSObjectClass, null, {
      "java.lang.Object": true
    });
  });
})($ScalaJSEnvironment);
