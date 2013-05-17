/* ------------------
 * Ref types in scala.runtime._
 * ------------------ */

(function ($env) {
  function registerRefType(elemShortName, elemCodeName, isVolatile, zero) {
    var isObject = elemShortName === "Object";
    var name = "scala.runtime." +
      (isVolatile ? "Volatile" : "") + elemShortName + "Ref";
    var constructorName = "<init>("+elemCodeName+")";

    $env.registerClass(name, function($env) {
      var ObjectClass = $env.c["java.lang.Object"];

      function Class() {
        ObjectClass.prototype.constructor.call(this);
        this.$jsfield$elem = zero;
      }
      Class.prototype = Object.create(ObjectClass.prototype);
      Class.prototype.constructor = Class;

      Class.prototype[constructorName] = function(elem) {
        ObjectClass.prototype["<init>()"].call(this);
        this.$jsfield$elem = elem;
        return this;
      }

      Class.prototype["toString():java.lang.String"] = function() {
        return this.$jsfield$elem.toString();
      }

      function JSClass(elem) {
        Class.call(this);
        return this[constructorName](elem);
      }
      JSClass.prototype = Class.prototype;

      var ancestors = {
        "java.io.Serializable": true,
        "java.lang.Object": true
      };
      ancestors[name] = true;

      $env.createClass(name, Class, JSClass, "java.lang.Object", ancestors);
    });
  }

  for (var volat = 0; volat < 2; volat++) {
    var isVolatile = volat != 0;
    registerRefType("Boolean", "Z", isVolatile, false);
    registerRefType("Char", "C", isVolatile, 0);
    registerRefType("Byte", "B", isVolatile, 0);
    registerRefType("Short", "S", isVolatile, 0);
    registerRefType("Int", "I", isVolatile, 0);
    registerRefType("Long", "J", isVolatile, 0);
    registerRefType("Float", "F", isVolatile, 0.0);
    registerRefType("Double", "D", isVolatile, 0.0);
    registerRefType("Object", "O", isVolatile, null);
  }
})($ScalaJSEnvironment);
