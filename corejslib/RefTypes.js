/* ------------------
 * Ref types in scala.runtime._
 * ------------------ */

(function ($env) {
  function registerRefType(elemShortName, isVolatile, zero) {
    var isObject = elemShortName === "Object";
    var name = "scala.runtime." +
      (isVolatile ? "Volatile" : "") + elemShortName + "Ref";
    var elemTypeName = isObject ? "java.lang.Object" : "scala."+elemShortName;
    var constructorName = "<init>("+elemTypeName+"):"+name;

    $env.registerClass(name, function($env) {
      var ObjectClass = $env.c["java.lang.Object"];

      function Class() {
        ObjectClass.prototype.constructor.call(this);
        this.$jsfield$elem = zero;
      }
      Class.prototype = Object.create(ObjectClass.prototype);
      Class.prototype.constructor = Class;

      Class.prototype[constructorName] = function(elem) {
        ObjectClass.prototype["<init>():java.lang.Object"].call(this);
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

  for (var volatile = 0; volatile < 2; volatile++) {
    var isVolatile = volatile != 0;
    registerRefType("Boolean", isVolatile, false);
    registerRefType("Char", isVolatile, 0);
    registerRefType("Byte", isVolatile, 0);
    registerRefType("Short", isVolatile, 0);
    registerRefType("Int", isVolatile, 0);
    registerRefType("Long", isVolatile, 0);
    registerRefType("Float", isVolatile, 0.0);
    registerRefType("Double", isVolatile, 0.0);
    registerRefType("Object", isVolatile, null);
  }
})($ScalaJSEnvironment);
