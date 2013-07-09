/* ------------------
 * Ref types in scala.runtime._
 * ------------------ */

(function ($env) {
  function registerRefType(elemShortName, elemCodeName, isVolatile, zero) {
    var name = "scala.runtime." +
      (isVolatile ? "Volatile" : "") + elemShortName + "Ref";

    $env.registerClass(name, function($env) {
      var ObjectClass = $env.c["java.lang.Object"];

      function Class() {
        ObjectClass.prototype.constructor.call(this);
        this.$jsfield$elem = zero;
      }
      Class.prototype = Object.create(ObjectClass.prototype);
      Class.prototype.constructor = Class;

      var init = function(elem) {
        ObjectClass.prototype.init\ufe33\ufe34.call(this);
        this.$jsfield$elem = elem;
        return this;
      }

      switch (elemCodeName) {
        case "Z": Class.prototype.init\ufe33\ufe34Z = init; break;
        case "C": Class.prototype.init\ufe33\ufe34C = init; break;
        case "B": Class.prototype.init\ufe33\ufe34B = init; break;
        case "S": Class.prototype.init\ufe33\ufe34S = init; break;
        case "I": Class.prototype.init\ufe33\ufe34I = init; break;
        case "J": Class.prototype.init\ufe33\ufe34J = init; break;
        case "F": Class.prototype.init\ufe33\ufe34F = init; break;
        case "D": Class.prototype.init\ufe33\ufe34D = init; break;
        case "O": Class.prototype.init\ufe33\ufe34O = init; break;
      }

      Class.prototype.toString\ufe34T = function() {
        return this.$jsfield$elem.toString();
      }

      function JSClass(elem) {
        Class.call(this);
        return init(elem);
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
