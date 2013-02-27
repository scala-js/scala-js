/* ------------------
 * Array class factory
 * ------------------ */

(function ($env) {
  $env.createArrayTypeFunction = function(name, componentData) {
    var ObjectClass = $env.classes["java.lang.Object"].type;
    var mangledName = componentData.name + "[]";

    function ArrayClass(arg) {
      ObjectClass.prototype.constructor.call(this);

      if (typeof(arg) === "number") {
        // arg is the length of the array
        this.underlying = new Array(arg);
        zero = componentData.zero;
        for (var i = 0; i < arg; i++)
          this.underlying[i] = zero;
      } else {
        // arg is a native array that we wrap
        this.underlying = arg;
      }

      ObjectClass.prototype["<init>():java.lang.Object"].call(this);
    }
    ArrayClass.prototype = Object.create(ObjectClass.prototype);
    ArrayClass.prototype.constructor = ArrayClass;

    ArrayClass.prototype.length = function() {
      return this.underlying.length;
    }

    ArrayClass.prototype.get = function(index) {
      return this.underlying[index];
    }

    ArrayClass.prototype.set = function(index, value) {
      this.underlying[index] = value;
    }

    return ArrayClass;
  }
})($ScalaJSEnvironment);
