/* ------------------
 * java.lang.Class
 * ------------------ */

(function ($env) {
  var ObjectClass = $env.classes["java.lang.Object"].type;

  function ClassClass() {
    ObjectClass.prototype.constructor.call(this);
    this.$data = null;
  }
  ClassClass.prototype = Object.create(ObjectClass.prototype);
  ClassClass.prototype.constructor = ClassClass;

  ClassClass.prototype["<init>(<special>):java.lang.Class"] = function(data) {
    ObjectClass.prototype["<init>():java.lang.Object"].call(this);
    this.$data = data;
    return this;
  }

  ClassClass.prototype["toString():java.lang.String"] = function() {
    return $env.makeNativeStrWrapper("class " + this.$data.name);
  }

  $env.createClass("java.lang.Class", ClassClass, "java.lang.Object", {
    "java.lang.Object": true,
    "java.lang.Class": true
  });
})($ScalaJSEnvironment);
