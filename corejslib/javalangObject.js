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

  ObjectClass.prototype["toString():java.lang.String"] = function() {
    return $env.makeNativeStrWrapper("<a Scala Object>");
  }

  $env.createClass("java.lang.Object", ObjectClass, null, {
    "java.lang.Object": true
  });
})($ScalaJSEnvironment);
