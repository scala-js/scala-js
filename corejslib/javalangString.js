/* ------------------
 * java.lang.String
 * ------------------ */

(function ($env) {
  var ObjectClass = $env.classes["java.lang.Object"].type;

  function StringClass() {
    ObjectClass.prototype.constructor.call(this);
  }
  StringClass.prototype = Object.create(ObjectClass.prototype);
  StringClass.prototype.constructor = StringClass;

  StringClass.prototype["<init>():java.lang.String"] = function() {
    ObjectClass.prototype["<init>():java.lang.Object"].call(this);
    return this;
  }

  StringClass.prototype["toString():java.lang.String"] = function() {
    return this;
  }

  StringClass.prototype["$plus(java.lang.String):java.lang.String"] = function(x) {
    return $env.makeNativeStrWrapper(this.toNativeString() + x.toNativeString());
  }

  StringClass.prototype["$plus(java.lang.Object):java.lang.String"] = function(x) {
    return this["$plus(java.lang.String):java.lang.String"](x["toString():java.lang.String"]());
  }

  StringClass.prototype.toNativeString = function() {
    return "<not a native string>";
  }

  StringClass.prototype["length():scala.Int"] = function() {
    return this.toNativeString.length;
  }

  StringClass.prototype["charAt(scala.Int):scala.Char"] = function(index) {
    return this.toNativeString.charCodeAt(index);
  }

  StringClass.prototype["codePointAt(scala.Int):scala.Int"] = function(index) {
    return this.toNativeString.charCodeAt(index);
  }

  $env.createClass("java.lang.String", StringClass, "java.lang.Object", {
    "java.lang.Object": true,
    "java.lang.String": true
  });

  /* ------------------
   * Wrapper for native JavaScript strings
   * ------------------ */

  function NativeStringWrapper(nativeStr) {
    this.nativeStr = nativeStr;
    StringClass.prototype.constructor.call(this);
  }
  NativeStringWrapper.prototype = Object.create(StringClass.prototype);
  NativeStringWrapper.prototype.constructor = NativeStringWrapper;

  NativeStringWrapper.prototype.toNativeString = function() {
    return this.nativeStr;
  }

  $env.makeNativeStrWrapper = function(nativeStr) {
    return new NativeStringWrapper(nativeStr)["<init>():java.lang.String"]();
  }
})($ScalaJSEnvironment);
