(function() {
  var $g = (typeof global !== "undefined" && global.Object === Object)
    ? global
    : this;
  var ScalaJSDefinedTestNativeParentClass = function(x) {
    $g.Object.call(this);
    this.x = x;
  };
  ScalaJSDefinedTestNativeParentClass.prototype.foo = function(s) {
    return s + this.x;
  };
  $g.Object.defineProperty(ScalaJSDefinedTestNativeParentClass.prototype, "bar", {
    "configurable": false,
    "enumerable": false,
    "get": function() {
      return this.x << 1; // x * 2 would not return an Int, technically
    }
  });
  $g.ScalaJSDefinedTestNativeParentClass = ScalaJSDefinedTestNativeParentClass;
}).call(this);
