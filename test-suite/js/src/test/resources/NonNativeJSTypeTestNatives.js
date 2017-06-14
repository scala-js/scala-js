(function() {
  var $g = (typeof global !== "undefined" && global.Object === Object)
    ? global
    : this;
  var NonNativeJSTypeTestNativeParentClass = function(x) {
    $g.Object.call(this);
    this.x = x;
  };
  NonNativeJSTypeTestNativeParentClass.prototype.foo = function(s) {
    return s + this.x;
  };
  NonNativeJSTypeTestNativeParentClass.prototype.methodWithDefault = function (x) {
    return x || 5;
  }
  $g.Object.defineProperty(NonNativeJSTypeTestNativeParentClass.prototype, "bar", {
    "configurable": false,
    "enumerable": false,
    "get": function() {
      return this.x << 1; // x * 2 would not return an Int, technically
    }
  });
  $g.NonNativeJSTypeTestNativeParentClass = NonNativeJSTypeTestNativeParentClass;

  /* Deferred members:
   * val x: Int
   * def bar(y: Int): Int
   */
  var NonNativeJSTypeTestNativeParentClassWithDeferred = function() {
    $g.Object.call(this);
  };
  NonNativeJSTypeTestNativeParentClassWithDeferred.prototype.foo = function(y) {
    return (this.bar((y + 4) | 0) + this.x) | 0;
  };
  $g.NonNativeJSTypeTestNativeParentClassWithDeferred =
    NonNativeJSTypeTestNativeParentClassWithDeferred;

  var NonNativeJSTypeTestNativeParentClassWithVarargs = function(x) {
    $g.Object.call(this);
    this.x = x;
    this.args = [];
    for (var i = 1; i != arguments.length; ++i)
      this.args.push(arguments[i]);
  };
  $g.NonNativeJSTypeTestNativeParentClassWithVarargs =
    NonNativeJSTypeTestNativeParentClassWithVarargs;

  var ConstructorDefaultParam = function(foo) {
    $g.Object.call(this);
    if (foo == undefined) {
      this.foo = -1;
    } else {
      this.foo = foo;
    }
  };
  $g.ConstructorDefaultParam = ConstructorDefaultParam;

  $g.JSNativeObjectInPackageFoo = {};
  $g.JSNativeObjectInPackageFooJSNameOmitted = $g.JSNativeObjectInPackageFoo;
  $g.JSNativeObjectInPackageBar = {};
  var JSNativeClassInPackageFoo = function() {
    $g.Object.call(this);
  };
  JSNativeClassInPackageFoo.prototype.foo = function() {
    return "foo";
  };
  $g.JSNativeClassInPackageFoo = JSNativeClassInPackageFoo;
  $g.JSNativeClassInPackageFooJSNameOmitted = $g.JSNativeClassInPackageFoo;
  var JSNativeClassInPackageBar = function() {
    $g.Object.call(this);
  };
  JSNativeClassInPackageBar.prototype.baz = function() {
    return "baz";
  };
  $g.JSNativeClassInPackageBar = JSNativeClassInPackageBar;
}).call(this);
