(function() {
  var $g = (typeof global !== "undefined" && global.Object === Object)
    ? global
    : this;
  function Foo(input){
    if (typeof input !== "string") throw new Error("input should be string");
    this.str = input;
  }
  function Bar(input) {
    if (typeof input !== "number") throw new Error("input should be number");
    this.num = input;
  }
  Foo.Bar = Bar;
  $g.Foo = Foo;
}).call(this);
