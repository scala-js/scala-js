function Foo(input){
  if (typeof input !== "string") throw new Error("input should be string");
  this.str = input;
}
function Bar(input) {
  if (typeof input !== "number") throw new Error("input should be number");
  this.num = input;
}
Foo.Bar = Bar;

module.exports = Foo;
