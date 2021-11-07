export const strConstant = "value";

export function ssum(x, y) {
  if (y === (void 0))
    y = 1;
  return x * x + y * y;
}

export function apply(x) {
  return 3 * x;
}

export default function() {
  return 5;
}

export function MyBox(x) {
  this.x = x;
}

MyBox.prototype.get = function() { return this.x; };
MyBox.prototype.set = function(x) { this.x = x; };
MyBox.make = function(x) { return new MyBox(x); };
