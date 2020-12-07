exports.strConstant = "value";

exports.ssum = function(x, y) {
  if (y === (void 0))
    y = 1;
  return x * x + y * y;
};

exports.default = function() {
  return 5;
};

exports.MyBox = function(x) {
  this.x = x;
};

exports.MyBox.prototype.get = function() { return this.x; };
exports.MyBox.prototype.set = function(x) { this.x = x; };

exports.MyBox.make = function(x) {
  return new exports.MyBox(x);
};
