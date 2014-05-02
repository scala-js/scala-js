// jasmine.js requires the following 4 to be defined.
(function() {
  var g = (typeof global === "object" && global && global["Object"] === Object) ? global : this;
  var stub = function() { console.log("jasmine-polyfill.js stub called"); };
  g.setTimeout    = g.setTimeout    || stub;
  g.clearTimeout  = g.clearTimeout  || stub;
  g.setInterval   = g.setInterval   || stub;
  g.clearInterval = g.clearInterval || stub;
}).call(this);
