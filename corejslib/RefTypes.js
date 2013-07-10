/* ------------------
 * Ref types in scala.runtime._
 * ------------------ */

(function ($env) {
  function registerRefType(baseName, elemCodeName, zero, propNameObj) {
    var name = "scala.runtime." + baseName + "Ref";

    $env.registerClass(name, propNameObj, function($env) {
      var ObjectClass = $env.c["java.lang.Object"];

      function Class() {
        ObjectClass.prototype.constructor.call(this);
        this.$jsfield$elem = zero;
      }
      Class.prototype = Object.create(ObjectClass.prototype);
      Class.prototype.constructor = Class;

      var init = function(elem) {
        ObjectClass.prototype.init\ufe33\ufe34.call(this);
        this.$jsfield$elem = elem;
        return this;
      }

      switch (elemCodeName) {
        case "Z": Class.prototype.init\ufe33\ufe34Z = init; break;
        case "C": Class.prototype.init\ufe33\ufe34C = init; break;
        case "B": Class.prototype.init\ufe33\ufe34B = init; break;
        case "S": Class.prototype.init\ufe33\ufe34S = init; break;
        case "I": Class.prototype.init\ufe33\ufe34I = init; break;
        case "J": Class.prototype.init\ufe33\ufe34J = init; break;
        case "F": Class.prototype.init\ufe33\ufe34F = init; break;
        case "D": Class.prototype.init\ufe33\ufe34D = init; break;
        case "O": Class.prototype.init\ufe33\ufe34O = init; break;
      }

      Class.prototype.toString\ufe34T = function() {
        return this.$jsfield$elem.toString();
      }

      function JSClass(elem) {
        Class.call(this);
        return init(elem);
      }
      JSClass.prototype = Class.prototype;

      var ancestors = {
        "java.io.Serializable": true,
        "java.lang.Object": true
      };
      ancestors[name] = true;

      $env.createClass(name, Class, JSClass, "java.lang.Object", ancestors);
    });
  }

  registerRefType("Boolean", "Z", false, {scala\ufe33runtime\ufe33BooleanRef:0});
  registerRefType("Char",    "C", 0,     {scala\ufe33runtime\ufe33CharRef:0});
  registerRefType("Byte",    "B", 0,     {scala\ufe33runtime\ufe33ByteRef:0});
  registerRefType("Short",   "S", 0,     {scala\ufe33runtime\ufe33ShortRef:0});
  registerRefType("Int",     "I", 0,     {scala\ufe33runtime\ufe33IntRef:0});
  registerRefType("Long",    "J", 0,     {scala\ufe33runtime\ufe33LongRef:0});
  registerRefType("Float",   "F", 0.0,   {scala\ufe33runtime\ufe33FloatRef:0});
  registerRefType("Double",  "D", 0.0,   {scala\ufe33runtime\ufe33DoubleRef:0});
  registerRefType("Object",  "O", null,  {scala\ufe33runtime\ufe33ObjectRef:0});

  registerRefType("VolatileBoolean", "Z", false, {scala\ufe33runtime\ufe33VolatileBooleanRef:0});
  registerRefType("VolatileChar",    "C", 0,     {scala\ufe33runtime\ufe33VolatileCharRef:0});
  registerRefType("VolatileByte",    "B", 0,     {scala\ufe33runtime\ufe33VolatileByteRef:0});
  registerRefType("VolatileShort",   "S", 0,     {scala\ufe33runtime\ufe33VolatileShortRef:0});
  registerRefType("VolatileInt",     "I", 0,     {scala\ufe33runtime\ufe33VolatileIntRef:0});
  registerRefType("VolatileLong",    "J", 0,     {scala\ufe33runtime\ufe33VolatileLongRef:0});
  registerRefType("VolatileFloat",   "F", 0.0,   {scala\ufe33runtime\ufe33VolatileFloatRef:0});
  registerRefType("VolatileDouble",  "D", 0.0,   {scala\ufe33runtime\ufe33VolatileDoubleRef:0});
  registerRefType("VolatileObject",  "O", null,  {scala\ufe33runtime\ufe33VolatileObjectRef:0});
})($ScalaJSEnvironment);
