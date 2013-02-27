/* ------------------
 * Definitions of core native methods
 * ------------------ */

(function ($env) {
  // java.lang.Boolean

  $env.registerNative("java.lang.Boolean :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.Character

  $env.registerNative("java.lang.Character :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(String.fromCharCode(this["charValue():scala.Char"]()));
  });

  // java.lang.Byte

  $env.registerNative("java.lang.Byte :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.Short

  $env.registerNative("java.lang.Short :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.Integer

  $env.registerNative("java.lang.Integer :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.Long

  $env.registerNative("java.lang.Long :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.Float

  $env.registerNative("java.lang.Float :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.Double

  $env.registerNative("java.lang.Double :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.System

  $env.registerNative("java.lang.System$ :: arraycopy(java.lang.Object,scala.Int,java.lang.Object,scala.Int,scala.Int):scala.Unit",
    function(src, srcPos, dest, destPos, count) {
      // TODO Throw errors properly
      var srcUnderlying = src.underlying;
      var destUnderlying = dest.underlying;
      for (var i = 0; i < count; i++) {
        destUnderlying[destPos+i] = srcUnderlying[srcPos+i];
      }
    });

  // java.lang.StandardOutPrintStream$

  $env.registerNative("java.lang.StandardOutPrintStream$ :: writeString(java.lang.String):scala.Unit", function(x) {
    console.log("out: " + x.toNativeString());
  });

  // java.lang.StandardErrPrintStream$

  $env.registerNative("java.lang.StandardErrPrintStream$ :: writeString(java.lang.String):scala.Unit", function(x) {
    console.log("err: " + x.toNativeString());
  });
})($ScalaJSEnvironment);
