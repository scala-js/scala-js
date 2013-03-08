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

  $env.registerNative("java.lang.Integer$ :: parseInt(java.lang.String):scala.Int", function(str) {
    return parseInt(str.toNativeString());
  });

  // java.lang.Long

  $env.registerNative("java.lang.Long :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.Float

  $env.registerNative("java.lang.Float :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  $env.registerNative("java.lang.Float$ :: parseFloat(java.lang.String):scala.Int", function(str) {
    return parseFloat(str.toNativeString());
  });

  // java.lang.Double

  $env.registerNative("java.lang.Double :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(this["$jsfield$value "].toString());
  });

  // java.lang.System

  $env.registerNative("java.lang.System$ :: currentTimeMillis():scala.Long", function() {
    return new Date().getTime();
  });

  $env.registerNative("java.lang.System$ :: arraycopy(java.lang.Object,scala.Int,java.lang.Object,scala.Int,scala.Int):scala.Unit",
    function(src, srcPos, dest, destPos, count) {
      // TODO Throw errors properly
      var srcUnderlying = src.underlying;
      var destUnderlying = dest.underlying;
      for (var i = 0; i < count; i++) {
        destUnderlying[destPos+i] = srcUnderlying[srcPos+i];
      }
    });

  $env.registerNative("java.lang.System$ :: identityHashCode(java.lang.Object):scala.Int", function(obj) {
    // TODO
    return 42;
  });

  $env.registerNative("java.lang.System$ :: halt0(scala.Int):scala.Unit", function(exitCode) {
    // Well, it is not possible to implement this, right?
    throw new this.classes["java.lang.SecurityException"].type()[
      "<init>(java.lang.String):java.lang.SecurityException"](
        this.makeNativeStrWrapper(
          "Cannot terminate a JavaScript program"));
  });

  // java.lang.Runtime

  $env.registerNative("java.lang.Runtime$ :: gc():scala.Unit", function() {
    // Ignore
  });

  // java.lang.reflect.Array

  $env.registerNative("java.lang.reflect.Array$ :: newArray(java.lang.Class,scala.Int):java.lang.Object", function(componentType, length) {
    return $env.newArrayObject(componentType.$data.array, [length]);
  });

  $env.registerNative("java.lang.reflect.Array$ :: multiNewArray(java.lang.Class,scala.Int[]):java.lang.Object", function(componentType, lengths) {
    lengths = lengths.underlying;
    var arrayClassData = componentType.$data;
    for (var i = 0; i < lengths.length; i++)
      arrayClassData = arrayClassData.array;
    return $env.newArrayObject(arrayClassData, lengths);
  });

  $env.registerNative("java.lang.reflect.Array$ :: getLength(java.lang.Object):scala.Int", function(array) {
    return array.length();
  });

  function registerArray_getX_setX(elementShortName) {
    if (elementShortName === "Object") {
      var functionNameSuffix = "";
      var elementTypeName = "java.lang.Object";
    } else {
      var functionNameSuffix = elementShortName;
      var elementTypeName = "scala."+elementShortName;
    }

    $env.registerNative("java.lang.reflect.Array$ :: get"+functionNameSuffix+"(java.lang.Object,scala.Int):"+elementTypeName), function(array, index) {
      return array.get(index);
    }

    $env.registerNative("java.lang.reflect.Array$ :: set"+functionNameSuffix+"(java.lang.Object,scala.Int,"+elementTypeName+"):scala.Unit"), function(array, index, value) {
      array.set(index, value);
    }
  }

  registerArray_getX_setX("Object");
  registerArray_getX_setX("Boolean");
  registerArray_getX_setX("Char");
  registerArray_getX_setX("Byte");
  registerArray_getX_setX("Short");
  registerArray_getX_setX("Int");
  registerArray_getX_setX("Long");
  registerArray_getX_setX("Float");
  registerArray_getX_setX("Double");

  // java.lang.StandardOutPrintStream$

  $env.registerNative("java.lang.StandardOutPrintStream$ :: writeString(java.lang.String):scala.Unit", function(x) {
    console.log("out: " + x.toNativeString());
  });

  // java.lang.StandardErrPrintStream$

  $env.registerNative("java.lang.StandardErrPrintStream$ :: writeString(java.lang.String):scala.Unit", function(x) {
    console.log("err: " + x.toNativeString());
  });

  // JavaScript Dynamic objects

  function unboxJSValue(boxed) {
    if (boxed === null)
      return null;
    else
      return boxed["$jsfield$underlying "];
  }

  function boxJSValue(unboxed) {
    if (unboxed === null)
      return null;
    else {
      return new $env.classes["scala.js.JavaScriptObject"].type()[
        "<init>(java.lang.Object):scala.js.JavaScriptObject"](unboxed);
    }
  }

  function unboxJSValueSeq(boxedSeq) {
    var size = boxedSeq["size():scala.Int"]();
    var result = new Array(size);
    for (var i = 0; i < size; i++) {
      result[i] = unboxJSValue(boxedSeq["apply(scala.Int):java.lang.Object"](i));
    }
    return result;
  }

  $env.registerNative("scala.js.JavaScriptObject :: applyDynamic(java.lang.String,scala.collection.Seq):scala.js.JavaScriptObject", function(fun, argsSeq) {
    var thisValue = unboxJSValue(this);
    var args = unboxJSValueSeq(argsSeq);
    var method = thisValue[fun.toNativeString()];
    return boxJSValue(method.apply(thisValue, args));
  });

  $env.registerNative("scala.js.JavaScriptObject :: selectDynamic(java.lang.String):scala.js.JavaScriptObject", function(property) {
    return boxJSValue(unboxJSValue(this)[property.toNativeString()]);
  });

  $env.registerNative("scala.js.JavaScriptObject :: updateDynamic(java.lang.String,scala.js.JavaScriptObject):scala.Unit", function(property, value) {
    unboxJSValue(this)[property.toNativeString()] = unboxJSValue(value);
  });

  $env.registerNative("scala.js.JavaScriptObject :: apply(scala.collection.Seq):scala.js.JavaScriptObject", function(argsSeq) {
    var thisValue = unboxJSValue(this);
    var args = unboxJSValueSeq(argsSeq);
    return boxJSValue(thisValue.apply(window, args));
  });

  $env.registerNative("scala.js.JavaScriptObject :: toString():java.lang.String", function() {
    return $env.makeNativeStrWrapper(unboxJSValue(this).toString());
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: window():scala.js.JavaScriptObject", function() {
    return boxJSValue(window);
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: fromBoolean(scala.Boolean):scala.js.JavaScriptObject", function(value) {
    return boxJSValue(value);
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: fromInt(scala.Int):scala.js.JavaScriptObject", function(value) {
    return boxJSValue(value);
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: fromString(java.lang.String):scala.js.JavaScriptObject", function(value) {
    return boxJSValue(value.toNativeString());
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: fromObject(java.lang.Object):scala.js.JavaScriptObject", function(value) {
    return boxJSValue(value);
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: toBoolean(scala.js.JavaScriptObject):scala.Boolean", function(value) {
    var result = unboxJSValue(value);
    if (typeof(result) !== 'boolean')
      throw "not a boolean";
    return result;
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: toInt(scala.js.JavaScriptObject):scala.Int", function(value) {
    var result = unboxJSValue(value);
    if (typeof(result) !== 'number')
      throw "not an integer";
    return Math.round(result);
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: toString(scala.js.JavaScriptObject):java.lang.String", function(value) {
    var result = unboxJSValue(value);
    if (typeof(result) !== 'string')
      throw "not a string";
    return $env.makeNativeStrWrapper(result);
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: toObject(scala.js.JavaScriptObject):java.lang.Object", function(value) {
    var result = unboxJSValue(value);
    if ((typeof(result) !== 'object') || (result.$classData === undefined))
      throw "not a Scala object";
    return result;
  });
})($ScalaJSEnvironment);
