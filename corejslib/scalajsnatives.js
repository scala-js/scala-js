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

  function scalaValueToJSValue(scalaValue) {
    if (scalaValue === null)
      return null;

    var classData = scalaValue.$classData;
    var className = classData.name;

    switch (className) {
      case 'java.lang.Boolean':
        return $env.modules["scala.Boolean$"].instance["unbox(java.lang.Boolean):scala.Boolean"](scalaValue);

      case 'java.lang.Integer':
        return $env.modules["scala.Int$"].instance["box(java.lang.Int):scala.Integer"](scalaValue);

      // TODO Other boxed values

      case 'java.lang.String':
        return scalaValue.toNativeString();

      default:
        if ($env.isInstance(scalaValue, "scala.js.JavaScriptObject"))
          return scalaValue["$jsfield$underlying "];
        else
          return scalaValue;
    }
  }

  function jsValueToScalaValue(jsValue) {
    switch (typeof(jsValue)) {
      case 'undefined':
        return $env.modules["scala.runtime.BoxedUnit$"].instance[
          "UNIT():scala.runtime.BoxedUnit"]();

      case 'null':
        return null;

      case 'boolean':
        return $env.modules["scala.Boolean$"].instance["box(scala.Boolean):java.lang.Boolean"](jsValue);

      case 'number':
        // TODO How do we handle floats, shorts, etc.?
        return $env.modules["scala.Int$"].instance["box(scala.Int):java.lang.Integer"](jsValue);

      case 'string':
        return $env.makeNativeStrWrapper(jsValue);

      case 'function':
        throw "jsValueToScalaValue() not implemented for 'function'";

      default:
        if (jsValue.$classData === undefined) {
          return new $env.classes["scala.js.JavaScriptObject"].type()[
            "<init>(java.lang.Object):scala.js.JavaScriptObject"](jsValue);
        } else {
          return jsValue;
        }
    }
  }

  $env.registerNative("scala.js.JavaScriptObject :: applyDynamic(java.lang.String,scala.collection.Seq):java.lang.Object", function(fun, argsSeq) {
    var thisValue = this["$jsfield$underlying "];
    var argc = argsSeq["size():scala.Int"]();
    var args = new Array(argc);
    for (var i = 0; i < argc; i++) {
      args[i] = scalaValueToJSValue(argsSeq["apply(scala.Int):java.lang.Object"](i));
    }
    var method = thisValue[fun];
    return jsValueToScalaValue(method.apply(thisValue, args));
  });

  $env.registerNative("scala.js.JavaScriptObject :: selectDynamic(java.lang.String):java.lang.Object", function(property) {
    var thisValue = this["$jsfield$underlying "];
    return jsValueToScalaValue(thisValue[property]);
  });

  $env.registerNative("scala.js.JavaScriptObject :: updateDynamic(java.lang.String,java.lang.Object):scala.Unit", function(property, value) {
    var thisValue = this["$jsfield$underlying "];
    thisValue[property] = scalaValueToJSValue(value);
  });

  $env.registerNative("scala.js.JavaScriptObject$ :: window():scala.js.JavaScriptObject", function() {
    return jsValueToScalaValue(window);
  });
})($ScalaJSEnvironment);
