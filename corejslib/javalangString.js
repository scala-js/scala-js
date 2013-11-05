/* Scala.js runtime support
 * Copyright 2013 LAMP/EPFL
 * Author: Sébastien Doeraene
 */

/* ------------------
 * java.lang.String
 * ------------------ */

/* java.lang.String does not have an actual implementation as a Scala.js
 * class, because strings are encoded as JS strings.
 * Hence, we only define data and instance tests.
 */

ScalaJS.is.java_lang_String = (function(obj) {
  return typeof(obj) === "string"
});

ScalaJS.as.java_lang_String = (function(obj) {
  if (typeof(obj) === "string" || obj === null) {
    return obj
  } else {
    ScalaJS.throwClassCastException(obj, "java.lang.String")
  }
});

ScalaJS.isArrayOf.java_lang_String = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.java_lang_String)))
});

ScalaJS.asArrayOf.java_lang_String = (function(obj, depth) {
  if ((ScalaJS.isArrayOf.java_lang_String(obj, depth) || (obj === null))) {
    return obj
  } else {
    ScalaJS.throwArrayCastException(obj, "Ljava.lang.String;", depth)
  }
});

ScalaJS.data.java_lang_String =
  new ScalaJS.ClassTypeData(
    {java_lang_String:0},
    false, "java.lang.String", ScalaJS.data.java_lang_Object,
    {
      java_lang_String: true,
      java_io_Serializable: true,
      java_lang_CharSequence: true,
      java_lang_Comparable: true,
      java_lang_Object: true
    },
    ScalaJS.is.java_lang_String);
