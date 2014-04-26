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

ScalaJS.is.T = (function(obj) {
  return typeof(obj) === "string"
});

ScalaJS.as.T = (function(obj) {
  if (typeof(obj) === "string" || obj === null) {
    return obj
  } else {
    ScalaJS.throwClassCastException(obj, "java.lang.String")
  }
});

ScalaJS.isArrayOf.T = (function(obj, depth) {
  return (!(!(((obj && obj.$classData) && (obj.$classData.arrayDepth === depth)) && obj.$classData.arrayBase.ancestors.T)))
});

ScalaJS.asArrayOf.T = (function(obj, depth) {
  if ((ScalaJS.isArrayOf.T(obj, depth) || (obj === null))) {
    return obj
  } else {
    ScalaJS.throwArrayCastException(obj, "Ljava.lang.String;", depth)
  }
});

ScalaJS.d.T =
  new ScalaJS.ClassTypeData(
    {T:0},
    false, "java.lang.String", ScalaJS.d.O,
    {
      T: 1,
      Ljava_io_Serializable: 1,
      jl_CharSequence: 1,
      jl_Comparable: 1,
      O: 1
    },
    ScalaJS.is.T);
