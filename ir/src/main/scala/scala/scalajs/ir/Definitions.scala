/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

object Definitions {
  val ObjectClass = "java_lang_Object"
  val ClassClass  = "java_lang_Class"

  val StringClass = "java_lang_String"

  val BoxedUnitClass      = "scala_runtime_BoxedUnit"
  val BoxedBooleanClass   = "java_lang_Boolean"
  val BoxedCharacterClass = "java_lang_Character"
  val BoxedByteClass      = "java_lang_Byte"
  val BoxedShortClass     = "java_lang_Short"
  val BoxedIntegerClass   = "java_lang_Integer"
  val BoxedLongClass      = "java_lang_Long"
  val BoxedFloatClass     = "java_lang_Float"
  val BoxedDoubleClass    = "java_lang_Double"

  val CharSequenceClass = "java_lang_CharSequence"
  val SerializableClass = "java_io_Serializable"
  val ComparableClass   = "java_lang_Comparable"
  val NumberClass       = "java_lang_Number"

  val HijackedBoxedClasses = Set(
      BoxedUnitClass, BoxedBooleanClass, BoxedByteClass, BoxedShortClass,
      BoxedIntegerClass, BoxedLongClass, BoxedFloatClass, BoxedDoubleClass)
  val HijackedClasses =
    HijackedBoxedClasses + StringClass

  val AncestorsOfStringClass = Set(
      CharSequenceClass, ComparableClass, SerializableClass)
  val AncestorsOfHijackedNumberClasses = Set(
      NumberClass, ComparableClass, SerializableClass)
  val AncestorsOfBoxedBooleanClass = Set(
      ComparableClass, SerializableClass)

  val AncestorsOfHijackedClasses =
    AncestorsOfStringClass ++ AncestorsOfHijackedNumberClasses ++
    AncestorsOfBoxedBooleanClass

  val RuntimeLongClass = "scala_scalajs_runtime_RuntimeLong"

  def decodeClassName(encodedName: String): String = {
    encodedName.replace("_", ".").replace("$und", "_")
  }
}
