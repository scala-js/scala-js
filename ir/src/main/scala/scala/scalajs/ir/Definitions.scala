/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

object Definitions {
  val ObjectClass = "O"
  val ClassClass  = "jl_Class"

  val StringClass = "T"

  val BoxedUnitClass      = "sr_BoxedUnit"
  val BoxedBooleanClass   = "jl_Boolean"
  val BoxedCharacterClass = "jl_Character"
  val BoxedByteClass      = "jl_Byte"
  val BoxedShortClass     = "jl_Short"
  val BoxedIntegerClass   = "jl_Integer"
  val BoxedLongClass      = "jl_Long"
  val BoxedFloatClass     = "jl_Float"
  val BoxedDoubleClass    = "jl_Double"

  val CharSequenceClass = "jl_CharSequence"
  val SerializableClass = "Ljava_io_Serializable"
  val ComparableClass   = "jl_Comparable"
  val NumberClass       = "jl_Number"

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

  val RuntimeLongClass = "sjsr_RuntimeLong"

  /** Encodes a class name. */
  def encodeClassName(fullName: String): String = {
    val base = fullName.replace("_", "$und").replace(".", "_")
    val encoded = compressedClasses.getOrElse(base, {
      compressedPrefixes collectFirst {
        case (prefix, compressed) if base.startsWith(prefix) =>
          compressed + base.substring(prefix.length)
      } getOrElse {
        "L"+base
      }
    })
    if (Trees.isKeyword(encoded) || encoded.charAt(0).isDigit ||
        encoded.charAt(0) == '$') {
      "$" + encoded
    } else encoded
  }

  /** Decodes a class name encoded with [[encodeClassName]]. */
  def decodeClassName(encodedName: String): String = {
    val encoded =
      if (encodedName.charAt(0) == '$') encodedName.substring(1)
      else encodedName
    val base = decompressedClasses.getOrElse(encoded, {
      decompressedPrefixes collectFirst {
        case (prefix, decompressed) if encodedName.startsWith(prefix) =>
          decompressed + encodedName.substring(prefix.length)
      } getOrElse {
        assert(!encodedName.isEmpty && encodedName.charAt(0) == 'L',
            s"Cannot decode invalid encoded name '$encodedName'")
        encodedName.substring(1)
      }
    })
    base.replace("_", ".").replace("$und", "_")
  }

  private val compressedClasses: Map[String, String] = Map(
      "java_lang_Object" -> "O",
      "java_lang_String" -> "T",
      "scala_Unit" -> "V",
      "scala_Boolean" -> "Z",
      "scala_Char" -> "C",
      "scala_Byte" -> "B",
      "scala_Short" -> "S",
      "scala_Int" -> "I",
      "scala_Long" -> "J",
      "scala_Float" -> "F",
      "scala_Double" -> "D"
  ) ++ (
      for (index <- 2 to 22)
        yield s"scala_Tuple$index" -> ("T"+index)
  ) ++ (
      for (index <- 0 to 22)
        yield s"scala_Function$index" -> ("F"+index)
  )

  private val decompressedClasses: Map[String, String] =
    compressedClasses map { case (a, b) => (b, a) }

  private val compressedPrefixes = Seq(
      "scala_scalajs_runtime_" -> "sjsr_",
      "scala_scalajs_" -> "sjs_",
      "scala_collection_immutable_" -> "sci_",
      "scala_collection_mutable_" -> "scm_",
      "scala_collection_generic_" -> "scg_",
      "scala_collection_" -> "sc_",
      "scala_runtime_" -> "sr_",
      "scala_" -> "s_",
      "java_lang_" -> "jl_",
      "java_util_" -> "ju_"
  )

  private val decompressedPrefixes: Seq[(String, String)] =
    compressedPrefixes map { case (a, b) => (b, a) }
}
