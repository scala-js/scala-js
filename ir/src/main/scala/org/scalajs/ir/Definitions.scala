/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.ir

import Types._

object Definitions {
  /** `java.lang.Object`, the root of the class hierarchy. */
  val ObjectClass = "O"

  // Primitive "classes"
  val VoidClass = "V"
  val BooleanClass = "Z"
  val CharClass = "C"
  val ByteClass = "B"
  val ShortClass = "S"
  val IntClass = "I"
  val LongClass = "J"
  val FloatClass = "F"
  val DoubleClass = "D"
  val NullClass = "N"
  val NothingClass = "E" // think "the Empty type", or "throws an Exception"

  /** The set of all primitive classes. */
  val PrimitiveClasses: Set[String] = Set(
      VoidClass,
      BooleanClass,
      CharClass,
      ByteClass,
      ShortClass,
      IntClass,
      LongClass,
      FloatClass,
      DoubleClass,
      NullClass,
      NothingClass
  )

  // Hijacked classes
  val BoxedUnitClass = "sr_BoxedUnit"
  val BoxedBooleanClass = "jl_Boolean"
  val BoxedCharacterClass = "jl_Character"
  val BoxedByteClass = "jl_Byte"
  val BoxedShortClass = "jl_Short"
  val BoxedIntegerClass = "jl_Integer"
  val BoxedLongClass = "jl_Long"
  val BoxedFloatClass = "jl_Float"
  val BoxedDoubleClass = "jl_Double"
  val BoxedStringClass = "T"

  /** The set of all hijacked classes. */
  val HijackedClasses: Set[String] = Set(
      BoxedUnitClass,
      BoxedBooleanClass,
      BoxedCharacterClass,
      BoxedByteClass,
      BoxedShortClass,
      BoxedIntegerClass,
      BoxedLongClass,
      BoxedFloatClass,
      BoxedDoubleClass,
      BoxedStringClass
  )

  /** The class of things returned by `ClassOf` and `GetClass`. */
  val ClassClass = "jl_Class"

  /** Name of the static initializer method. */
  final val StaticInitializerName = "clinit___"

  /** Encodes a class name. */
  def encodeClassName(fullName: String): String = {
    val base = fullName.replace("_", "$und").replace(".", "_")
    compressedClasses.getOrElse(base, {
      compressedPrefixes.collectFirst {
        case (prefix, compressed) if base.startsWith(prefix) =>
          compressed + base.substring(prefix.length)
      } getOrElse {
        "L" + base
      }
    })
  }

  // !!! Duplicate logic: this code must be in sync with runtime.StackTrace

  /** Decodes a class name encoded with [[encodeClassName]]. */
  def decodeClassName(encodedName: String): String = {
    val base = decompressedClasses.getOrElse(encodedName, {
      decompressedPrefixes.collectFirst {
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
      "java_lang_String" -> "T"
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

  /** Decodes a method name into its full signature.
   *
   *  This discards the information whether the method is private or not, and
   *  at which class level it is private. If necessary, you can recover that
   *  information from `encodedName.indexOf("__p") >= 0`.
   */
  def decodeMethodName(
      encodedName: String): (String, List[TypeRef], Option[TypeRef]) = {
    val (simpleName, privateAndSigString) = if (isConstructorName(encodedName)) {
      val privateAndSigString =
        if (encodedName == "init___") ""
        else encodedName.stripPrefix("init___") + "__"
      ("<init>", privateAndSigString)
    } else if (encodedName == StaticInitializerName) {
      ("<clinit>", "")
    } else {
      val pos = encodedName.indexOf("__")
      val pos2 =
        if (!encodedName.substring(pos + 2).startsWith("p")) pos
        else encodedName.indexOf("__", pos + 2)
      (encodedName.substring(0, pos), encodedName.substring(pos2 + 2))
    }

    // -1 preserves trailing empty strings
    val parts = privateAndSigString.split("__", -1).toSeq
    val paramsAndResultStrings =
      if (parts.headOption.exists(_.startsWith("p"))) parts.tail
      else parts

    val paramStrings :+ resultString = paramsAndResultStrings

    val paramTypes = paramStrings.map(decodeTypeRef).toList
    val resultType =
      if (resultString == "") None // constructor or reflective proxy
      else Some(decodeTypeRef(resultString))

    (simpleName, paramTypes, resultType)
  }

  /** Decodes a [[Types.TypeRef]], such as in an encoded method signature.
   */
  def decodeTypeRef(encodedName: String): TypeRef = {
    val arrayDepth = encodedName.indexWhere(_ != 'A')
    if (arrayDepth == 0)
      ClassRef(encodedName)
    else
      ArrayTypeRef(encodedName.substring(arrayDepth), arrayDepth)
  }

  /* Common predicates on encoded names */

  def isConstructorName(name: String): Boolean =
    name.startsWith("init___")

  def isReflProxyName(name: String): Boolean = {
    name.endsWith("__") &&
    !isConstructorName(name) &&
    name != StaticInitializerName
  }

}
