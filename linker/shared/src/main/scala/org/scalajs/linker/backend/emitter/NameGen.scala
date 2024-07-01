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

package org.scalajs.linker.backend.emitter

import scala.collection.mutable

import org.scalajs.ir.{OriginalName, UTF8String}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Types._

/** Performs state independent name mangling.
 *
 *  - Converts IR names to JavaScript names.
 *  - Converts module names to JavaScript names.
 */
private[emitter] final class NameGen {
  import NameGen._

  private val genLocalNameCache = {
    /* Fill the cache with reserved JS identifiers so that we do not have to
     * deal with avoiding them in `genName`. We append '\u00f8' to their
     * translation, which is the first non-ASCII valid JS identifier start, and
     * does not collide with any other encoding performed by `genName`.
     */
    val cache = mutable.Map.empty[LocalName, String]
    for (reserved <- ReservedJSIdentifierNames)
      cache.put(LocalName(reserved), reserved + "\u00f8")
    cache
  }

  private val genLabelNameCache = {
    // Same as genLocalNameCache
    val cache = mutable.Map.empty[LabelName, String]
    for (reserved <- ReservedJSIdentifierNames)
      cache.put(LabelName(reserved), reserved + "\u00f8")
    cache
  }

  private val genSimpleFieldNameCache =
    mutable.Map.empty[SimpleFieldName, String]

  private val genMethodNameCache =
    mutable.Map.empty[MethodName, String]

  private val genClassNameCache = {
    /* Fill the cache with the compressed form of java.lang.Object,
     * java.lang.String and org.scalajs.linker.runtime.RuntimeLong, so that we
     * do not have to take care of them in genName(ClassName).
     */
    val cache = mutable.Map.empty[ClassName, String]
    cache.put(ObjectClass, "O")
    cache.put(BoxedStringClass, "T")
    cache.put(LongImpl.RuntimeLongClass, "RTLong")
    cache.put(LongImpl.RuntimeLongModuleClass, "RTLong$")
    cache
  }

  private def genNameGeneric[N <: Name](name: N,
      cache: mutable.Map[N, String]): String = {

    cache.getOrElseUpdate(name, {
      val encoded = name.encoded
      val len = encoded.length
      val result = new Array[Char](len)
      result(0) = startByteToChar(encoded(0) & 0xff)
      var i = 1
      while (i != len) {
        val b = encoded(i) & 0xff
        if (b == '_' && encoded(i - 1) == '_')
          result(i) = FullwidthSpacingUnderscore
        else
          result(i) = partByteToChar(b)
        i += 1
      }
      new String(result)
    })
  }

  def genName(name: LocalName): String = {
    genLocalNameCache.getOrElseUpdate(name, {
      val encoded = name.encoded
      val len = encoded.length
      val result = new Array[Char](len)
      result(0) = localStartByteToChar(encoded(0) & 0xff)
      var i = 1
      while (i != len) {
        val b = encoded(i) & 0xff
        if (b == '_' && encoded(i - 1) == '_')
          result(i) = FullwidthSpacingUnderscore
        else
          result(i) = partByteToChar(b)
        i += 1
      }
      new String(result)
    })
  }

  def genName(name: LabelName): String = genNameGeneric(name, genLabelNameCache)
  def genName(name: SimpleFieldName): String = genNameGeneric(name, genSimpleFieldNameCache)

  def genName(name: FieldName): String =
    genName(name.className) + "__f_" + genName(name.simpleName)

  def genName(name: MethodName): String = {
    genMethodNameCache.getOrElseUpdate(name, {
      val builder = new java.lang.StringBuilder()

      // First encode the simple name
      if (name.isConstructor) {
        // No encoded name is emitted. Param type refs are enough
      } else if (name.isStaticInitializer) {
        builder.append("stinit__")
      } else if (name.isClassInitializer) {
        builder.append("clinit__")
      } else {
        val encoded = name.simpleName.encoded
        builder.append(startByteToChar(encoded(0) & 0xff))
        val len = encoded.length
        var i = 1
        while (i != len) {
          val b = encoded(i) & 0xff
          // Avoid '__' in the output as that must be the end of the simple name
          if (b == '_' && encoded(i - 1) == '_')
            builder.append(FullwidthSpacingUnderscore)
          else
            builder.append(partByteToChar(b))
          i += 1
        }
        builder.append('_').append('_')
      }

      def appendTypeRef(typeRef: TypeRef): Unit = {
        typeRef match {
          case PrimRef(tpe) =>
            tpe match {
              case NoType      => builder.append('V')
              case BooleanType => builder.append('Z')
              case CharType    => builder.append('C')
              case ByteType    => builder.append('B')
              case ShortType   => builder.append('S')
              case IntType     => builder.append('I')
              case LongType    => builder.append('J')
              case FloatType   => builder.append('F')
              case DoubleType  => builder.append('D')
              case NullType    => builder.append('N')
              case NothingType => builder.append('E')
            }
          case ClassRef(className) =>
            builder.append(genName(className))
          case ArrayTypeRef(base, dimensions) =>
            var i = 0
            while (i != dimensions) {
              builder.append('A')
              i += 1
            }
            appendTypeRef(base)
          case ClosureTypeRef(paramTypeRefs, resultTypeRef) =>
            builder.append('c')
            builder.append(paramTypeRefs.size)
            for (paramTypeRef <- paramTypeRefs) {
              builder.append('_').append('_')
              appendTypeRef(paramTypeRef)
            }
            builder.append('_').append('_')
            appendTypeRef(resultTypeRef)
        }
      }

      for (typeRef <- name.paramTypeRefs) {
        appendTypeRef(typeRef)
        builder.append('_').append('_')
      }

      if (!name.isConstructor && !name.isStaticInitializer &&
          !name.isClassInitializer && !name.isReflectiveProxy) {
        appendTypeRef(name.resultTypeRef)
      }

      builder.toString()
    })
  }

  def genName(name: ClassName): String = {
    genClassNameCache.getOrElseUpdate(name, {
      val encoded = name.encoded
      val len = encoded.length
      val builder = new java.lang.StringBuilder(len + 1)

      // Handle compressed prefixes
      var i = compressedPrefixes.find(pair => encodedNameStartsWith(encoded, pair._1, 0)) match {
        case None =>
          builder.append('L')
          0
        case Some(pair) =>
          builder.append(pair._2)
          pair._1.length
      }

      // Encode the rest
      while (i != len) {
        builder.append(classByteToChar(encoded(i) & 0xff))
        i += 1
      }

      builder.toString()
    })
  }

  def genOriginalName(name: Name, originalName: OriginalName,
      jsName: String): OriginalName = {
    genOriginalName(name.encoded, originalName, jsName)
  }

  def genOriginalName(name: FieldName, originalName: OriginalName,
      jsName: String): OriginalName = {
    genOriginalName(name.simpleName, originalName, jsName)
  }

  def genOriginalName(name: MethodName, originalName: OriginalName,
      jsName: String): OriginalName = {
    genOriginalName(name.simpleName, originalName, jsName)
  }

  private def genOriginalName(name: UTF8String, originalName: OriginalName,
      jsName: String): OriginalName = {

    def sameName: Boolean = {
      /* This method compares a UTF-8 string and a (UTF-16) string
       * element-wise, thus comparing bytes with chars, in order to avoid any
       * recoding. We can do this here because:
       *
       * - for ASCII characters, the byte and char values are the same
       * - for non-ASCII characters, the byte value is always negative while
       *   the char values are always positive, so the comparison is always
       *   false
       * - the always-false result for non-ASCII characters is correct in the
       *   case of `JSGen`, because all non-ASCII code points in `Name`s are
       *   encoded in `genName()` byte-by-byte into Chars that have lost all
       *   connection to what they meant, so any non-ASCII character will
       *   require an original name to be generated.
       */

      // scalastyle:off return
      if (name.length != jsName.length())
        return false
      var i = 0
      while (i != name.length) {
        if (name(i).toInt != jsName.charAt(i).toInt)
          return false
        i += 1
      }
      true
      // scalastyle:on return
    }

    if (originalName.isDefined) originalName
    else if (sameName) NoOriginalName
    else OriginalName(name)
  }

  def genModuleName(module: String): String = {
    /* This is written so that the happy path, when `module` contains only
     * valid characters, is fast.
     */

    def isValidChar(c: Char): Boolean =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    def containsOnlyValidChars(): Boolean = {
      // scalastyle:off return
      val len = module.length
      var i = 0
      while (i != len) {
        if (!isValidChar(module.charAt(i)))
          return false
        i += 1
      }
      true
      // scalastyle:on return
    }

    def buildValidName(): String = {
      val result = new java.lang.StringBuilder()
      val len = module.length
      var i = 0
      while (i != len) {
        val c = module.charAt(i)
        if (isValidChar(c))
          result.append(c)
        else
          result.append("$%04x".format(c.toInt))
        i += 1
      }
      result.toString()
    }

    if (containsOnlyValidChars()) module
    else buildValidName()
  }
}

private[emitter] object NameGen {

  private final val FullwidthSpacingUnderscore = '\uff3f'
  private final val GreekSmallLetterDelta = '\u03b4'

  private val startByteToChar: Array[Char] = {
    /* The code points 256 through (512 - 1) are all valid start characters for
     * JavaScript identifiers. We encode each invalid byte as one of those
     * characters. Valid ASCII start chars are encoded as themselves
     */
    val table = Array.tabulate(256)(i => (256 + i).toChar)
    for (b <- 'A'.toInt to 'Z'.toInt)
      table(b) = b.toChar
    for (b <- 'a'.toInt to 'z'.toInt)
      table(b) = b.toChar
    table('$'.toInt) = '$'
    table('_'.toInt) = '_'
    table
  }

  private val partByteToChar: Array[Char] = {
    /* Once not at the start anymore, ASCII digits are also encoded as
     * themselves.
     */
    val table = startByteToChar.clone()
    for (b <- '0'.toInt to '9'.toInt)
      table(b) = b.toChar
    table
  }

  private val localStartByteToChar: Array[Char] = {
    /* Local variables must not start with a '$', otherwise they might clash
     * with compiler-generated variables such as codegenVars or the `$thiz` of
     * default methods (#2972).
     * We rewrite a starting '$' as '\u03b4' (greek small letter delta), which
     * is an arbitrary choice based on the sound ('dollar' starts with the
     * sound of delta).
     */
    val table = startByteToChar.clone()
    table('$'.toInt) = GreekSmallLetterDelta
    table
  }

  private val classByteToChar: Array[Char] = {
    /* For class names, '.' are rewritten as '_' and '_' as '\uff3f'. We can
     * use the 'part' table even for start characters because class names are
     * always prefixed by something in our encoding.
     */
    val table = partByteToChar.clone()
    table('.'.toInt) = '_'
    table('_'.toInt) = FullwidthSpacingUnderscore
    table
  }

  /** Set of identifier names that cannot or should not be used for variable
   *  names.
   *
   *  This set includes and is limited to:
   *
   *  - All ECMAScript 2015 keywords;
   *  - Identifier names that are treated as keywords in ECMAScript 2015
   *    Strict Mode;
   *  - Identifier names that are treated as keywords in some contexts, such as
   *    ES modules;
   *  - The identifiers `arguments` and `eval`, because they cannot be used for
   *    local variable names in ECMAScript 2015 Strict Mode;
   *  - The identifier `undefined`, because that's way too confusing if it does
   *    not actually mean `void 0`, and who knows what JS engine performance
   *    cliffs we can trigger with that.
   */
  private[emitter] final val ReservedJSIdentifierNames: Set[String] = Set(
      "arguments", "await", "break", "case", "catch", "class", "const",
      "continue", "debugger", "default", "delete", "do", "else", "enum",
      "eval", "export", "extends", "false", "finally", "for", "function", "if",
      "implements", "import", "in", "instanceof", "interface", "let", "new",
      "null", "package", "private", "protected", "public", "return", "static",
      "super", "switch", "this", "throw", "true", "try", "typeof", "undefined",
      "var", "void", "while", "with", "yield"
  )

  private val compressedPrefixes: List[(UTF8String, String)] = {
    List(
        "java.lang." -> "jl_",
        "java.util." -> "ju_",
        "scala.collection.immutable." -> "sci_",
        "scala.collection.mutable." -> "scm_",
        "scala.collection.generic." -> "scg_",
        "scala.collection." -> "sc_",
        "scala.runtime." -> "sr_",
        "scala.scalajs.runtime." -> "sjsr_",
        "scala.scalajs." -> "sjs_",
        "scala.Function" -> "F",
        "scala.Tuple" -> "T",
        "scala." -> "s_"
    ).map { pair =>
      UTF8String(pair._1) -> pair._2
    }
  }

  private def encodedNameStartsWith(encoded: UTF8String, prefix: UTF8String,
      start: Int): Boolean = {
    // scalastyle:off return
    val prefixLen = prefix.length
    if (start + prefixLen > encoded.length)
      return false
    var i = 0
    while (i != prefixLen) {
      if (encoded(start + i) != prefix(i))
        return false
      i += 1
    }
    true
    // scalastyle:on return
  }

}
