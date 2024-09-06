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

package java.util.regex

import scala.annotation.{switch, tailrec}

import java.lang.Character.{
  charCount,
  isBmpCodePoint,
  highSurrogate,
  lowSurrogate,
  MIN_HIGH_SURROGATE,
  MAX_HIGH_SURROGATE,
  MIN_LOW_SURROGATE,
  MAX_LOW_SURROGATE
}

import java.lang.Utils._
import java.util.ScalaOps._

import scala.scalajs.js
import scala.scalajs.js.JSStringOps.enableJSStringOps
import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

/** Compiler from Java regular expressions to JavaScript regular expressions.
 *
 *  See `README.md` in this directory for the design.
 *
 *  !!! PLEASE (re-)read the README before modifying this class. !!!
 *
 *  There are very intricate concerns that are cross-cutting all over the
 *  class, and assumptions are not local!
 */
private[regex] object PatternCompiler {
  import Pattern._

  def compile(regex: String, flags: Int): Pattern =
    new PatternCompiler(regex, flags).compile()

  /** RegExp to match leading embedded flag specifiers in a pattern.
   *
   *  E.g. (?u), (?-i), (?U-i)
   */
  private val leadingEmbeddedFlagSpecifierRegExp =
    new js.RegExp("^\\(\\?([idmsuxU]*)(?:-([idmsuxU]*))?\\)")

  /** RegExp to renumber backreferences (used for possessive quantifiers). */
  private val renumberingRegExp =
    new js.RegExp("(\\\\+)(\\d+)", "g")

  /** Returns the flag that corresponds to an embedded flag specifier. */
  private def charToFlag(c: Char): Int = (c: @switch) match {
    case 'i' => CASE_INSENSITIVE
    case 'd' => UNIX_LINES
    case 'm' => MULTILINE
    case 's' => DOTALL
    case 'u' => UNICODE_CASE
    case 'x' => COMMENTS
    case 'U' => UNICODE_CHARACTER_CLASS
    case _   => throw new IllegalArgumentException("bad in-pattern flag")
  }

  private def featureTest(flags: String): Boolean = {
    try {
      new js.RegExp("", flags)
      true
    } catch {
      case _: Throwable =>
        false
    }
  }

  /** Cache for `Support.supportsUnicode`. */
  private val _supportsUnicode =
    (LinkingInfo.esVersion >= ESVersion.ES2015) || featureTest("u")

  /** Cache for `Support.supportsSticky`. */
  private val _supportsSticky =
    (LinkingInfo.esVersion >= ESVersion.ES2015) || featureTest("y")

  /** Cache for `Support.supportsDotAll`. */
  private val _supportsDotAll =
    (LinkingInfo.esVersion >= ESVersion.ES2018) || featureTest("us")

  /** Cache for `Support.supportsIndices`. */
  private val _supportsIndices =
    featureTest("d")

  /** Feature-test methods.
   *
   *  They are located in a separate object so that the methods can be fully
   *  inlined and optimized away, without leaving a `LoadModule` of the
   *  enclosing object behind, depending on the target ES version.
   */
  private[regex] object Support {
    /** Tests whether the underlying JS RegExp supports the 'u' flag. */
    @inline
    def supportsUnicode: Boolean =
      (LinkingInfo.esVersion >= ESVersion.ES2015) || _supportsUnicode

    /** Tests whether the underlying JS RegExp supports the 'y' flag. */
    @inline
    def supportsSticky: Boolean =
      (LinkingInfo.esVersion >= ESVersion.ES2015) || _supportsSticky

    /** Tests whether the underlying JS RegExp supports the 's' flag. */
    @inline
    def supportsDotAll: Boolean =
      (LinkingInfo.esVersion >= ESVersion.ES2018) || _supportsDotAll

    /** Tests whether the underlying JS RegExp supports the 'd' flag. */
    @inline
    def supportsIndices: Boolean =
      _supportsIndices

    /** Tests whether features requiring support for the 'u' flag are enabled.
     *
     *  They are enabled if and only if the project is configured to rely on
     *  ECMAScript 2015 features.
     */
    @inline
    def enableUnicodeCaseInsensitive: Boolean =
      LinkingInfo.esVersion >= ESVersion.ES2015

    /** Tests whether features requiring \p{} and/or look-behind assertions are enabled.
     *
     *  They are enabled if and only if the project is configured to rely on
     *  ECMAScript 2018 features.
     */
    @inline
    def enableUnicodeCharacterClassesAndLookBehinds: Boolean =
      LinkingInfo.esVersion >= ESVersion.ES2018
  }

  import Support._

  // Helpers to deal with surrogate pairs when the 'u' flag is not supported

  private def codePointNotAmong(characters: String): String = {
    if (supportsUnicode) {
      if (characters != "")
        "[^" + characters + "]"
      else if (supportsDotAll)
        "." // we always add the 's' flag when it is supported, so we can use "." here
      else
        "[\\d\\D]" // In theory, "[^]" works, but XRegExp does not trust JS engines on that, so we don't either
    } else {
      val highCharRange = s"$MIN_HIGH_SURROGATE-$MAX_HIGH_SURROGATE"
      val lowCharRange = s"$MIN_LOW_SURROGATE-$MAX_LOW_SURROGATE"
      val highCPOrSupplementaryCP = s"[$highCharRange](?:[$lowCharRange]|(?![$lowCharRange]))"
      s"(?:[^$characters$highCharRange]|$highCPOrSupplementaryCP)"
    }
  }

  // Other helpers

  /** Helpers that are always inlined; kept in a separate object so that they
   *  can be inlined without cost.
   */
  private object InlinedHelpers {
    /* isHighSurrogateCP, isLowSurrogateCP and toCodePointCP are like the
     * non-CP equivalents in Character, but they take Int code point
     * parameters. The implementation strategy is the same as the methods for
     * Chars. The magical constants are copied from Character and extended to
     * 32 bits.
     */

    private final val HighSurrogateCPMask     = 0xfffffc00 // ffff  111111 00  00000000
    private final val HighSurrogateCPID       = 0x0000d800 // 0000  110110 00  00000000
    private final val LowSurrogateCPMask      = 0xfffffc00 // ffff  111111 00  00000000
    private final val LowSurrogateCPID        = 0x0000dc00 // 0000  110111 00  00000000
    private final val SurrogateUsefulPartMask = 0x000003ff // 0000  000000 11  11111111

    private final val HighSurrogateShift = 10
    private final val HighSurrogateAddValue = 0x10000 >> HighSurrogateShift

    @inline def isHighSurrogateCP(cp: Int): Boolean =
      (cp & HighSurrogateCPMask) == HighSurrogateCPID

    @inline def isLowSurrogateCP(cp: Int): Boolean =
      (cp & LowSurrogateCPMask) == LowSurrogateCPID

    @inline def toCodePointCP(high: Int, low: Int): Int = {
      (((high & SurrogateUsefulPartMask) + HighSurrogateAddValue) << HighSurrogateShift) |
        (low & SurrogateUsefulPartMask)
    }

    @inline def isLetter(c: Char): Boolean =
      (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')

    @inline def isDigit(c: Char): Boolean =
      c >= '0' && c <= '9'

    @inline def isLetterOrDigit(c: Char): Boolean =
      isLetter(c) || isDigit(c)

    @inline def isHexDigit(c: Char): Boolean =
      isDigit(c) || (c >= 'A' && c <= 'F') || (c >= 'a' && c <= 'f')

    @inline def parseInt(s: String, radix: Int): Int =
      js.Dynamic.global.parseInt(s, radix).asInstanceOf[Int]
  }

  import InlinedHelpers._

  private def codePointToString(codePoint: Int): String = {
    if (LinkingInfo.esVersion >= ESVersion.ES2015) {
      js.Dynamic.global.String.fromCodePoint(codePoint).asInstanceOf[String]
    } else {
      if (isBmpCodePoint(codePoint)) {
        js.Dynamic.global.String.fromCharCode(codePoint).asInstanceOf[String]
      } else {
        js.Dynamic.global.String
          .fromCharCode(highSurrogate(codePoint).toInt, lowSurrogate(codePoint).toInt)
          .asInstanceOf[String]
      }
    }
  }

  // Everything for compiling character classes

  /* This should be a sealed class with subclasses that we pattern-match on.
   * However, to cut costs in terms of code size, we use a single class with a
   * `kind` field.
   */
  private final class CompiledCharClass(val kind: Int, val data: String) {
    import CompiledCharClass._

    lazy val negated: CompiledCharClass =
      new CompiledCharClass(kind ^ 1, data)
  }

  // This object is entirely inlined and DCE'ed. Keep it that way.
  private object CompiledCharClass {
    /** Represents `\p{data}`. */
    final val PosP = 0

    /** Represents `\P{data}`. */
    final val NegP = 1

    /** Represents `[data]`. */
    final val PosClass = 2

    /** Represents `[^data]`. */
    final val NegClass = 3

    @inline def posP(name: String): CompiledCharClass =
      new CompiledCharClass(PosP, name)

    @inline def negP(name: String): CompiledCharClass =
      new CompiledCharClass(NegP, name)

    @inline def posClass(content: String): CompiledCharClass =
      new CompiledCharClass(PosClass, content)

    @inline def negClass(content: String): CompiledCharClass =
      new CompiledCharClass(NegClass, content)
  }

  private val ASCIIDigit = CompiledCharClass.posClass("0-9")
  private val UnicodeDigit = CompiledCharClass.posP("Nd")

  private val UniversalHorizontalWhiteSpace =
    CompiledCharClass.posClass("\t \u00A0\u1680\u180E\u2000-\u200A\u202F\u205F\u3000")

  private val ASCIIWhiteSpace = CompiledCharClass.posClass("\t-\r ")
  private val UnicodeWhitespace = CompiledCharClass.posP("White_Space")

  private val UniversalVerticalWhiteSpace = CompiledCharClass.posClass("\n-\r\u0085\u2028\u2029")

  private val ASCIIWordChar = CompiledCharClass.posClass("a-zA-Z_0-9")
  private val UnicodeWordChar =
    CompiledCharClass.posClass("\\p{Alphabetic}\\p{Mn}\\p{Me}\\p{Mc}\\p{Nd}\\p{Pc}\\p{Join_Control}")

  /** Mapping from POSIX character class to the character set to use when
   *  `UNICODE_CHARACTER_CLASSES` is *not* set.
   *
   *  This is a `js.Dictionary` because it can be used even when compiling to
   *  ECMAScript 5.1.
   */
  private val asciiPOSIXCharacterClasses: js.Dictionary[CompiledCharClass] = {
    import CompiledCharClass._

    val r = dictEmpty[CompiledCharClass]()
    dictSet(r, "Lower", posClass("a-z"))
    dictSet(r, "Upper", posClass("A-Z"))
    dictSet(r, "ASCII", posClass("\u0000-\u007f"))
    dictSet(r, "Alpha", posClass("A-Za-z")) // [\p{Lower}\p{Upper}]
    dictSet(r, "Digit", posClass("0-9"))
    dictSet(r, "Alnum", posClass("0-9A-Za-z")) // [\p{Alpha}\p{Digit}]
    dictSet(r, "Punct", posClass("!-/:-@[-`{-~")) // One of !"#$%&'()*+,-./:;<=>?@[\]^_`{|}~
    dictSet(r, "Graph", posClass("!-~")) // [\p{Alnum}\p{Punct}]
    dictSet(r, "Print", posClass(" -~")) // [\p{Graph}\x20]
    dictSet(r, "Blank", posClass("\t "))
    dictSet(r, "Cntrl", posClass("\u0000-\u001f\u007f"))
    dictSet(r, "XDigit", posClass("0-9A-Fa-f"))
    dictSet(r, "Space", posClass("\t-\r ")) // [ \t\n\x0B\f\r]
    r
  }

  /** Mapping of predefined character classes to the corresponding character
   *  set.
   *
   *  Mappings that also exist in `asciiPOSIXCharacterClasses` must be
   *  preferred when `UNICODE_CHARACTER_CLASSES` is not set.
   *
   *  This is a `js.Map` (and a lazy val) because it is only used when `\\p` is
   *  already known to be supported by the underlying `js.RegExp` (ES 2018),
   *  and we assume that that implies that `js.Map` is supported (ES 2015).
   */
  private lazy val predefinedPCharacterClasses: js.Map[String, CompiledCharClass] = {
    import CompiledCharClass._

    val result = new js.Map[String, CompiledCharClass]()

    // General categories

    val generalCategories = js.Array(
      "Lu", "Ll", "Lt", "LC", "Lm", "Lo", "L",
      "Mn", "Mc", "Me", "M",
      "Nd", "Nl", "No", "N",
      "Pc", "Pd", "Ps", "Pe", "Pi", "Pf", "Po", "P",
      "Sm", "Sc", "Sk", "So", "S",
      "Zs", "Zl", "Zp", "Z",
      "Cc", "Cf", "Cs", "Co", "Cn", "C"
    )

    forArrayElems(generalCategories) { gc =>
      val compiled = posP(gc)
      mapSet(result, gc, compiled)
      mapSet(result, "Is" + gc, compiled)
      mapSet(result, "general_category=" + gc, compiled)
      mapSet(result, "gc=" + gc, compiled)
    }

    // Binary properties

    mapSet(result, "IsAlphabetic", posP("Alphabetic"))
    mapSet(result, "IsIdeographic", posP("Ideographic"))
    mapSet(result, "IsLetter", posP("Letter"))
    mapSet(result, "IsLowercase", posP("Lowercase"))
    mapSet(result, "IsUppercase", posP("Uppercase"))
    mapSet(result, "IsTitlecase", posP("Lt"))
    mapSet(result, "IsPunctuation", posP("Punctuation"))
    mapSet(result, "IsControl", posP("Control"))
    mapSet(result, "IsWhite_Space", posP("White_Space"))
    mapSet(result, "IsDigit", posP("Nd"))
    mapSet(result, "IsHex_Digit", posP("Hex_Digit"))
    mapSet(result, "IsJoin_Control", posP("Join_Control"))
    mapSet(result, "IsNoncharacter_Code_Point", posP("Noncharacter_Code_Point"))
    mapSet(result, "IsAssigned", posP("Assigned"))

    // java.lang.Character classes

    mapSet(result, "javaAlphabetic", posP("Alphabetic"))
    mapSet(result, "javaDefined", negP("Cn"))
    mapSet(result, "javaDigit", posP("Nd"))
    mapSet(result, "javaIdentifierIgnorable", posClass("\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}"))
    mapSet(result, "javaIdeographic", posP("Ideographic"))
    mapSet(result, "javaISOControl", posClass("\u0000-\u001F\u007F-\u009F"))
    mapSet(result, "javaJavaIdentifierPart",
      posClass("\\p{L}\\p{Sc}\\p{Pc}\\p{Nd}\\p{Nl}\\p{Mn}\\p{Mc}\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}"))
    mapSet(result, "javaJavaIdentifierStart", posClass("\\p{L}\\p{Sc}\\p{Pc}\\p{Nl}"))
    mapSet(result, "javaLetterOrDigit", posClass("\\p{L}\\p{Nd}"))
    mapSet(result, "javaLowerCase", posP("Lowercase"))
    mapSet(result, "javaMirrored", posP("Bidi_Mirrored"))
    mapSet(result, "javaSpaceChar", posP("Z"))
    mapSet(result, "javaTitleCase", posP("Lt"))
    mapSet(result, "javaUnicodeIdentifierPart",
      posClass("\\p{ID_Continue}\u2E2F\u0000-\u0008\u000E-\u001B\u007F-\u009F\\p{Cf}"))
    mapSet(result, "javaUnicodeIdentifierStart", posClass("\\p{ID_Start}\u2E2F"))
    mapSet(result, "javaUpperCase", posP("Uppercase"))

    // [\t-\r\u001C-\u001F\\p{Z}&&[^\u00A0\u2007\u202F]]
    mapSet(result, "javaWhitespace",
      posClass("\t-\r\u001C-\u001F \u1680\u2000-\u2006\u2008-\u200A\u205F\u3000\\p{Zl}\\p{Zp}"))

    /* POSIX character classes with Unicode compatibility
     * (resolved from the original definitions, which are in comments)
     */

    mapSet(result, "Lower", posP("Lower")) // \p{IsLowercase}
    mapSet(result, "Upper", posP("Upper")) // \p{IsUppercase}
    mapSet(result, "ASCII", posClass("\u0000-\u007f"))
    mapSet(result, "Alpha", posP("Alpha")) // \p{IsAlphabetic}
    mapSet(result, "Digit", posP("Nd")) // \p{IsDigit}
    mapSet(result, "Alnum", posClass("\\p{Alpha}\\p{Nd}")) // [\p{IsAlphabetic}\p{IsDigit}]
    mapSet(result, "Punct", posP("P")) // \p{IsPunctuation}

    // [^\p{IsWhite_Space}\p{gc=Cc}\p{gc=Cs}\p{gc=Cn}]
    mapSet(result, "Graph", negClass("\\p{White_Space}\\p{Cc}\\p{Cs}\\p{Cn}"))

    /* [\p{Graph}\p{Blank}&&[^\p{Cntrl}]]
     *   === (by definition of Cntrl)
     * [\p{Graph}\p{Blank}&&[^\p{Cc}]]
     *   === (because Graph already excludes anything in the Cc category)
     * [\p{Graph}[\p{Blank}&&[^\p{Cc}]]]
     *   === (by the resolved definition of Blank below)
     * [\p{Graph}[\t\p{Zs}&&[^\p{Cc}]]]
     *   === (by the fact that \t is a Cc, and general categories are disjoint)
     * [\p{Graph}\p{Zs}]
     *   === (by definition of Graph)
     * [[^\p{IsWhite_Space}\p{Cc}\p{Cs}\p{Cn}]\p{Zs}]
     *   === (see the excerpt from PropList.txt below)
     * [[^\x09-\x0d\x85\p{Zs}\p{Zl}\p{Zp}\p{Cc}\p{Cs}\p{Cn}]\p{Zs}]
     *   === (canceling \p{Zs})
     * [^\x09-\x0d\x85\p{Zl}\p{Zp}\p{Cc}\p{Cs}\p{Cn}]
     *   === (because \x09-\x0d and \x85 are all in the Cc category)
     * [^\p{Zl}\p{Zp}\p{Cc}\p{Cs}\p{Cn}]
     */
    mapSet(result, "Print", negClass("\\p{Zl}\\p{Zp}\\p{Cc}\\p{Cs}\\p{Cn}"))

    /* [\p{IsWhite_Space}&&[^\p{gc=Zl}\p{gc=Zp}\x0a\x0b\x0c\x0d\x85]]
     *   === (see the excerpt from PropList.txt below)
     * [[\x09-\x0d\x85\p{gc=Zs}\p{gc=Zl}\p{gc=Zp}]&&[^\p{gc=Zl}\p{gc=Zp}\x0a\x0b\x0c\x0d\x85]]
     *   === (by simplification)
     * [\x09\p{gc=Zs}]
     */
    mapSet(result, "Blank", posClass("\t\\p{Zs}"))

    mapSet(result, "Cntrl", posP("Cc")) // \p{gc=Cc}
    mapSet(result, "XDigit", posClass("\\p{Nd}\\p{Hex}")) // [\p{gc=Nd}\p{IsHex_Digit}]
    mapSet(result, "Space", posP("White_Space")) // \p{IsWhite_Space}

    result
  }

  /* Excerpt from PropList.txt v13.0.0:
   *
   * 0009..000D    ; White_Space # Cc   [5] <control-0009>..<control-000D>
   * 0020          ; White_Space # Zs       SPACE
   * 0085          ; White_Space # Cc       <control-0085>
   * 00A0          ; White_Space # Zs       NO-BREAK SPACE
   * 1680          ; White_Space # Zs       OGHAM SPACE MARK
   * 2000..200A    ; White_Space # Zs  [11] EN QUAD..HAIR SPACE
   * 2028          ; White_Space # Zl       LINE SEPARATOR
   * 2029          ; White_Space # Zp       PARAGRAPH SEPARATOR
   * 202F          ; White_Space # Zs       NARROW NO-BREAK SPACE
   * 205F          ; White_Space # Zs       MEDIUM MATHEMATICAL SPACE
   * 3000          ; White_Space # Zs       IDEOGRAPHIC SPACE
   *
   * Note that *all* the code points with general category Zs, Zl or Zp are
   * listed here. In addition, we have 0009-000D and 0085 from the Cc category.
   * Therefore, the following equivalence holds:
   *
   *   \p{IsWhite_Space} === [\x09-\x0d\x85\p{gc=Zs}\p{gc=Zl}\p{gc=Zp}]
   *
   * That equivalence is known to be true as of Unicode 13.0.0, and seems to
   * have been true for a number of past versions as well. We rely on it to
   * define \p{Print} and \p{Blank} above. Those would become buggy if a future
   * version of Unicode invalidates that assumption.
   */

  private val scriptCanonicalizeRegExp = new js.RegExp("(?:^|_)[a-z]", "g")

  /** A cache for verified and canonicalized script names.
   *
   *  This is a `js.Map` (and a lazy val) because it is only used when `\\p` is
   *  already known to be supported by the underlying `js.RegExp` (ES 2018),
   *  and we assume that that implies that `js.Map` is supported (ES 2015).
   */
  private lazy val canonicalizedScriptNameCache: js.Map[String, String] = {
    val result = new js.Map[String, String]()

    /* SignWriting is an exception. It has an uppercase 'W' even though it is
     * not after '_'. We add the exception to the map immediately.
     */
    mapSet(result, "signwriting", "SignWriting")

    result
  }

  @inline
  private final class CodePointRange(val start: Int, val end: Int) {
    def isEmpty: Boolean = start > end
    def nonEmpty: Boolean = start <= end

    /** Computes the intersection of two *non-empty* ranges.
     *
     *  This method makes no guarantee about its result if either or both input
     *  ranges are empty.
     *
     *  The result range may be empty.
     */
    def intersect(that: CodePointRange): CodePointRange =
      CodePointRange(Math.max(this.start, that.start), Math.min(this.end, that.end))

    def shift(offset: Int): CodePointRange =
      CodePointRange(start + offset, end + offset)
  }

  private object CodePointRange {
    @inline
    def apply(start: Int, end: Int): CodePointRange =
      new CodePointRange(start, end)

    @inline
    def BmpBelowHighSurrogates: CodePointRange =
      CodePointRange(0, Character.MIN_HIGH_SURROGATE - 1)

    @inline
    def HighSurrogates: CodePointRange =
      CodePointRange(Character.MIN_HIGH_SURROGATE, Character.MAX_HIGH_SURROGATE)

    @inline
    def BmpAboveHighSurrogates: CodePointRange =
      CodePointRange(Character.MAX_HIGH_SURROGATE + 1, Character.MAX_VALUE)

    @inline
    def Supplementaries: CodePointRange =
      CodePointRange(Character.MIN_SUPPLEMENTARY_CODE_POINT, Character.MAX_CODE_POINT)
  }

  private final class CharacterClassBuilder(asciiCaseInsensitive: Boolean, isNegated: Boolean) {
    private var conjunction = ""
    private var thisConjunct = ""
    private var thisSegment = ""

    def finish(): String = {
      val conjunct = conjunctResult()
      if (conjunction == "") conjunct else s"(?:$conjunction$conjunct)"
    }

    def startNewConjunct(): Unit = {
      val conjunct = conjunctResult()
      conjunction += (if (isNegated) conjunct + "|" else s"(?=$conjunct)")
      thisConjunct = ""
      thisSegment = ""
    }

    private def addAlternative(alt: String): Unit = {
      if (thisConjunct == "")
        thisConjunct = alt
      else
        thisConjunct += "|" + alt
    }

    private def conjunctResult(): String = {
      if (isNegated) {
        val negThisSegment = codePointNotAmong(thisSegment)
        if (thisConjunct == "")
          negThisSegment
        else
          s"(?:(?!$thisConjunct)$negThisSegment)"
      } else if (thisSegment == "") {
        if (thisConjunct == "")
          "[^\\d\\D]" // impossible to satisfy
        else
          s"(?:$thisConjunct)"
      } else {
        if (thisConjunct == "")
          s"[$thisSegment]"
        else
          s"(?:$thisConjunct|[$thisSegment])"
      }
    }

    private def literalCodePoint(codePoint: Int): String = {
      val s = codePointToString(codePoint)
      if (codePoint == ']' || codePoint == '\\' || codePoint == '-' || codePoint == '^')
        "\\" + s
      else
        s
    }

    def addCharacterClass(cls: String): Unit =
      addAlternative(cls)

    def addCharacterClass(cls: CompiledCharClass): Unit = {
      cls.kind match {
        case CompiledCharClass.PosP =>
          thisSegment += "\\p{" + cls.data + "}"
        case CompiledCharClass.NegP =>
          thisSegment += "\\P{" + cls.data + "}"
        case CompiledCharClass.PosClass =>
          thisSegment += cls.data
        case CompiledCharClass.NegClass =>
          addAlternative(codePointNotAmong(cls.data))
      }
    }

    def addCodePointsInString(str: String, start: Int, end: Int): Unit = {
      var i = start
      while (i != end) {
        val codePoint = str.codePointAt(i)
        addSingleCodePoint(codePoint)
        i += charCount(codePoint)
      }
    }

    def addSingleCodePoint(codePoint: Int): Unit = {
      val s = literalCodePoint(codePoint)

      if (supportsUnicode || (isBmpCodePoint(codePoint) && !isHighSurrogateCP(codePoint))) {
        if (isLowSurrogateCP(codePoint)) {
          // Put low surrogates at the beginning so that they do not merge with high surrogates
          thisSegment = s + thisSegment
        } else {
          thisSegment += s
        }
      } else {
        if (isBmpCodePoint(codePoint)) {
          // It is a high surrogate
          addAlternative(s"(?:$s(?![$MIN_LOW_SURROGATE-$MAX_LOW_SURROGATE]))")
        } else {
          // It is a supplementary code point
          addAlternative(s)
        }
      }

      if (asciiCaseInsensitive) {
        if (codePoint >= 'A' && codePoint <= 'Z')
          thisSegment += codePointToString(codePoint - 'A' + 'a')
        else if (codePoint >= 'a' && codePoint <= 'z')
          thisSegment += codePointToString(codePoint - 'a' + 'A')
      }
    }

    def addCodePointRange(startCodePoint: Int, endCodePoint: Int): Unit = {
      def literalRange(range: CodePointRange): String =
        literalCodePoint(range.start) + "-" + literalCodePoint(range.end)

      val range = CodePointRange(startCodePoint, endCodePoint)

      if (supportsUnicode || range.end < MIN_HIGH_SURROGATE) {
        val s = literalRange(range)

        if (isLowSurrogateCP(range.start)) {
          /* Put ranges whose start code point is a low surrogate at the
           * beginning, so that they cannot merge with a high surrogate. Since
           * the numeric values of high surrogates is *less than* that of low
           * surrogates, the `range.end` cannot be a high surrogate here, and
           * so there is no danger of it merging with a low surrogate already
           * present at the beginning of `thisSegment`.
           */
          thisSegment = s + thisSegment
        } else {
          thisSegment += s
        }
      } else {
        /* Here be dragons. We need to split the range into several ranges that
         * we can separately compile.
         *
         * Since the 'u' flag is not used when we get here, the RegExp engine
         * treats surrogate chars as individual chars in all cases. Therefore,
         * we do not need to protect low surrogates.
         */

        val bmpBelowHighSurrogates = range.intersect(CodePointRange.BmpBelowHighSurrogates)
        if (bmpBelowHighSurrogates.nonEmpty)
          thisSegment += literalRange(bmpBelowHighSurrogates)

        val highSurrogates = range.intersect(CodePointRange.HighSurrogates)
        if (highSurrogates.nonEmpty)
          addAlternative("[" + literalRange(highSurrogates) + "]" + s"(?![$MIN_LOW_SURROGATE-$MAX_LOW_SURROGATE])")

        val bmpAboveHighSurrogates = range.intersect(CodePointRange.BmpAboveHighSurrogates)
        if (bmpAboveHighSurrogates.nonEmpty)
          thisSegment += literalRange(bmpAboveHighSurrogates)

        val supplementaries = range.intersect(CodePointRange.Supplementaries)
        if (supplementaries.nonEmpty) {
          val startHigh = highSurrogate(supplementaries.start)
          val startLow = lowSurrogate(supplementaries.start)

          val endHigh = highSurrogate(supplementaries.end)
          val endLow = lowSurrogate(supplementaries.end)

          if (startHigh == endHigh) {
            addAlternative(
                codePointToString(startHigh) + "[" + literalRange(CodePointRange(startLow, endLow)) + "]")
          } else {
            addAlternative(
                codePointToString(startHigh) + "[" + literalRange(CodePointRange(startLow, MAX_LOW_SURROGATE)) + "]")

            val middleHighs = CodePointRange(startHigh + 1, endHigh - 1)
            if (middleHighs.nonEmpty)
              addAlternative(s"[${literalRange(middleHighs)}][$MIN_LOW_SURROGATE-$MAX_LOW_SURROGATE]")

            addAlternative(
                codePointToString(endHigh) + "[" + literalRange(CodePointRange(MIN_LOW_SURROGATE, endLow)) + "]")
          }
        }
      }

      if (asciiCaseInsensitive) {
        val uppercases = range.intersect(CodePointRange('A', 'Z'))
        if (uppercases.nonEmpty)
          thisSegment += literalRange(uppercases.shift('a' - 'A'))

        val lowercases = range.intersect(CodePointRange('a', 'z'))
        if (lowercases.nonEmpty)
          thisSegment += literalRange(lowercases.shift('A' - 'a'))
      }
    }
  }
}

private final class PatternCompiler(private val pattern: String, private var flags: Int) {
  import PatternCompiler._
  import PatternCompiler.Support._
  import PatternCompiler.InlinedHelpers._
  import Pattern._

  /** Whether the result `Pattern` must be sticky. */
  private var sticky: Boolean = false

  /** The parse index, within `pattern`. */
  private var pIndex: Int = 0

  /** The number of capturing groups in the compiled pattern.
   *
   *  This is different than `originalGroupCount` when there are atomic groups
   *  (or possessive quantifiers, which are sugar for atomic groups).
   */
  private var compiledGroupCount: Int = 0

  /** Map from original group number to compiled group number.
   *
   *  It contains a mapping for the entire match, which is group 0.
   */
  private val groupNumberMap = js.Array[Int](0)

  /** The number of capturing groups found so far in the original pattern.
   *
   *  This is `groupNumberMap.length - 1`, because `groupNumberMap` contains
   *  the mapping for the entire match, which is group 0.
   */
  @inline private def originalGroupCount = groupNumberMap.length - 1

  /** Map from group name to original group number.
   *
   *  We store *original* group numbers, rather than compiled group numbers,
   *  in order to make the renumbering caused by possessive quantifiers easier.
   */
  private val namedGroups = dictEmpty[Int]()

  @inline private def hasFlag(flag: Int): Boolean = (flags & flag) != 0

  @inline private def unixLines: Boolean = hasFlag(UNIX_LINES)
  @inline private def comments: Boolean = hasFlag(COMMENTS)
  @inline private def dotAll: Boolean = hasFlag(DOTALL)

  @inline
  private def asciiCaseInsensitive: Boolean =
    (flags & (CASE_INSENSITIVE | UNICODE_CASE)) == CASE_INSENSITIVE

  @inline
  private def unicodeCaseInsensitive: Boolean = {
    enableUnicodeCaseInsensitive && // for dead code elimination
    (flags & (CASE_INSENSITIVE | UNICODE_CASE)) == (CASE_INSENSITIVE | UNICODE_CASE)
  }

  @inline
  private def unicodeCaseOrUnicodeCharacterClass: Boolean = {
    enableUnicodeCaseInsensitive && // for dead code elimination
    (flags & (UNICODE_CASE | UNICODE_CHARACTER_CLASS)) != 0
  }

  @inline
  private def multiline: Boolean = {
    enableUnicodeCharacterClassesAndLookBehinds && // for dead code elimination
    hasFlag(MULTILINE)
  }

  @inline
  private def unicodeCharacterClass: Boolean = {
    enableUnicodeCharacterClassesAndLookBehinds && // for dead code elimination
    hasFlag(UNICODE_CHARACTER_CLASS)
  }

  def compile(): Pattern = {
    // UNICODE_CHARACTER_CLASS implies UNICODE_CASE, even for LITERAL
    if (hasFlag(UNICODE_CHARACTER_CLASS))
      flags |= UNICODE_CASE

    val isLiteral = hasFlag(LITERAL)

    if (!isLiteral)
      processLeadingEmbeddedFlags()

    if (hasFlag(CANON_EQ))
      parseError("CANON_EQ is not supported")

    if (!enableUnicodeCharacterClassesAndLookBehinds) {
      if (hasFlag(MULTILINE))
        parseErrorRequireESVersion("MULTILINE", "2018")
      if (hasFlag(UNICODE_CHARACTER_CLASS))
        parseErrorRequireESVersion("UNICODE_CHARACTER_CLASS", "2018")
    }

    if (!enableUnicodeCaseInsensitive) {
      if (hasFlag(UNICODE_CASE))
        parseErrorRequireESVersion("UNICODE_CASE", "2015")
    }

    val jsPattern = if (isLiteral) {
      literal(pattern)
    } else {
      if (pattern.jsSubstring(pIndex, pIndex + 2) == "\\G") {
        sticky = true
        pIndex += 2
      }
      compileTopLevel()
    }

    val jsFlags = {
      // We always use the 'u' and 's' flags when they are supported.
      val baseJSFlags = {
        if (supportsDotAll) "us"
        else if (supportsUnicode) "u"
        else ""
      }

      // We add the 'i' flag when using Unicode-aware case insensitive matching.
      if (unicodeCaseInsensitive) baseJSFlags + "i"
      else baseJSFlags
    }

    new Pattern(pattern, flags, jsPattern, jsFlags, sticky, originalGroupCount,
        groupNumberMap, namedGroups)
  }

  private def parseError(desc: String): Nothing =
    throw new PatternSyntaxException(desc, pattern, pIndex)

  @inline
  private def requireES2018Features(purpose: String): Unit = {
    if (!enableUnicodeCharacterClassesAndLookBehinds)
      parseErrorRequireESVersion(purpose, "2018")
  }

  @noinline
  private def parseErrorRequireESVersion(purpose: String, es: String): Nothing = {
    parseError(
        s"$purpose is not supported because it requires RegExp features of ECMAScript $es.\n" +
        s"If you only target environments with ES$es+, you can enable ES$es features with\n" +
        s"  scalaJSLinkerConfig ~= { _.withESFeatures(_.withESVersion(ESVersion.ES$es)) }\n" +
        "or an equivalent configuration depending on your build tool.")
  }

  private def processLeadingEmbeddedFlags(): Unit = {
    val m = leadingEmbeddedFlagSpecifierRegExp.exec(pattern)
    if (m != null) {
      undefOrForeach(m(1)) { chars =>
        for (i <- 0 until chars.length())
          flags |= charToFlag(chars.charAt(i))
      }

      // If U was in the flags, we need to enable UNICODE_CASE as well
      if (hasFlag(UNICODE_CHARACTER_CLASS))
        flags |= UNICODE_CASE

      undefOrForeach(m(2)) { chars =>
        for (i <- 0 until chars.length())
          flags &= ~charToFlag(chars.charAt(i))
      }

      /* The way things are done here, it is possible to *remove*
       * `UNICODE_CASE` from the set of flags while leaving
       * `UNICODE_CHARACTER_CLASS` in. This creates a somewhat inconsistent
       * state, but it matches what the JVM does, as illustrated in the test
       * `RegexPatternTest.flags()`.
       */

      // Advance past the embedded flags
      pIndex += undefOrForceGet(m(0)).length()
    }
  }

  // The predefined character class for \w, depending on the UNICODE_CHARACTER_CLASS flag

  @inline
  private def wordCharClass: CompiledCharClass =
    if (unicodeCharacterClass) UnicodeWordChar
    else ASCIIWordChar

  // Meat of the compilation

  private def literal(s: String): String = {
    var result = ""
    val len = s.length()
    var i = 0
    while (i != len) {
      val cp = s.codePointAt(i)
      result += literal(cp)
      i += charCount(cp)
    }
    result
  }

  private def literal(cp: Int): String = {
    val s = codePointToString(cp)

    if (cp < 0x80) {
      /* SyntaxCharacter :: one of
       *   ^ $ \ . * + ? ( ) [ ] { } |
       */
      (cp: @switch) match {
        case '^' | '$' | '\\' | '.' | '*' | '+' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '|' =>
          "\\" + s
        case _ =>
          if (!asciiCaseInsensitive)
            s
          else if (cp >= 'A' && cp <= 'Z')
            "[" + s + codePointToString(cp + ('a' - 'A')) + "]"
          else if (cp >= 'a' && cp <= 'z')
            "[" + codePointToString(cp + ('A' - 'a')) + s + "]"
          else
            s
      }
    } else {
      if (supportsUnicode) {
        /* We wrap low surrogates with `(?:x)` to ensure that we do not
         * artificially create a surrogate pair in the compiled pattern where
         * none existed in the source pattern.
         * Consider the source pattern `\x{D834}\x{DD1E}`, for example.
         * If low surrogates were not wrapped, it would be compiled to a
         * surrogate pair, which would match the input string `"𝄞"` although it
         * is not supposed to.
         */
        if (isLowSurrogateCP(cp))
          s"(?:$s)"
        else
          s
      } else {
        if (isHighSurrogateCP(cp))
          s"(?:$s(?![$MIN_LOW_SURROGATE-$MAX_LOW_SURROGATE]))"
        else if (isBmpCodePoint(cp))
          s
        else
          s"(?:$s)" // group a surrogate pair so that it is repeated as a whole
      }
    }
  }

  @inline
  private def compileTopLevel(): String =
    compileTopLevelOrInsideGroup(insideGroup = false)

  @inline
  private def compileInsideGroup(): String =
    compileTopLevelOrInsideGroup(insideGroup = true)

  /** The main parsing method.
   *
   *  It follows a recursive descent approach. It is recursive for any
   *  `(...)`-enclosed subpattern, and flat for other kinds of patterns.
   */
  private def compileTopLevelOrInsideGroup(insideGroup: Boolean): String = {
    // scalastyle:off return
    // the 'return' is in the case ')'

    val pattern = this.pattern // local copy
    val len = pattern.length()

    var result = ""

    while (pIndex != len) {
      val dispatchCP = pattern.codePointAt(pIndex)
      (dispatchCP: @switch) match {
        // Cases that mess with the control flow and/or that cannot be repeated

        case ')' =>
          if (!insideGroup)
            parseError("Unmatched closing ')'")
          pIndex += 1
          return result

        case '|' =>
          if (sticky && !insideGroup)
            parseError("\\G is not supported when there is an alternative at the top level")
          pIndex += 1
          result += "|"

        // experimentally, this is the set of chars considered as whitespace for comments
        case ' ' | '\t' | '\n' | '\u000B' | '\f' | '\r' if comments =>
          pIndex += 1

        case '#' if comments =>
          skipSharpComment()

        case '?' | '*' | '+' | '{' =>
          parseError("Dangling meta character '" + codePointToString(dispatchCP) + "'")

        // Regular cases, which can be repeated

        case _ =>
          // Record the current compiledGroupCount, for possessive quantifiers
          val compiledGroupCountBeforeThisToken = compiledGroupCount

          val compiledToken = (dispatchCP: @switch) match {
            case '\\' => compileEscape()
            case '['  => compileCharacterClass()
            case '('  => compileGroup()
            case '^'  => compileCaret()
            case '$'  => compileDollar()
            case '.'  => compileDot()

            case _ =>
              pIndex += charCount(dispatchCP)
              literal(dispatchCP)
          }

          result += compileRepeater(compiledGroupCountBeforeThisToken, compiledToken)
      }
    }

    if (insideGroup)
      parseError("Unclosed group")

    result
    // scalastyle:on return
  }

  /** Skip a '#' comment.
   *
   *  Pre-condition: `comments && pattern.charAt(pIndex) == '#'` is true
   */
  private def skipSharpComment(): Unit = {
    val pattern = this.pattern // local copy
    val len = pattern.length()

    @inline def isEOL(c: Char): Boolean =
      c == '\n' || c == '\r' || c == '\u0085' || c == '\u2028' || c == '\u2029'

    while (pIndex != len && !isEOL(pattern.charAt(pIndex)))
      pIndex += 1
  }

  /** Skip all comments.
   *
   *  Pre-condition: `comments` is true
   */
  @noinline
  private def skipComments(): Unit = {
    val pattern = this.pattern // local copy
    val len = pattern.length()

    @inline @tailrec
    def loop(): Unit = {
      if (pIndex != len) {
        (pattern.charAt(pIndex): @switch) match {
          case ' ' | '\t' | '\n' | '\u000B' | '\f' | '\r' =>
            pIndex += 1
            loop()
          case '#' =>
            skipSharpComment()
            loop()
          case _ =>
            ()
        }
      }
    }

    loop()
  }

  private def compileRepeater(compiledGroupCountBeforeThisToken: Int, compiledToken: String): String = {
    val pattern = this.pattern // local copy
    val len = pattern.length()

    val startOfRepeater = pIndex
    val repeaterDispatchChar =
      if (startOfRepeater == len) '.'
      else pattern.charAt(startOfRepeater)

    @inline def hasRepeater: Boolean = {
      repeaterDispatchChar == '?' || repeaterDispatchChar == '*' ||
      repeaterDispatchChar == '+' || repeaterDispatchChar == '{'
    }

    if (hasRepeater) {
      // There is a repeater

      /* #4784 Wrap tokens that are Assertions in ES' pattern syntax, since
       * it is not syntactically valid to directly quantify them. It is valid
       * to quantify a group containing an Assertion, however.
       *
       * There is no index-out-of-bounds in the following code because
       * `compiledToken` is known to be a syntactically valid, non-empty regex.
       */
      val isTokenAnAssertion = (compiledToken.charAt(0): @switch) match {
        case '^' | '$' =>
          true
        case '(' =>
          /* This expression would also match named capturing groups, but we
           * never emit those. Anyway, even if we did, we would uselessly wrap
           * a group that does not need to be, but it would still be correct.
           */
          compiledToken.charAt(1) == '?' && compiledToken.charAt(2) != ':'
        case '\\' =>
          val c = compiledToken.charAt(1)
          c == 'b' || c == 'B'
        case _ =>
          false
      }
      val wrappedToken =
        if (isTokenAnAssertion) "(?:" + compiledToken + ")"
        else compiledToken

      val baseRepeater = parseBaseRepeater(repeaterDispatchChar)

      if (pIndex != len) {
        pattern.charAt(pIndex) match {
          case '+' =>
            // Possessive quantifier
            pIndex += 1
            buildPossessiveQuantifier(compiledGroupCountBeforeThisToken, wrappedToken, baseRepeater)
          case '?' =>
            // Lazy quantifier
            pIndex += 1
            wrappedToken + baseRepeater + "?"
          case _ =>
            // Greedy quantifier
            wrappedToken + baseRepeater
        }
      } else {
        // Greedy quantifier
        wrappedToken + baseRepeater
      }
    } else {
      // No repeater
      compiledToken
    }
  }

  private def parseBaseRepeater(repeaterDispatchChar: Char): String = {
    val pattern = this.pattern // local copy
    val startOfRepeater = pIndex

    pIndex += 1

    if (repeaterDispatchChar == '{') {
      val len = pattern.length()

      if (pIndex == len || !isDigit(pattern.charAt(pIndex)))
        parseError("Illegal repetition")
      while (pIndex != len && isDigit(pattern.charAt(pIndex)))
        pIndex += 1
      if (pIndex == len)
        parseError("Illegal repetition")
      if (pattern.charAt(pIndex) == ',') {
        pIndex += 1
        while (pIndex != len && isDigit(pattern.charAt(pIndex)))
          pIndex += 1
      }
      if (pIndex == len || pattern.charAt(pIndex) != '}')
        parseError("Illegal repetition")
      pIndex += 1
    }

    pattern.jsSubstring(startOfRepeater, pIndex)
  }

  /** Builds a possessive quantifier, which is sugar for an atomic group over
   *  a greedy quantifier.
   */
  private def buildPossessiveQuantifier(compiledGroupCountBeforeThisToken: Int,
      compiledToken: String, baseRepeater: String): String = {

    /* This is very intricate. Not only do we need to surround a posteriori the
     * previous token, we are introducing a new capturing group in between.
     * This means that we need to renumber all backreferences contained in the
     * compiled token.
     */

    // Remap group numbers
    for (i <- 0 until groupNumberMap.length) {
      val mapped = groupNumberMap(i)
      if (mapped > compiledGroupCountBeforeThisToken)
        groupNumberMap(i) = mapped + 1
    }

    // Renumber all backreferences contained in the compiled token
    import js.JSStringOps._
    val amendedToken = compiledToken.jsReplace(renumberingRegExp, {
      (str, backslashes, groupString) =>
        if (backslashes.length() % 2 == 0) { // poor man's negative look-behind
          str
        } else {
          val groupNumber = parseInt(groupString, 10)
          if (groupNumber > compiledGroupCountBeforeThisToken)
            backslashes + (groupNumber + 1)
          else
            str
        }
    }: js.Function3[String, String, String, String])

    // Plan the future remapping
    compiledGroupCount += 1

    // Finally, the encoding of the atomic group over the greedy quantifier
    val myGroupNumber = compiledGroupCountBeforeThisToken + 1
    s"(?:(?=($amendedToken$baseRepeater))\\$myGroupNumber)"
  }

  @inline
  private def compileCaret(): String = {
    pIndex += 1
    if (multiline) {
      /* `multiline` implies ES2018, so we can use look-behind assertions.
       * We cannot use the 'm' flag of JavaScript RegExps because its semantics
       * differ from the Java ones (either with or without `UNIX_LINES`).
       */
      if (unixLines)
        "(?<=^|\n)"
      else
        "(?<=^|\r(?!\n)|[\n\u0085\u2028\u2029])"
    } else {
      "^"
    }
  }

  @inline
  private def compileDollar(): String = {
    pIndex += 1
    if (multiline) {
      /* `multiline` implies ES2018, so we can use look-behind assertions.
       * We cannot use the 'm' flag of JavaScript RegExps (see ^ above).
       */
      if (unixLines)
        "(?=$|\n)"
      else
        "(?=$|(?<!\r)\n|[\r\u0085\u2028\u2029])"
    } else {
      "$"
    }
  }

  @inline
  private def compileDot(): String = {
    /* Since JavaScript's `.`'s interpretation of new lines is not the same as
     * Java's (with or without UNIX_LINES), we compile `.` to custom character
     * classes.
     */
    pIndex += 1
    val rejected = {
      if (dotAll) ""
      else if (unixLines) "\n"
      else "\n\r\u0085\u2028\u2029"
    }
    codePointNotAmong(rejected)
  }

  private def compileEscape(): String = {
    val pattern = this.pattern // local copy
    val len = pattern.length()

    if (pIndex + 1 == len)
      parseError("\\ at end of pattern")

    pIndex += 1
    val dispatchChar = pattern.charAt(pIndex)

    (dispatchChar: @switch) match {
      // Predefined character classes

      case 'd' | 'D' | 'h' | 'H' | 's' | 'S' | 'v' | 'V' | 'w' | 'W' | 'p' | 'P' =>
        val cls = parsePredefinedCharacterClass(dispatchChar)
        cls.kind match {
          case CompiledCharClass.PosP =>
            "\\p{" + cls.data + "}"
          case CompiledCharClass.NegP =>
            "\\P{" + cls.data + "}"
          case CompiledCharClass.PosClass =>
            "[" + cls.data + "]"
          case CompiledCharClass.NegClass =>
            codePointNotAmong(cls.data)
        }

      // Boundary matchers

      case 'b' =>
        if (pattern.jsSubstring(pIndex, pIndex + 4) == "b{g}") {
          parseError("\\b{g} is not supported")
        } else {
          /* Compile as is if both `UNICODE_CASE` and `UNICODE_CHARACTER_CLASS` are false.
           * This is correct because:
           * - since `UNICODE_CHARACTER_CLASS` is false, word chars are
           *   considered to be `[a-zA-Z_0-9]` for Java semantics, and
           * - since `UNICODE_CASE` is false, we do not use the 'i' flag in the
           *   JS RegExp, and so word chars are considered to be `[a-zA-Z_0-9]`
           *   for the JS semantics as well.
           *
           * In all other cases, we determine the compiled form of `\w` and use
           * a custom look-around-based implementation.
           * This requires ES2018+, hence why we go to the trouble of trying to
           * reuse `\b` if we can.
           */
          if (unicodeCaseOrUnicodeCharacterClass) {
            requireES2018Features("\\b with UNICODE_CASE") // UNICODE_CHARACTER_CLASS would have been rejected earlier
            pIndex += 1
            val w = wordCharClass.data
            s"(?:(?<=[$w])(?![$w])|(?<![$w])(?=[$w]))"
          } else {
            pIndex += 1
            "\\b"
          }
        }
      case 'B' =>
        // Same strategy as for \b above
        if (unicodeCaseOrUnicodeCharacterClass) {
          requireES2018Features("\\B with UNICODE_CASE") // UNICODE_CHARACTER_CLASS would have been rejected earlier
          pIndex += 1
          val w = wordCharClass.data
          s"(?:(?<=[$w])(?=[$w])|(?<![$w])(?![$w]))"
        } else {
          pIndex += 1
          "\\B"
        }
      case 'A' =>
        // We can always use ^ for start-of-text because we never use the 'm' flag in the JS RegExp
        pIndex += 1
        "^"
      case 'G' =>
        parseError("\\G in the middle of a pattern is not supported")
      case 'Z' =>
        // We can always use $ for end-of-text because we never use the 'm' flag in the JS RegExp
        pIndex += 1
        val lineTerminator =
          if (unixLines) "\n"
          else "(?:\r\n?|[\n\u0085\u2028\u2029])"
        "(?=" + lineTerminator + "?$)"
      case 'z' =>
        // We can always use $ for end-of-text because we never use the 'm' flag in the JS RegExp
        pIndex += 1
        "$"

      // Linebreak matcher

      case 'R' =>
        pIndex += 1
        "(?:\r\n|[\n-\r\u0085\u2028\u2029])"

      // Unicode Extended Grapheme matcher

      case 'X' =>
        parseError("\\X is not supported")

      // Back references

      case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
        /* From the JavaDoc:
         *
         * > In this class, \1 through \9 are always interpreted as back
         * > references, and a larger number is accepted as a back reference if
         * > at least that many subexpressions exist at that point in the
         * > regular expression, otherwise the parser will drop digits until
         * > the number is smaller or equal to the existing number of groups or
         * > it is one digit.
         */
        val start = pIndex
        var end = start + 1

        // In most cases, one of the first two conditions is immediately false
        while (end != len && isDigit(pattern.charAt(end)) &&
            parseInt(pattern.jsSubstring(start, end + 1), 10) <= originalGroupCount) {
          end += 1
        }

        val groupString = pattern.jsSubstring(start, end)
        val groupNumber = parseInt(groupString, 10)
        if (groupNumber > originalGroupCount)
          parseError(s"numbered capturing group <$groupNumber> does not exist")
        val compiledGroupNumber = groupNumberMap(groupNumber)
        pIndex = end
        // Wrap in a non-capturing group in case it's followed by a (de-escaped) digit
        "(?:\\" + compiledGroupNumber + ")"

      case 'k' =>
        pIndex += 1
        if (pIndex == len || pattern.charAt(pIndex) != '<')
          parseError("\\k is not followed by '<' for named capturing group")
        pIndex += 1
        val groupName = parseGroupName()
        val groupNumber = dictGetOrElse(namedGroups, groupName) {
          parseError(s"named capturing group <$groupName> does not exit")
        }
        val compiledGroupNumber = groupNumberMap(groupNumber)
        pIndex += 1
        // Wrap in a non-capturing group in case it's followed by a (de-escaped) digit
        "(?:\\" + compiledGroupNumber + ")"

      // Quotes

      case 'Q' =>
        val start = pIndex + 1
        val end = pattern.indexOf("\\E", start)
        if (end < 0) {
          pIndex = pattern.length()
          literal(pattern.jsSubstring(start))
        } else {
          pIndex = end + 2
          literal(pattern.jsSubstring(start, end))
        }

      // Other

      case c =>
        literal(parseSingleCodePointEscape())
    }
  }

  private def parseSingleCodePointEscape(): Int = {
    val pattern = this.pattern // local copy

    (pattern.codePointAt(pIndex): @switch) match {
      case '0' =>
        parseOctalEscape()
      case 'x' =>
        parseHexEscape()
      case 'u' =>
        parseUnicodeHexEscape()
      case 'N' =>
        parseError("\\N is not supported")
      case 'a' =>
        pIndex += 1
        0x0007
      case 't' =>
        pIndex += 1
        0x0009
      case 'n' =>
        pIndex += 1
        0x000a
      case 'f' =>
        pIndex += 1
        0x000c
      case 'r' =>
        pIndex += 1
        0x000d
      case 'e' =>
        pIndex += 1
        0x001b
      case 'c' =>
        pIndex += 1
        if (pIndex == pattern.length())
          parseError("Illegal control escape sequence")
        val cp = pattern.codePointAt(pIndex)
        pIndex += charCount(cp)
        // https://stackoverflow.com/questions/35208570/java-regular-expression-cx-control-characters
        cp ^ 0x40

      case cp =>
        // Other letters are forbidden / reserved for future use
        if ((cp >= 'A' && cp <= 'Z') || (cp >= 'a' && cp <= 'z'))
          parseError("Illegal/unsupported escape sequence")

        // But everything else is accepted and quoted as is
        pIndex += charCount(cp)
        cp
    }
  }

  private def parseOctalEscape(): Int = {
    /* \0n    The character with octal value 0n (0 <= n <= 7)
     * \0nn   The character with octal value 0nn (0 <= n <= 7)
     * \0mnn  The character with octal value 0mnn (0 <= m <= 3, 0 <= n <= 7)
     */

    val pattern = this.pattern // local copy
    val len = pattern.length()
    val start = pIndex

    val d1 =
      if (start + 1 < len) pattern.charAt(start + 1) - '0'
      else -1
    if (d1 < 0 || d1 > 7)
      parseError("Illegal octal escape sequence")

    val d2 =
      if (start + 2 < len) pattern.charAt(start + 2) - '0'
      else -1

    if (d2 < 0 || d2 > 7) {
      pIndex += 2
      d1
    } else if (d1 > 3) {
      pIndex += 3
      d1 * 8 + d2
    } else {
      val d3 =
        if (start + 3 < len) pattern.charAt(start + 3) - '0'
        else -1

      if (d3 < 0 || d3 > 7) {
        pIndex += 3
        d1 * 8 + d2
      } else {
        pIndex += 4
        d1 * 64 + d2 * 8 + d3
      }
    }
  }

  private def parseHexEscape(): Int = {
    /* \xhh       The character with hexadecimal value 0xhh
     * \x{h...h}  The character with hexadecimal value 0xh...h
     *            (Character.MIN_CODE_POINT <= 0xh...h <= Character.MAX_CODE_POINT)
     */

    val pattern = this.pattern // local copy
    val len = pattern.length()

    val start = pIndex + 1

    if (start != len && pattern.charAt(start) == '{') {
      val innerStart = start + 1
      val innerEnd = pattern.indexOf("}", innerStart)
      if (innerEnd < 0)
        parseError("Unclosed hexadecimal escape sequence")
      val cp = parseHexCodePoint(innerStart, innerEnd, "hexadecimal")
      pIndex = innerEnd + 1
      cp
    } else {
      val cp = parseHexCodePoint(start, start + 2, "hexadecimal")
      pIndex = start + 2
      cp
    }
  }

  private def parseUnicodeHexEscape(): Int = {
    /* \ uhhhh  The character with hexadecimal value 0xhhhh
     *
     * An escaped high surrogate followed by an escaped low surrogate form a
     * unique escaped code point. This is important in character classes.
     */

    val pattern = this.pattern // local copy

    val start = pIndex + 1
    val end = start + 4
    val codeUnit = parseHexCodePoint(start, end, "Unicode")

    pIndex = end

    val lowStart = end + 2
    val lowEnd = lowStart + 4

    if (isHighSurrogateCP(codeUnit) && pattern.jsSubstring(end, lowStart) == "\\u") {
      val low = parseHexCodePoint(lowStart, lowEnd, "Unicode")
      if (isLowSurrogateCP(low)) {
        pIndex = lowEnd
        toCodePointCP(codeUnit, low)
      } else {
        codeUnit
      }
    } else {
      codeUnit
    }
  }

  private def parseHexCodePoint(start: Int, end: Int, nameForError: String): Int = {
    val pattern = this.pattern // local copy
    val len = pattern.length()

    if (start == end || end > len)
      parseError(s"Illegal $nameForError escape sequence")

    for (i <- start until end) {
      if (!isHexDigit(pattern.charAt(i)))
        parseError(s"Illegal $nameForError escape sequence")
    }

    val cp =
      if (end - start > 6) Character.MAX_CODE_POINT + 1
      else parseInt(pattern.jsSubstring(start, end), 16)
    if (cp > Character.MAX_CODE_POINT)
      parseError("Hexadecimal codepoint is too big")

    cp
  }

  /** Parses and returns a translated version of a pre-defined character class. */
  private def parsePredefinedCharacterClass(dispatchChar: Char): CompiledCharClass = {
    import CompiledCharClass._

    pIndex += 1

    val positive = (dispatchChar: @switch) match {
      case 'd' | 'D' =>
        if (unicodeCharacterClass) UnicodeDigit
        else ASCIIDigit
      case 'h' | 'H' =>
        UniversalHorizontalWhiteSpace
      case 's' | 'S' =>
        if (unicodeCharacterClass) UnicodeWhitespace
        else ASCIIWhiteSpace
      case 'v' | 'V' =>
        UniversalVerticalWhiteSpace
      case 'w' | 'W' =>
        wordCharClass
      case 'p' | 'P' =>
        parsePCharacterClass()
    }

    if (dispatchChar >= 'a') // cheap isLower
      positive
    else
      positive.negated
  }

  /** Parses and returns a translated version of a `\p` character class. */
  private def parsePCharacterClass(): CompiledCharClass = {
    val pattern = this.pattern // local copy
    val len = pattern.length()

    val start = pIndex
    val property = if (start == len) {
      "?" // mimics the behavior of the JVM
    } else if (pattern.charAt(start) == '{') {
      val innerStart = start + 1
      val innerEnd = pattern.indexOf("}", innerStart)
      if (innerEnd < 0)
        parseError("Unclosed character family")
      pIndex = innerEnd
      pattern.jsSubstring(innerStart, innerEnd)
    } else {
      pattern.jsSubstring(start, start + 1)
    }

    val result = if (!unicodeCharacterClass && dictContains(asciiPOSIXCharacterClasses, property)) {
      val property2 =
        if (asciiCaseInsensitive && (property == "Lower" || property == "Upper")) "Alpha"
        else property
      dictRawApply(asciiPOSIXCharacterClasses, property2)
    } else {
      // For anything else, we need built-in support for \p
      requireES2018Features("Unicode character family")

      mapGetOrElse(predefinedPCharacterClasses, property) {
        val scriptPrefixLen = if (property.startsWith("Is")) {
          2
        } else if (property.startsWith("sc=")) {
          3
        } else if (property.startsWith("script=")) {
          7
        } else if (property.startsWith("In") || property.startsWith("blk=") || property.startsWith("block=")) {
          parseError("Blocks are not supported in \\p Unicode character families")
        } else {
          // Error
          parseError(s"Unknown Unicode character class '$property'")
        }
        CompiledCharClass.posP("sc=" + canonicalizeScriptName(property.jsSubstring(scriptPrefixLen)))
      }
    }

    pIndex += 1

    result
  }

  /** Validates a script name and canonicalizes its casing.
   *
   *  The JDK regexps compare script names while ignoring case, but JavaScript
   *  requires the canonical name.
   *
   *  After canonicalizing the script name, we try to create a `js.RegExp` that
   *  uses it. If that fails, we report the (original) script name as unknown.
   */
  private def canonicalizeScriptName(scriptName: String): String = {
    import js.JSStringOps._

    val lowercase = scriptName.toLowerCase()

    mapGetOrElseUpdate(canonicalizedScriptNameCache, lowercase) {
      val canonical = lowercase.jsReplace(scriptCanonicalizeRegExp,
          ((s: String) => s.toUpperCase()): js.Function1[String, String])

      try {
        new js.RegExp(s"\\p{sc=$canonical}", "u")
      } catch {
        case _: Throwable =>
          parseError(s"Unknown character script name {$scriptName}")
      }

      canonical
    }
  }

  private def compileCharacterClass(): String = {
    // scalastyle:off return
    // the 'return' is in the case ']'

    val pattern = PatternCompiler.this.pattern // local copy
    val len = pattern.length()

    pIndex += 1 // skip '['

    /* If there is a leading '^' right after the '[', the whole class is
     * negated. In a sense, '^' is the operator with the lowest precedence.
     */
    val isNegated = pIndex != len && pattern.charAt(pIndex) == '^'
    if (isNegated)
      pIndex += 1

    val builder = new CharacterClassBuilder(asciiCaseInsensitive, isNegated)

    while (pIndex != len) {
      def processRangeOrSingleCodePoint(startCodePoint: Int): Unit = {
        if (comments)
          skipComments()

        if (pIndex != len && pattern.charAt(pIndex) == '-') {
          // Perhaps a range of code points, unless the '-' is followed by '[' or ']'
          pIndex += 1
          if (comments)
            skipComments()

          if (pIndex == len)
            parseError("Unclosed character class")

          val cpEnd = pattern.codePointAt(pIndex)

          if (cpEnd == '[' || cpEnd == ']') {
            // Oops, it wasn't a range after all
            builder.addSingleCodePoint(startCodePoint)
            builder.addSingleCodePoint('-')
          } else {
            // Range of code points
            pIndex += charCount(cpEnd)
            val endCodePoint =
              if (cpEnd == '\\') parseSingleCodePointEscape()
              else cpEnd
            if (endCodePoint < startCodePoint)
              parseError("Illegal character range")
            builder.addCodePointRange(startCodePoint, endCodePoint)
          }
        } else {
          // Single code point
          builder.addSingleCodePoint(startCodePoint)
        }
      }

      (pattern.codePointAt(pIndex): @switch) match {
        case ']' =>
          pIndex += 1
          return builder.finish()

        case '&' =>
          pIndex += 1
          if (pIndex != len && pattern.charAt(pIndex) == '&') {
            pIndex += 1
            builder.startNewConjunct()
          } else {
            processRangeOrSingleCodePoint('&')
          }

        case '[' =>
          builder.addCharacterClass(compileCharacterClass())

        case '\\' =>
          pIndex += 1
          if (pIndex == len)
            parseError("Illegal escape sequence")
          val c2 = pattern.charAt(pIndex)
          (c2: @switch) match {
            case 'd' | 'D' | 'h' | 'H' | 's' | 'S' | 'v' | 'V' | 'w' | 'W' | 'p' | 'P' =>
              builder.addCharacterClass(parsePredefinedCharacterClass(c2))

            case 'Q' =>
              pIndex += 1
              val end = pattern.indexOf("\\E", pIndex)
              if (end < 0)
                parseError("Unclosed character class")
              builder.addCodePointsInString(pattern, pIndex, end)
              pIndex = end + 2 // for the \E

            case _ =>
              processRangeOrSingleCodePoint(parseSingleCodePointEscape())
          }

        case ' ' | '\t' | '\n' | '\u000B' | '\f' | '\r' if comments =>
          pIndex += 1
        case '#' if comments =>
          skipSharpComment()

        case codePoint =>
          pIndex += charCount(codePoint)
          processRangeOrSingleCodePoint(codePoint)
      }
    }

    parseError("Unclosed character class")
    // scalastyle:on return
  }

  private def compileGroup(): String = {
    val pattern = this.pattern // local copy
    val len = pattern.length()

    val start = pIndex

    if (start + 1 == len || pattern.charAt(start + 1) != '?') {
      // Numbered capturing group
      pIndex = start + 1
      compiledGroupCount += 1
      groupNumberMap.push(compiledGroupCount)
      "(" + compileInsideGroup() + ")"
    } else {
      if (start + 2 == len)
        parseError("Unclosed group")

      val c1 = pattern.charAt(start + 2)

      if (c1 == ':' || c1 == '=' || c1 == '!') {
        // Non-capturing group or look-ahead
        pIndex = start + 3
        pattern.jsSubstring(start, start + 3) + compileInsideGroup() + ")"
      } else if (c1 == '<') {
        if (start + 3 == len)
          parseError("Unclosed group")

        val c2 = pattern.charAt(start + 3)

        if (isLetter(c2)) {
          // Named capturing group
          pIndex = start + 3
          val name = parseGroupName()
          if (dictContains(namedGroups, name))
            parseError(s"named capturing group <$name> is already defined")
          compiledGroupCount += 1
          groupNumberMap.push(compiledGroupCount) // this changes originalGroupCount
          dictSet(namedGroups, name, originalGroupCount)
          pIndex += 1
          "(" + compileInsideGroup() + ")"
        } else {
          // Look-behind group
          if (c2 != '=' && c2 != '!')
            parseError("Unknown look-behind group")
          requireES2018Features("Look-behind group")
          pIndex = start + 4
          pattern.jsSubstring(start, start + 4) + compileInsideGroup() + ")"
        }
      } else if (c1 == '>') {
        // Atomic group
        pIndex = start + 3
        compiledGroupCount += 1
        val groupNumber = compiledGroupCount
        s"(?:(?=(${compileInsideGroup()}))\\$groupNumber)"
      } else {
        parseError("Embedded flag expression in the middle of a pattern is not supported")
      }
    }
  }

  /** Parses a group name.
   *
   *  Pre: `pIndex` should point right after the opening '<'.
   *
   *  Post: `pIndex` points right before the closing '>' (it is guaranteed to be a '>').
   */
  private def parseGroupName(): String = {
    val pattern = this.pattern // local copy
    val len = pattern.length()
    val start = pIndex
    while (pIndex != len && isLetterOrDigit(pattern.charAt(pIndex)))
      pIndex += 1
    if (pIndex == len || pattern.charAt(pIndex) != '>')
      parseError("named capturing group is missing trailing '>'")
    pattern.jsSubstring(start, pIndex)
  }
}
