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

package java.lang

import scala.annotation.{switch, tailrec}

import java.util.Comparator

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.JSStringOps.enableJSStringOps
import scala.scalajs.LinkingInfo
import scala.scalajs.LinkingInfo.ESVersion

import java.lang.constant.{Constable, ConstantDesc}
import java.nio.ByteBuffer
import java.nio.charset.Charset
import java.util.Locale
import java.util.function._
import java.util.regex._

/* This is the implementation of java.lang.String, which is a hijacked class.
 * Its instances are primitive strings. Constructors are not emitted.
 *
 * It should be declared as `class String`, but scalac really does not like
 * being forced to compile java.lang.String, so we call it `_String` instead.
 * The Scala.js compiler back-end applies some magic to rename it into `String`
 * when emitting the IR.
 */
final class _String private () // scalastyle:ignore
    extends AnyRef with java.io.Serializable with Comparable[String]
    with CharSequence with Constable with ConstantDesc {

  import _String._

  @inline
  private def thisString: String =
    this.asInstanceOf[String]

  @inline
  def length(): Int =
    throw new Error("stub") // body replaced by the compiler back-end

  @inline
  def charAt(index: Int): Char =
    throw new Error("stub") // body replaced by the compiler back-end

  // Wasm intrinsic
  def codePointAt(index: Int): Int = {
    charAt(index) // bounds check
    this.asInstanceOf[js.Dynamic].codePointAt(index).asInstanceOf[Int]
  }

  @noinline
  def codePointBefore(index: Int): Int =
    Character.codePointBeforeImpl(this, index)

  @noinline
  def codePointCount(beginIndex: Int, endIndex: Int): Int =
    Character.codePointCountImpl(this, beginIndex, endIndex)

  @noinline
  def offsetByCodePoints(index: Int, codePointOffset: Int): Int =
    Character.offsetByCodePointsImpl(this, index, codePointOffset)

  override def hashCode(): Int = {
    var res = 0
    var mul = 1 // holds pow(31, length-i-1)
    var i = length() - 1
    while (i >= 0) {
      res += charAt(i) * mul
      mul *= 31
      i -= 1
    }
    res
  }

  @inline
  override def equals(that: Any): scala.Boolean =
    this eq that.asInstanceOf[AnyRef]

  def compareTo(anotherString: String): Int = {
    // scalastyle:off return
    val thisLength = this.length()
    val strLength = anotherString.length()
    val minLength = Math.min(thisLength, strLength)

    var i = 0
    while (i != minLength) {
      val cmp = this.charAt(i) - anotherString.charAt(i)
      if (cmp != 0)
        return cmp
      i += 1
    }
    thisLength - strLength
    // scalastyle:on return
  }

  def compareToIgnoreCase(str: String): Int = {
    // scalastyle:off return
    val thisLength = this.length()
    val strLength = str.length()
    val minLength = Math.min(thisLength, strLength)

    var i = 0
    while (i != minLength) {
      val cmp = caseFold(this.charAt(i)) - caseFold(str.charAt(i))
      if (cmp != 0)
        return cmp
      i += 1
    }
    thisLength - strLength
    // scalastyle:on return
  }

  @inline
  def equalsIgnoreCase(anotherString: String): scala.Boolean = {
    // scalastyle:off return
    val len = length()
    if (anotherString == null || anotherString.length() != len) {
      false
    } else {
      var i = 0
      while (i != len) {
        if (caseFold(this.charAt(i)) != caseFold(anotherString.charAt(i)))
          return false
        i += 1
      }
      true
    }
    // scalastyle:on return
  }

  /** Performs case folding of a single character for use by `equalsIgnoreCase`
   *  and `compareToIgnoreCase`.
   *
   *  This implementation respects the specification of those two methods,
   *  although that behavior does not generally conform to Unicode Case
   *  Folding.
   */
  @inline private def caseFold(c: Char): Char =
    Character.toLowerCase(Character.toUpperCase(c))

  @inline
  def concat(s: String): String =
    thisString + s

  @inline
  def contains(s: CharSequence): scala.Boolean =
    indexOf(s.toString) != -1

  @inline
  def endsWith(suffix: String): scala.Boolean = {
    suffix.getClass() // null check
    thisString.asInstanceOf[js.Dynamic].endsWith(suffix).asInstanceOf[scala.Boolean]
  }

  def getBytes(): Array[scala.Byte] =
    getBytes(Charset.defaultCharset())

  def getBytes(charsetName: String): Array[scala.Byte] =
    getBytes(Charset.forName(charsetName))

  def getBytes(charset: Charset): Array[scala.Byte] = {
    val buf = charset.encode(thisString)
    val res = new Array[scala.Byte](buf.remaining())
    buf.get(res)
    res
  }

  def getChars(srcBegin: Int, srcEnd: Int, dst: Array[Char],
      dstBegin: Int): Unit = {
    if (srcEnd > length() || srcBegin < 0 || srcEnd < 0 || srcBegin > srcEnd)
      throw new StringIndexOutOfBoundsException("Index out of Bound")

    val offset = dstBegin - srcBegin
    var i = srcBegin
    while (i < srcEnd) {
      dst(i + offset) = charAt(i)
      i += 1
    }
  }

  def indexOf(ch: Int): Int =
    indexOf(Character.toString(ch))

  def indexOf(ch: Int, fromIndex: Int): Int =
    indexOf(Character.toString(ch), fromIndex)

  @inline
  def indexOf(str: String): Int =
    thisString.jsIndexOf(str)

  @inline
  def indexOf(str: String, fromIndex: Int): Int =
    thisString.jsIndexOf(str, fromIndex)

  /* Just returning this string is a valid implementation for `intern` in
   * JavaScript, since strings are primitive values. Therefore, value equality
   * and reference equality is the same.
   */
  @inline
  def intern(): String = thisString

  @inline
  def isEmpty(): scala.Boolean = (this: AnyRef) eq ("": AnyRef)

  def lastIndexOf(ch: Int): Int =
    lastIndexOf(Character.toString(ch))

  def lastIndexOf(ch: Int, fromIndex: Int): Int =
    if (fromIndex < 0) -1
    else lastIndexOf(Character.toString(ch), fromIndex)

  @inline
  def lastIndexOf(str: String): Int =
    thisString.jsLastIndexOf(str)

  @inline
  def lastIndexOf(str: String, fromIndex: Int): Int =
    if (fromIndex < 0) -1
    else thisString.jsLastIndexOf(str, fromIndex)

  @inline
  def matches(regex: String): scala.Boolean =
    Pattern.matches(regex, thisString)

  /* Both regionMatches ported from
   * https://github.com/gwtproject/gwt/blob/master/user/super/com/google/gwt/emul/java/lang/String.java
   */
  def regionMatches(ignoreCase: scala.Boolean, toffset: Int, other: String,
      ooffset: Int, len: Int): scala.Boolean = {
    if (other == null) {
      throw new NullPointerException()
    } else if (toffset < 0 || ooffset < 0 || len > this.length() - toffset ||
        len > other.length() - ooffset) {
      false
    } else if (len <= 0) {
      true
    } else {
      val left = this.substring(toffset, toffset + len)
      val right = other.substring(ooffset, ooffset + len)
      if (ignoreCase) left.equalsIgnoreCase(right) else left == right
    }
  }

  @inline
  def regionMatches(toffset: Int, other: String, ooffset: Int,
      len: Int): scala.Boolean = {
    regionMatches(false, toffset, other, ooffset, len)
  }

  def repeat(count: Int): String = {
    if (count < 0)
      throw new IllegalArgumentException()

    /* This will throw a `js.RangeError` if `count` is too large, instead of
     * an `OutOfMemoryError`. That's fine because the behavior of `repeat` is
     * not specified for `count` too large.
     */
    this.asInstanceOf[js.Dynamic].repeat(count).asInstanceOf[String]
  }

  @inline
  def replace(oldChar: Char, newChar: Char): String =
    replace(oldChar.toString, newChar.toString)

  @inline
  def replace(target: CharSequence, replacement: CharSequence): String =
    thisString.jsSplit(target.toString).join(replacement.toString)

  def replaceAll(regex: String, replacement: String): String =
    Pattern.compile(regex).matcher(thisString).replaceAll(replacement)

  def replaceFirst(regex: String, replacement: String): String =
    Pattern.compile(regex).matcher(thisString).replaceFirst(replacement)

  @inline
  def split(regex: String): Array[String] =
    split(regex, 0)

  def split(regex: String, limit: Int): Array[String] =
    Pattern.compile(regex).split(thisString, limit)

  @inline
  def startsWith(prefix: String): scala.Boolean = {
    prefix.getClass() // null check
    thisString.asInstanceOf[js.Dynamic].startsWith(prefix).asInstanceOf[scala.Boolean]
  }

  @inline
  def startsWith(prefix: String, toffset: Int): scala.Boolean = {
    prefix.getClass() // null check
    (toffset <= length() && toffset >= 0 &&
        thisString.asInstanceOf[js.Dynamic].startsWith(prefix, toffset).asInstanceOf[scala.Boolean])
  }

  @inline
  def subSequence(beginIndex: Int, endIndex: Int): CharSequence =
    substring(beginIndex, endIndex)

  // Wasm intrinsic
  @inline
  def substring(beginIndex: Int): String = {
    // Bounds check
    if (beginIndex < 0 || beginIndex > length())
      charAt(beginIndex)

    thisString.jsSubstring(beginIndex)
  }

  // Wasm intrinsic
  @inline
  def substring(beginIndex: Int, endIndex: Int): String = {
    // Bounds check
    if (beginIndex < 0)
      charAt(beginIndex)
    if (endIndex > length())
      charAt(endIndex)
    if (endIndex < beginIndex)
      charAt(-1)

    thisString.jsSubstring(beginIndex, endIndex)
  }

  def toCharArray(): Array[Char] = {
    val len = length()
    val result = new Array[Char](len)
    var i = 0
    while (i < len) {
      result(i) = charAt(i)
      i += 1
    }
    result
  }

  /* toLowerCase() and toUpperCase()
   *
   * The overloads without an explicit locale use the default locale, which is
   * the root locale by specification. They are implemented by direct
   * delegation to ECMAScript's `toLowerCase()` and `toUpperCase()`, which are
   * specified as locale-insensitive, therefore equivalent to the root locale.
   *
   * It turns out virtually every locale behaves in the same way as the root
   * locale for default case algorithms. Only Lithuanian (lt), Turkish (tr)
   * and Azeri (az) have different behaviors.
   *
   * The overloads with a `Locale` specifically test for those three languages
   * and delegate to dedicated methods to handle them. Those methods start by
   * handling their respective special cases, then delegate to the locale-
   * insensitive version. The special cases are specified in the Unicode
   * reference file at
   *
   *   https://unicode.org/Public/13.0.0/ucd/SpecialCasing.txt
   *
   * That file first contains a bunch of locale-insensitive special cases,
   * which we do not need to handle. Only the last two sections about locale-
   * sensitive special-cases are important for us.
   *
   * Some of the rules are further context-sensitive, using predicates that are
   * defined in Section 3.13 "Default Case Algorithms" of the Unicode Standard,
   * available at
   *
   *   http://www.unicode.org/versions/Unicode13.0.0/
   *
   * We based the implementations on Unicode 13.0.0. It is worth noting that
   * there has been no non-comment changes in the SpecialCasing.txt file
   * between Unicode 4.1.0 and 13.0.0 (perhaps even earlier; the version 4.1.0
   * is the earliest that is easily accessible).
   */

  def toLowerCase(locale: Locale): String = {
    locale.getLanguage() match {
      case "lt"        => toLowerCaseLithuanian()
      case "tr" | "az" => toLowerCaseTurkishAndAzeri()
      case _           => toLowerCase()
    }
  }

  private def toLowerCaseLithuanian(): String = {
    /* Relevant excerpt from SpecialCasing.txt
     *
     * # Lithuanian
     *
     * # Lithuanian retains the dot in a lowercase i when followed by accents.
     *
     * [...]
     *
     * # Introduce an explicit dot above when lowercasing capital I's and J's
     * # whenever there are more accents above.
     * # (of the accents used in Lithuanian: grave, acute, tilde above, and ogonek)
     *
     * 0049; 0069 0307; 0049; 0049; lt More_Above; # LATIN CAPITAL LETTER I
     * 004A; 006A 0307; 004A; 004A; lt More_Above; # LATIN CAPITAL LETTER J
     * 012E; 012F 0307; 012E; 012E; lt More_Above; # LATIN CAPITAL LETTER I WITH OGONEK
     * 00CC; 0069 0307 0300; 00CC; 00CC; lt; # LATIN CAPITAL LETTER I WITH GRAVE
     * 00CD; 0069 0307 0301; 00CD; 00CD; lt; # LATIN CAPITAL LETTER I WITH ACUTE
     * 0128; 0069 0307 0303; 0128; 0128; lt; # LATIN CAPITAL LETTER I WITH TILDE
     */

    /* Tests whether we are in an `More_Above` context.
     * From Table 3.17 in the Unicode standard:
     * - Description: C is followed by a character of combining class
     *   230 (Above) with no intervening character of combining class 0 or
     *   230 (Above).
     * - Regex, after C: [^\p{ccc=230}\p{ccc=0}]*[\p{ccc=230}]
     */
    def moreAbove(i: Int): scala.Boolean = {
      import Character._
      val len = length()

      @tailrec def loop(j: Int): scala.Boolean = {
        if (j == len) {
          false
        } else {
          val cp = this.codePointAt(j)
          combiningClassNoneOrAboveOrOther(cp) match {
            case CombiningClassIsNone  => false
            case CombiningClassIsAbove => true
            case _                     => loop(j + charCount(cp))
          }
        }
      }

      loop(i + 1)
    }

    val preprocessed = replaceCharsAtIndex { i =>
      (this.charAt(i): @switch) match {
        case '\u0049' if moreAbove(i) => "\u0069\u0307"
        case '\u004A' if moreAbove(i) => "\u006A\u0307"
        case '\u012E' if moreAbove(i) => "\u012F\u0307"
        case '\u00CC'                 => "\u0069\u0307\u0300"
        case '\u00CD'                 => "\u0069\u0307\u0301"
        case '\u0128'                 => "\u0069\u0307\u0303"
        case _                        => null
      }
    }

    preprocessed.toLowerCase()
  }

  private def toLowerCaseTurkishAndAzeri(): String = {
    /* Relevant excerpt from SpecialCasing.txt
     *
     * # Turkish and Azeri
     *
     * # I and i-dotless; I-dot and i are case pairs in Turkish and Azeri
     * # The following rules handle those cases.
     *
     * 0130; 0069; 0130; 0130; tr; # LATIN CAPITAL LETTER I WITH DOT ABOVE
     * 0130; 0069; 0130; 0130; az; # LATIN CAPITAL LETTER I WITH DOT ABOVE
     *
     * # When lowercasing, remove dot_above in the sequence I + dot_above, which will turn into i.
     * # This matches the behavior of the canonically equivalent I-dot_above
     *
     * 0307; ; 0307; 0307; tr After_I; # COMBINING DOT ABOVE
     * 0307; ; 0307; 0307; az After_I; # COMBINING DOT ABOVE
     *
     * # When lowercasing, unless an I is before a dot_above, it turns into a dotless i.
     *
     * 0049; 0131; 0049; 0049; tr Not_Before_Dot; # LATIN CAPITAL LETTER I
     * 0049; 0131; 0049; 0049; az Not_Before_Dot; # LATIN CAPITAL LETTER I
     */

    /* Tests whether we are in an `After_I` context.
     * From Table 3.17 in the Unicode standard:
     * - Description: There is an uppercase I before C, and there is no
     *   intervening combining character class 230 (Above) or 0.
     * - Regex, before C: [I]([^\p{ccc=230}\p{ccc=0}])*
     */
    def afterI(i: Int): scala.Boolean = {
      val j = skipCharsWithCombiningClassOtherThanNoneOrAboveBackwards(i)
      j > 0 && charAt(j - 1) == 'I'
    }

    /* Tests whether we are in an `Before_Dot` context.
     * From Table 3.17 in the Unicode standard:
     * - Description: C is followed by combining dot above (U+0307). Any
     *   sequence of characters with a combining class that is neither 0 nor
     *   230 may intervene between the current character and the combining dot
     *   above.
     * - Regex, after C: ([^\p{ccc=230}\p{ccc=0}])*[\u0307]
     */
    def beforeDot(i: Int): scala.Boolean = {
      val j = skipCharsWithCombiningClassOtherThanNoneOrAboveForwards(i + 1)
      j != length() && charAt(j) == '\u0307'
    }

    val preprocessed = replaceCharsAtIndex { i =>
      (this.charAt(i): @switch) match {
        case '\u0130'                  => "\u0069"
        case '\u0307' if afterI(i)     => ""
        case '\u0049' if !beforeDot(i) => "\u0131"
        case _                         => null
      }
    }

    preprocessed.toLowerCase()
  }

  @inline
  def toLowerCase(): String =
    this.asInstanceOf[js.Dynamic].toLowerCase().asInstanceOf[String]

  def toUpperCase(locale: Locale): String = {
    locale.getLanguage() match {
      case "lt"        => toUpperCaseLithuanian()
      case "tr" | "az" => toUpperCaseTurkishAndAzeri()
      case _           => toUpperCase()
    }
  }

  private def toUpperCaseLithuanian(): String = {
    /* Relevant excerpt from SpecialCasing.txt
     *
     * # Lithuanian
     *
     * # Lithuanian retains the dot in a lowercase i when followed by accents.
     *
     * # Remove DOT ABOVE after "i" with upper or titlecase
     *
     * 0307; 0307; ; ; lt After_Soft_Dotted; # COMBINING DOT ABOVE
     */

    /* Tests whether we are in an `After_Soft_Dotted` context.
     * From Table 3.17 in the Unicode standard:
     * - Description: There is a Soft_Dotted character before C, with no
     *   intervening character of combining class 0 or 230 (Above).
     * - Regex, before C: [\p{Soft_Dotted}]([^\p{ccc=230} \p{ccc=0}])*
     *
     * According to https://unicode.org/Public/13.0.0/ucd/PropList.txt, there
     * are 44 code points with the Soft_Dotted property. However,
     * experimentation on the JVM reveals that the JDK (8 and 14 were tested)
     * only recognizes 8 code points when deciding whether to remove the 0x0307
     * code points. The following script reproduces the list:

for (cp <- 0 to Character.MAX_CODE_POINT) {
  val input = new String(Array(cp, 0x0307, 0x0301), 0, 3)
  val output = input.toUpperCase(new java.util.Locale("lt"))
  if (!output.contains('\u0307'))
    println(cp.toHexString)
}

     */
    def afterSoftDotted(i: Int): scala.Boolean = {
      val j = skipCharsWithCombiningClassOtherThanNoneOrAboveBackwards(i)
      j > 0 && (codePointBefore(j) match {
        case 0x0069 | 0x006a | 0x012f | 0x0268 | 0x0456 | 0x0458 | 0x1e2d | 0x1ecb => true
        case _                                                                     => false
      })
    }

    val preprocessed = replaceCharsAtIndex { i =>
      (this.charAt(i): @switch) match {
        case '\u0307' if afterSoftDotted(i) => ""
        case _                              => null
      }
    }

    preprocessed.toUpperCase()
  }

  private def toUpperCaseTurkishAndAzeri(): String = {
    /* Relevant excerpt from SpecialCasing.txt
     *
     * # Turkish and Azeri
     *
     * # When uppercasing, i turns into a dotted capital I
     *
     * 0069; 0069; 0130; 0130; tr; # LATIN SMALL LETTER I
     * 0069; 0069; 0130; 0130; az; # LATIN SMALL LETTER I
     */

    val preprocessed = replaceCharsAtIndex { i =>
      (this.charAt(i): @switch) match {
        case '\u0069' => "\u0130"
        case _        => null
      }
    }

    preprocessed.toUpperCase()
  }

  @inline
  def toUpperCase(): String =
    this.asInstanceOf[js.Dynamic].toUpperCase().asInstanceOf[String]

  /** Replaces special characters in this string (possibly in special contexts)
   *  by dedicated strings.
   *
   *  This method encodes the general pattern of
   *
   *  - `toLowerCaseLithuanian()`
   *  - `toLowerCaseTurkishAndAzeri()`
   *  - `toUpperCaseLithuanian()`
   *  - `toUpperCaseTurkishAndAzeri()`
   *
   *  @param replacementAtIndex
   *    A function from index to `String | Null`, which should return a special
   *    replacement string for the character at the given index, or `null` if
   *    the character at the given index is not special.
   */
  @inline
  private def replaceCharsAtIndex(replacementAtIndex: IntFunction[String]): String = {
    var prep = ""
    val len = this.length()
    var i = 0
    var startOfSegment = 0

    while (i != len) {
      val replacement = replacementAtIndex(i)
      if (replacement != null) {
        prep += this.substring(startOfSegment, i)
        prep += replacement
        startOfSegment = i + 1
      }
      i += 1
    }

    if (startOfSegment == 0)
      thisString // opt: no character needed replacing, directly return the original string
    else
      prep + this.substring(startOfSegment, i)
  }

  private def skipCharsWithCombiningClassOtherThanNoneOrAboveForwards(i: Int): Int = {
    // scalastyle:off return
    import Character._
    val len = length()
    var j = i
    while (j != len) {
      val cp = this.codePointAt(j)
      if (combiningClassNoneOrAboveOrOther(cp) != CombiningClassIsOther)
        return j
      j += charCount(cp)
    }
    j
    // scalastyle:on return
  }

  private def skipCharsWithCombiningClassOtherThanNoneOrAboveBackwards(i: Int): Int = {
    // scalastyle:off return
    import Character._
    var j = i
    while (j > 0) {
      val cp = this.codePointBefore(j)
      if (combiningClassNoneOrAboveOrOther(cp) != CombiningClassIsOther)
        return j
      j -= charCount(cp)
    }
    0
    // scalastyle:on return
  }

  def trim(): String = {
    val len = length()
    var start = 0
    while (start != len && charAt(start) <= ' ')
      start += 1
    if (start == len) {
      ""
    } else {
      /* If we get here, 0 <= start < len, so the original string is not empty.
       * We also know that charAt(start) > ' '.
       */
      var end = len
      while (charAt(end - 1) <= ' ') // no need for a bounds check here since charAt(start) > ' '
        end -= 1
      if (start == 0 && end == len) thisString
      else substring(start, end)
    }
  }

  def stripLeading(): String = {
    val len = length()
    var idx = 0
    while (idx < len && Character.isWhitespace(charAt(idx)))
      idx += 1
    substring(idx)
  }

  def stripTrailing(): String = {
    val len = length()
    var idx = len - 1
    while (idx >= 0 && Character.isWhitespace(charAt(idx)))
      idx -= 1
    substring(0, idx + 1)
  }

  def strip(): String = {
    val len = length()
    var leading = 0
    while (leading < len && Character.isWhitespace(charAt(leading)))
      leading += 1
    if (leading == len) {
      ""
    } else {
      var trailing = len
      while (Character.isWhitespace(charAt(trailing - 1)))
        trailing -= 1
      if (leading == 0 && trailing == len) thisString
      else substring(leading, trailing)
    }
  }

  def isBlank(): scala.Boolean = {
    val len = length()
    var start = 0
    while (start != len && Character.isWhitespace(charAt(start)))
      start += 1
    start == len
  }

  private def splitLines(): js.Array[String] = {
    val xs = js.Array[String]()
    val len = length()
    var idx = 0
    var last = 0
    while (idx < len) {
      val c = charAt(idx)
      if (c == '\n' || c == '\r') {
        xs.push(substring(last, idx))
        if (c == '\r' && idx + 1 < len && charAt(idx + 1) == '\n')
          idx += 1
        last = idx + 1
      }
      idx += 1
    }
    // make sure we add the last segment, but not the last new line
    if (last != len)
      xs.push(substring(last))
    xs
  }

  def indent(n: Int): String = {

    def forEachLn(f: Function[String, String]): String = {
      var out = ""
      var i = 0
      val xs = splitLines()
      while (i < xs.length) {
        out += f(xs(i)) + "\n"
        i += 1
      }
      out
    }

    if (n < 0) {
      forEachLn { l =>
        // n is negative here
        var idx = 0
        val lim = if (l.length() <= -n) l.length() else -n
        while (idx < lim && Character.isWhitespace(l.charAt(idx)))
          idx += 1
        l.substring(idx)
      }
    } else {
      val padding = " ".asInstanceOf[_String].repeat(n)
      forEachLn(padding + _)
    }
  }

  def stripIndent(): String = {
    if (isEmpty()) {
      ""
    } else {
      import Character.{isWhitespace => isWS}
      // splitLines discards the last NL if it's empty so we identify it here first
      val trailingNL = charAt(length() - 1) match {
        // this also covers the \r\n case via the last \n
        case '\r' | '\n' => true
        case _           => false
      }

      val xs = splitLines()
      var i = 0
      var minLeading = Int.MaxValue

      while (i < xs.length) {
        val l = xs(i)
        // count the last line even if blank
        if (i == xs.length - 1 || !l.asInstanceOf[_String].isBlank()) {
          var idx = 0
          while (idx < l.length() && isWS(l.charAt(idx)))
            idx += 1
          if (idx < minLeading)
            minLeading = idx
        }
        i += 1
      }
      // if trailingNL, then the last line is zero width
      if (trailingNL || minLeading == Int.MaxValue)
        minLeading = 0

      var out = ""
      var j = 0
      while (j < xs.length) {
        val line = xs(j)
        if (!line.asInstanceOf[_String].isBlank()) {
          // we strip the computed leading WS and also any *trailing* WS
          out += line.substring(minLeading).asInstanceOf[_String].stripTrailing()
        }
        // different from indent, we don't add an LF at the end unless there's already one
        if (j != xs.length - 1)
          out += "\n"
        j += 1
      }
      if (trailingNL)
        out += "\n"
      out
    }
  }

  def translateEscapes(): String = {
    def isOctalDigit(c: Char): scala.Boolean = c >= '0' && c <= '7'
    def isValidIndex(n: Int): scala.Boolean = n < length()
    var i = 0
    var result = ""
    while (i < length()) {
      if (charAt(i) == '\\') {
        if (isValidIndex(i + 1)) {
          charAt(i + 1) match {
            // <line-terminator>, so CR(\r), LF(\n), or CRLF(\r\n)
            case '\r' if isValidIndex(i + 2) && charAt(i + 2) == '\n' =>
              i += 1 // skip \r and \n and discard, so 2+1 chars
            case '\r' | '\n' => // skip and discard

            // normal one char escapes
            case 'b'  => result += "\b"
            case 't'  => result += "\t"
            case 'n'  => result += "\n"
            case 'f'  => result += "\f"
            case 'r'  => result += "\r"
            case 's'  => result += " "
            case '"'  => result += "\""
            case '\'' => result += "\'"
            case '\\' => result += "\\"

            // we're parsing octal now, as per JLS-3, we got three cases:
            // 1) [0-3][0-7][0-7]
            case a @ ('0' | '1' | '2' | '3')
                if isValidIndex(i + 3) && isOctalDigit(charAt(i + 2)) && isOctalDigit(charAt(i + 3)) =>
              val codePoint =
                ((a - '0') * 64) + ((charAt(i + 2) - '0') * 8) + (charAt(i + 3) - '0')
              result += codePoint.toChar
              i += 2 // skip two other numbers, so 2+2 chars
            // 2) [0-7][0-7]
            case a if isOctalDigit(a) && isValidIndex(i + 2) && isOctalDigit(charAt(i + 2)) =>
              val codePoint = ((a - '0') * 8) + (charAt(i + 2) - '0')
              result += codePoint.toChar
              i += 1 // skip one other number, so 2+1 chars
            // 3) [0-7]
            case a if isOctalDigit(a) =>
              val codePoint = a - '0'
              result += codePoint.toChar
            // bad escape otherwise, this catches everything else including the Unicode ones
            case bad =>
              throw new IllegalArgumentException(s"Illegal escape: `\\$bad`")
          }
          // skip ahead 2 chars (\ and the escape char) at minimum, cases above can add more if needed
          i += 2
        } else {
          throw new IllegalArgumentException("Illegal escape: `\\(end-of-string)`")
        }
      } else {
        result += charAt(i)
        i += 1
      }
    }
    result
  }

  def transform[R](f: java.util.function.Function[String, R]): R =
    f.apply(thisString)

  @inline
  override def toString(): String =
    thisString
}

object _String { // scalastyle:ignore
  final lazy val CASE_INSENSITIVE_ORDER: Comparator[String] = {
    new Comparator[String] with Serializable {
      def compare(o1: String, o2: String): Int = o1.compareToIgnoreCase(o2)
    }
  }

  // Constructors

  def `new`(): String = ""

  def `new`(value: Array[Char]): String =
    `new`(value, 0, value.length)

  def `new`(value: Array[Char], offset: Int, count: Int): String = {
    val end = offset + count
    if (offset < 0 || end < offset || end > value.length)
      throw new StringIndexOutOfBoundsException

    var result = ""
    var i = offset
    while (i != end) {
      result += value(i).toString
      i += 1
    }
    result
  }

  def `new`(bytes: Array[scala.Byte]): String =
    `new`(bytes, Charset.defaultCharset())

  def `new`(bytes: Array[scala.Byte], charsetName: String): String =
    `new`(bytes, Charset.forName(charsetName))

  def `new`(bytes: Array[scala.Byte], charset: Charset): String =
    charset.decode(ByteBuffer.wrap(bytes)).toString()

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int): String =
    `new`(bytes, offset, length, Charset.defaultCharset())

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int,
      charsetName: String): String =
    `new`(bytes, offset, length, Charset.forName(charsetName))

  def `new`(bytes: Array[scala.Byte], offset: Int, length: Int,
      charset: Charset): String =
    charset.decode(ByteBuffer.wrap(bytes, offset, length)).toString()

  def `new`(codePoints: Array[Int], offset: Int, count: Int): String = {
    val end = offset + count
    if (offset < 0 || end < offset || end > codePoints.length)
      throw new StringIndexOutOfBoundsException

    var result = ""
    var i = offset
    while (i != end) {
      result += Character.toString(codePoints(i))
      i += 1
    }
    result
  }

  def `new`(original: String): String = {
    if (original == null)
      throw new NullPointerException
    original
  }

  def `new`(buffer: java.lang.StringBuffer): String =
    buffer.toString

  def `new`(builder: java.lang.StringBuilder): String =
    builder.toString

  // Static methods (aka methods on the companion object)

  def valueOf(b: scala.Boolean): String = b.toString()
  def valueOf(c: scala.Char): String = c.toString()
  def valueOf(i: scala.Int): String = i.toString()
  def valueOf(l: scala.Long): String = l.toString()
  def valueOf(f: scala.Float): String = f.toString()
  def valueOf(d: scala.Double): String = d.toString()

  @inline def valueOf(obj: Object): String =
    "" + obj // if (obj eq null), returns "null"

  def valueOf(data: Array[Char]): String =
    valueOf(data, 0, data.length)

  def valueOf(data: Array[Char], offset: Int, count: Int): String =
    `new`(data, offset, count)

  def format(format: String, args: Array[AnyRef]): String =
    new java.util.Formatter().format(format, args).toString()

  def format(l: Locale, format: String, args: Array[AnyRef]): String =
    new java.util.Formatter(l).format(format, args).toString()

}
