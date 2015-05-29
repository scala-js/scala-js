/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import scala.scalajs.js
import scala.scalajs.js.JSConverters._

import scala.scalajs.niocharset.StandardCharsets

import org.scalajs.jasminetest.JasmineTest

object StringTest extends JasmineTest {

  describe("java.lang.String") {

    it("should respond to `length`") {
      expect("Scala.js".length).toEqual(8)
      expect("".length).toEqual(0)
    }

    it("should respond to `intern`") {
      val s = "Scala.js"
      expect(s.intern).toEqual(s)
    }

    it("should respond to `equals`") {
      expect("Scala.js".equals("Scala.js")).toBeTruthy
      expect("Scala.js".equals("Java")).toBeFalsy
    }

    it("should respond to `equalsIgnoreCase`") {
      expect("Scala.JS".equalsIgnoreCase("Scala.js")).toBeTruthy
      expect("åløb".equalsIgnoreCase("ÅLØb")).toBeTruthy
      expect("Scala.js".equalsIgnoreCase("Java")).toBeFalsy
      expect("Scala.js".equalsIgnoreCase(null)).toBeFalsy
    }

    it("should respond to `compareTo`") {
      expect("Scala.js".compareTo("Scala")).toBeGreaterThan(0)
      expect("Scala.js".compareTo("Scala.js")).toBe(0)
      expect("Scala.js".compareTo("banana")).toBeLessThan(0)
    }

    it("should respond to `compareToIgnoreCase`") {
      expect("Scala.JS".compareToIgnoreCase("Scala.js")).toBe(0)
      expect("Scala.JS".compareToIgnoreCase("scala")).toBeGreaterThan(0)
      expect("åløb".compareToIgnoreCase("ÅLØB")).toBe(0)
      expect("Java".compareToIgnoreCase("Scala")).toBeLessThan(0)
    }

    it("should respond to `isEmpty`") {
      expect("Scala.js".isEmpty).toBeFalsy
      expect("".isEmpty).toBeTruthy
    }

    it("should respond to `contains`") {
      expect("Scala.js".contains("Scala")).toBeTruthy
      expect("Scala.js".contains("Scala.js")).toBeTruthy
      expect("ananas".contains("na")).toBeTruthy
      expect("Scala.js".contains("scala")).toBeFalsy
    }

    it("should respond to `startWith`") {
      expect("Scala.js".startsWith("Scala")).toBeTruthy
      expect("Scala.js".startsWith("Scala.js")).toBeTruthy
      expect("Scala.js".startsWith("scala")).toBeFalsy
      expect("ananas".startsWith("an")).toBeTruthy
    }

    it("should respond to `endsWith`") {
      expect("Scala.js".endsWith("js")).toBeTruthy
      expect("Scala.js".endsWith("Scala.js")).toBeTruthy
      expect("Scala.js".endsWith("JS")).toBeFalsy
      expect("banana".endsWith("na")).toBeTruthy
    }

    it("should respond to `indexOf(String)`") {
      expect("Scala.js".indexOf("js")).toBe(6)
      expect("Scala.js".indexOf("Scala.js")).toBe(0)
      expect("ananas".indexOf("na")).toBe(1)
      expect("Scala.js".indexOf("Java")).toBe(-1)
    }

    it("should respond to `indexOf(int)`") {
      expect("abc\uD834\uDF06def\uD834\uDF06def".indexOf(0x61)).toEqual(0)
      expect("abc\uD834\uDF06def\uD834\uDF06def".indexOf(0x1D306)).toEqual(3)
      expect("abc\uD834\uDF06def\uD834\uDF06def".indexOf(0xD834)).toEqual(3)
      expect("abc\uD834\uDF06def\uD834\uDF06def".indexOf(0xDF06)).toEqual(4)
      expect("abc\uD834\uDF06def\uD834\uDF06def".indexOf(0x64)).toEqual(5)
    }

    it("should respond to `lastIndexOf(String)`") {
      expect("Scala.js".lastIndexOf("Scala.js")).toBe(0)
      expect("ananas".lastIndexOf("na")).toBe(3)
      expect("Scala.js".lastIndexOf("Java")).toBe(-1)
      expect("Negative index".lastIndexOf("N", -5)).toBe(-1)
    }

    it("should respond to `lastIndexOf(int)`") {
      expect("abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x61)).toEqual(0)
      expect("abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x1D306)).toEqual(8)
      expect("abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0xD834)).toEqual(8)
      expect("abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0xDF06)).toEqual(9)
      expect("abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x64)).toEqual(10)
      expect("abc\uD834\uDF06def\uD834\uDF06def".lastIndexOf(0x64, -1)).toEqual(-1)
    }

    it("should respond to `toUpperCase`") {
      expect("Scala.js".toUpperCase()).toBe("SCALA.JS")
    }

    it("should respond to `toLowerCase`") {
      expect("Scala.js".toLowerCase()).toBe("scala.js")
    }

    it("should respond to `charAt`") {
      expect("Scala.js".charAt(5)).toBe('.')
      expect("Scala.js".charAt(6)).not.toBe('.')
    }

    when("compliant-asinstanceofs").
    it("charAt() should throw with out-of-bound indices") {
      // Type Strings both as CharSequence and String. One will invoke the
      // helper, the other directly the method on RuntimeString.
      expect(() => ("Scala.js": CharSequence).charAt(-3)).toThrow
      expect(() => ("Scala.js": CharSequence).charAt(20)).toThrow
      expect(() => "Scala.js".charAt(-3)).toThrow
      expect(() => "Scala.js".charAt(20)).toThrow
    }

    it("should respond to `codePointAt`") {
      // String that starts with a BMP symbol
      expect("abc\uD834\uDF06def".codePointAt(0)).toEqual(0x61)
      expect("abc\uD834\uDF06def".codePointAt(3)).toEqual(0x1D306)
      expect("abc\uD834\uDF06def".codePointAt(4)).toEqual(0xDF06)
      expect("abc\uD834\uDF06def".codePointAt(5)).toEqual(0x64)

      // String that starts with an astral symbol
      expect("\uD834\uDF06def".codePointAt(0)).toEqual(0x1D306)
      expect("\uD834\uDF06def".codePointAt(1)).toEqual(0xDF06)

      // Lone high surrogates
      expect("\uD834abc".codePointAt(0)).toEqual(0xD834)

      // Lone low surrogates
      expect("\uDF06abc".codePointAt(0)).toEqual(0xDF06)
    }

    it("codePointCount") {
      val s = "abc\uD834\uDF06de\uD834\uDF06fgh\uD834ij\uDF06\uD834kl\uDF06"
      expect(s.codePointCount(0, s.length)).toEqual(18)
      expect(s.codePointCount(3, 5)).toEqual(1)
      expect(s.codePointCount(2, 3)).toEqual(1)
      expect(s.codePointCount(2, 4)).toEqual(2)
      expect(s.codePointCount(2, 5)).toEqual(2)
      expect(s.codePointCount(2, 6)).toEqual(3)
      expect(s.codePointCount(12, 17)).toEqual(5)
      expect(s.codePointCount(8, 10)).toEqual(2)
      expect(s.codePointCount(7, 10)).toEqual(2)
      expect(s.codePointCount(7, 7)).toEqual(0)
      expect(s.codePointCount(s.length - 1, s.length)).toEqual(1)
      expect(s.codePointCount(s.length - 1, s.length - 1)).toEqual(0)
      expect(s.codePointCount(s.length, s.length)).toEqual(0)

      expect(() => s.codePointCount(-3, 4)).toThrow
      expect(() => s.codePointCount(6, 2)).toThrow
      expect(() => s.codePointCount(10, 30)).toThrow
    }

    it("should respond to `subSequence`") {
      expect("Scala.js".subSequence(0, 5)).toBe("Scala")
      expect("Scala.js".subSequence(6, 8)).toBe("js")
      expect("Scala.js".subSequence(3, 5)).toBe("la")
      expect("Scala.js".subSequence(3, 3)).toBe("")
    }

    it("should respond to `replace`") {
      expect("Scala.js".replace(".js", "")).toBe("Scala")
      expect("Scala.js".replace("JS", "")).toBe("Scala.js")
      expect("aa".replace('a', 'b')).toBe("bb") // #25
    }

    it("should respond to `matches`") {
      expect("Scala.js".matches(".*js")).toBeTruthy
      expect("Scala.js".matches(".*JS")).toBeFalsy
    }

    it("should respond to `split`") {
      expect("Scala.js".split("a").toJSArray).toEqual(js.Array("Sc", "l", ".js"))
      expect("asdf".split("").toJSArray).toEqual(js.Array("","a","s","d","f"))
      expect("asdf".split("", -1).toJSArray).toEqual(js.Array("","a","s","d","f", ""))
    }

    it("should respond to `split` with char as argument") {
      expect("Scala.js".split('.').toJSArray).toEqual(js.Array("Scala","js"))
      for (i <- 0 to 32) {
        val c = i.toChar
        expect(s"blah${c}blah${c}blah${c}blah".split(c).toJSArray).toEqual(
          js.Array("blah", "blah", "blah", "blah"))
      }
    }

    it("startsWith(prefix, toffset) - #1603") {
      expect("Scala.js".startsWith("ala", 2)).toBeTruthy
      expect("Scala.js".startsWith("Scal", 0)).toBeTruthy

      expect("Scala.js".startsWith("", 3)).toBeTruthy
      expect("Scala.js".startsWith("", 0)).toBeTruthy
      expect("Scala.js".startsWith("", 8)).toBeTruthy

      expect("Scala.js".startsWith("ala", 0)).toBeFalsy
      expect("Scala.js".startsWith("Scal", 2)).toBeFalsy

      expect("Scala.js".startsWith("Sc", -1)).toBeFalsy
      expect("Scala.js".startsWith(".js", 10)).toBeFalsy
      expect("Scala.js".startsWith("", -1)).toBeFalsy
      expect("Scala.js".startsWith("", 9)).toBeFalsy
    }

    it("should respond to `toCharArray`") {
      expect("Scala.js".toCharArray()(5)).toEqual('.')
    }

    it("should respond to `hashCode`") {
      expect("a`jkxzcbfaslkjfbkj,289oinkasdf".hashCode()).toEqual(-1395193631)
      expect("-34".hashCode()).toEqual(44878)
      expect("".hashCode()).toEqual(0)
    }

    it("should respond to `getChars`") {
      val trg = new Array[Char](10)
      "asdf_foo".getChars(2, 6, trg, 3)
      val exp = Array(0,0,0,'d','f','_','f',0,0,0)

      for ((i,e) <- trg zip exp) {
        expect(i).toEqual(e)
      }
    }


    it("should respond to `concat`") {
      expect("asdf".concat("fdsa")).toEqual("asdffdsa")
    }

    it("should respond to constructors") {
      val charArray =
        Array('a', 'b', 'c', 'd', '\uD834', '\uDF06', 'e', 'f', 'g', 'h', 'i')
      val codePointArray =
        Array(65, 0x1D306, 67, 68, 0xD834, 69, 72, 0xDF06)

      expect(new String()).toEqual("")
      expect(new String(charArray)).toEqual("abcd\uD834\uDF06efghi")
      expect(new String(charArray, 3, 5)).toEqual("d\uD834\uDF06ef")
      expect(new String(codePointArray, 1, 5)).toEqual("\uD834\uDF06CD\uD834E")
      expect(new String("foo")).toEqual("foo")
      expect(new String(new StringBuffer("buffer-foo"))).toEqual("buffer-foo")
      expect(new String(new java.lang.StringBuilder("builder-foo"))
        ).toEqual("builder-foo")
    }

    it("should provide `format`") {
      expect(String.format("%d", new Integer(5))).toEqual("5")
      expect(String.format("%05d", new Integer(5))).toEqual("00005")
      expect(String.format("%0#5x", new Integer(5))).toEqual("0x005")
      expect(String.format("%#5x", new Integer(5))).toEqual("  0x5")
      expect(String.format("%#5X", new Integer(5))).toEqual("  0X5")
      expect(String.format("%5d", new Integer(-10))).toEqual("  -10")
      expect(String.format("%05d", new Integer(-10))).toEqual("-0010")
      expect(String.format("%x", new Integer(-3))).toEqual("fffffffd")
      expect(String.format("%x", new java.lang.Byte(-4.toByte))).toEqual("fffffffc")
    }

    it("should respond to `getBytes`") {
      import StandardCharsets._

      expect("hello-world".getBytes(UTF_8).toJSArray).toEqual(
          js.Array(104, 101, 108, 108, 111, 45, 119, 111, 114, 108, 100))
      expect("ᚠᛇᚻ᛫ᛒᛦᚦ᛫ᚠᚱᚩᚠᚢᚱ᛫ᚠᛁᚱᚪ᛫ᚷᛖᚻᚹᛦᛚᚳᚢᛗ".getBytes(UTF_16).toJSArray).toEqual(
          js.Array(-2, -1, 22, -96, 22, -57, 22, -69, 22, -21, 22, -46, 22, -26,
              22, -90, 22, -21, 22, -96, 22, -79, 22, -87, 22, -96, 22, -94, 22,
              -79, 22, -21, 22, -96, 22, -63, 22, -79, 22, -86, 22, -21, 22,
              -73, 22, -42, 22, -69, 22, -71, 22, -26, 22, -38, 22, -77, 22,
              -94, 22, -41))
    }

    it("should respond to `regionMatches`") {
      /* Ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/StringTest.java
       */
      val test = "abcdef"

      expect(test.regionMatches(1, "bcd", 0, 3)).toBeTruthy()
      expect(test.regionMatches(1, "bcdx", 0, 3)).toBeTruthy()
      expect(test.regionMatches(1, "bcdx", 0, 4)).toBeFalsy()
      expect(test.regionMatches(1, "bcdx", 1, 3)).toBeFalsy()
      expect(test.regionMatches(true, 0, "XAbCd", 1, 4)).toBeTruthy()
      expect(test.regionMatches(true, 1, "BcD", 0, 3)).toBeTruthy()
      expect(test.regionMatches(true, 1, "bCdx", 0, 3)).toBeTruthy()
      expect(test.regionMatches(true, 1, "bCdx", 0, 4)).toBeFalsy()
      expect(test.regionMatches(true, 1, "bCdx", 1, 3)).toBeFalsy()
      expect(test.regionMatches(true, 0, "xaBcd", 1, 4)).toBeTruthy()

      val testU = test.toUpperCase()
      expect(testU.regionMatches(true, 0, "XAbCd", 1, 4)).toBeTruthy()
      expect(testU.regionMatches(true, 1, "BcD", 0, 3)).toBeTruthy()
      expect(testU.regionMatches(true, 1, "bCdx", 0, 3)).toBeTruthy()
      expect(testU.regionMatches(true, 1, "bCdx", 0, 4)).toBeFalsy()
      expect(testU.regionMatches(true, 1, "bCdx", 1, 3)).toBeFalsy()
      expect(testU.regionMatches(true, 0, "xaBcd", 1, 4)).toBeTruthy()

      expect(() => test.regionMatches(-1, null, -1, -1)).toThrow()
      expect(() => test.regionMatches(true, -1, null, -1, -1)).toThrow()

      // scalastyle:off line.size.limit
      /* If len is negative, you must return true in some cases. See
       * http://docs.oracle.com/javase/8/docs/api/java/lang/String.html#regionMatches-boolean-int-java.lang.String-int-int-
       */
      // scalastyle:on line.size.limit

      // four cases that are false, irrelevant of sign of len nor the value of the other string
      expect(test.regionMatches(-1, test, 0, -4)).toBeFalsy
      expect(test.regionMatches(0, test, -1, -4)).toBeFalsy
      expect(test.regionMatches(100, test, 0, -4)).toBeFalsy
      expect(test.regionMatches(0, test, 100, -4)).toBeFalsy

      // the strange cases that are true
      expect(test.regionMatches(0, test, 0, -4)).toBeTruthy
      expect(test.regionMatches(1, "bcdx", 0, -4)).toBeTruthy
      expect(test.regionMatches(1, "bcdx", 1, -3)).toBeTruthy
      expect(test.regionMatches(true, 1, "bCdx", 0, -4)).toBeTruthy
      expect(test.regionMatches(true, 1, "bCdx", 1, -3)).toBeTruthy
      expect(testU.regionMatches(true, 1, "bCdx", 0, -4)).toBeTruthy
      expect(testU.regionMatches(true, 1, "bCdx", 1, -3)).toBeTruthy
    }
  }
}
