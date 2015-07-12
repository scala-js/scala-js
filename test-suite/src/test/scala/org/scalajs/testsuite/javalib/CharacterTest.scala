/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

import org.scalajs.jasminetest.JasmineTest
import scala.scalajs.js

object CharacterTest extends JasmineTest {

  describe("java.lang.Character") {

    it("should provide `isISOControl`") {
      val isoControlChars = (('\u0000' to '\u001F') ++
          ('\u007F' to '\u009F')).map(_.toInt).toSet
      isoControlChars foreach { c =>
        expect(Character.isISOControl(c)).toEqual(true)
      }

      val randomInts = List.fill(100)(scala.util.Random.nextInt)
      ((-1000 to 1000) ++ randomInts).filterNot(isoControlChars) foreach { c =>
        expect(Character.isISOControl(c)).toEqual(false)
      }
    }

    it("should provide `digit`") {
      expect(Character.digit('a', 16)).toEqual(10)
      expect(Character.digit('}', 5)).toEqual(-1)
      expect(Character.digit('1', 50)).toEqual(-1)
      expect(Character.digit('1', 36)).toEqual(1)
      expect(Character.digit('Z', 36)).toEqual(35)
      expect(Character.digit('\uFF22', 20)).toEqual(11)
    }

    it("should provide `forDigit`") {
      /* Ported from
       * https://github.com/gwtproject/gwt/blob/master/user/test/com/google/gwt/emultest/java/lang/CharacterTest.java
       */
      for (i <- 0 until 36) {
        expect(Character.digit(Character.forDigit(i, 36), 36)).toEqual(i)
      }
      expect(Character.forDigit(9, 10) == '9').toBeTruthy
    }

    it("should provide `toChars`") {
      expect(Character.toChars(0x61) sameElements Array('a')).toBeTruthy
      expect(Character.toChars(0x10000) sameElements Array('\uD800', '\uDC00')).toBeTruthy
      expect(Character.toChars(0x10001) sameElements Array('\uD800', '\uDC01')).toBeTruthy
      expect(Character.toChars(0x10401) sameElements Array('\uD801', '\uDC01')).toBeTruthy
      expect(Character.toChars(0x10FFFF) sameElements Array('\uDBFF', '\uDFFF')).toBeTruthy

      expect(() => Character.toChars(Integer.MAX_VALUE)).toThrow
    }

    it("should provide isDigit") {
      expect(Character.isDigit('a')).toBeFalsy
      expect(Character.isDigit('0')).toBeTruthy
      expect(Character.isDigit('5')).toBeTruthy
      expect(Character.isDigit('9')).toBeTruthy
      expect(Character.isDigit('z')).toBeFalsy
      expect(Character.isDigit(' ')).toBeFalsy
    }

    it("should provide `compareTo`") {
      def compare(x: Char, y: Char): Int =
        new Character(x).compareTo(new Character(y))

      expect(compare('0', '5')).toBeLessThan(0)
      expect(compare('o', 'g')).toBeGreaterThan(0)
      expect(compare('A', 'a')).toBeLessThan(0)
      expect(compare('b', 'b')).toEqual(0)
    }

    it("should be a Comparable") {
      def compare(x: Any, y: Any): Int =
        x.asInstanceOf[Comparable[Any]].compareTo(y)

      expect(compare('0', '5')).toBeLessThan(0)
      expect(compare('o', 'g')).toBeGreaterThan(0)
      expect(compare('A', 'a')).toBeLessThan(0)
      expect(compare('b', 'b')).toEqual(0)
    }

    it("should provide isIdentifierIgnorable") {
      for (c <- '\u0000' to '\u0008')
        expect(Character.isIdentifierIgnorable(c)).toBeTruthy

      for (c <- '\u000E' to '\u001B')
        expect(Character.isIdentifierIgnorable(c)).toBeTruthy

      for (c <- '\u007F' to '\u009F')
        expect(Character.isIdentifierIgnorable(c)).toBeTruthy

      // Exhaustive list of Cf category. Unicode 7.0.0
      expect(Character.isIdentifierIgnorable('\u00AD')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u0600')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u0601')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u0602')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u0603')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u0604')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u0605')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u061C')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u06DD')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u070F')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u180E')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u200B')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u200C')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u200D')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u200E')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u200F')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u202A')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u202B')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u202C')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u202D')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u202E')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2060')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2061')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2062')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2063')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2064')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2066')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2067')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2068')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u2069')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u206A')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u206B')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u206C')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u206D')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u206E')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\u206F')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\uFEFF')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\uFFF9')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\uFFFA')).toBeTruthy
      expect(Character.isIdentifierIgnorable('\uFFFB')).toBeTruthy

      // BUG in JDK? 17B4 should be "Mn", Java says "Cf"
      //expect(Character.isIdentifierIgnorable('\u17b4')).toBeTruthy

      // 100 randomly generated negatives
      expect(Character.isIdentifierIgnorable('\u745a')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub445')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub23a')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub029')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ufb5c')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u1b67')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u943b')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ue766')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uad12')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub80b')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u7341')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ubc73')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uabb9')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub34b')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u1063')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u272f')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3801')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u53a6')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u2ec2')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u540c')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uc85f')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ud2c8')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u551b')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uc0a1')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ud25a')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u2b98')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u398b')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ubc77')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u54cc')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uc9a0')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ud10f')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uf7e1')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u0f29')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uafcd')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uf187')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u6287')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uacb6')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uff99')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub59e')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uf630')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ufaec')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ua7d7')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3eab')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u54a5')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u393a')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uc621')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u766c')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ud64c')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u8beb')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u44e2')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub6f6')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u58b6')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3bad')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3c28')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ufbfd')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u585f')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u7227')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ucea7')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u2c82')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u686d')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u120d')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uf3db')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u320a')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ud96e')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u85eb')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u9648')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u08a4')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u9db7')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u82c7')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ufe12')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u0eaf')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u96dc')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3a2a')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uc72e')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3745')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ubcf9')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u5f66')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u9be1')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ud81d')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3ca3')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3e82')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u7ce4')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u33ca')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ue725')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uef49')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ue2cf')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\udcf0')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u5f2e')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u2a63')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ud2d2')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u8023')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ua957')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u10ba')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uf85f')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uc40d')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u2509')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u0d8e')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u9db8')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u824d')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u5670')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u6005')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub8de')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uff5c')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\ub36d')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u0cf2')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u82f6')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u9206')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u95e1')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u990f')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u9fc7')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\udffb')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u0ecb')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u7563')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uf0ff')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u6b2e')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u894c')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u8f06')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\uffa9')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u37b0')).toBeFalsy
      expect(Character.isIdentifierIgnorable('\u3e04')).toBeFalsy

    }

    it("should provide isUnicodeIdentifierStart") {
      // 100 randomly generated positives and 100 randomly generated negatives

      expect(Character.isUnicodeIdentifierStart('\ud6d5')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u3f9c')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u3a40')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u53af')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u1636')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u4884')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ucba4')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u1ee4')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u6dec')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u10d4')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u631f')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u3661')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u55f8')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ub4ef')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ud509')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u65b5')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u316b')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ub270')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u7f0f')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\uff84')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u11cc')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u0294')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u51b1')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u9ae2')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u304a')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ud5c7')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u3b4b')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u5e42')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u51fc')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\uc148')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\uc1ae')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u7372')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\uc116')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u5d29')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u8753')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u50f8')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u3f9d')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u1f44')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ucd43')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u9126')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u8d2e')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u4f5c')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u66d7')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ua30b')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u140b')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ub264')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u7b35')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u15e4')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ubb37')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u34e3')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\uac3e')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ubd0e')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ub641')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u1580')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u30c1')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ub0c8')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u8681')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u7f14')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u4142')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u56c1')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u0444')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u9964')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ub5c0')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u43d8')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u479e')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u0853')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ube08')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u9346')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\uf9c1')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u0e8a')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u212c')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u810c')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u8089')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u1331')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ua5f7')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u5e5e')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u613b')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u34a7')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ud15b')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\uc1fc')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u92f1')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u3ae6')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ufceb')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u7584')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ufe98')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ubb23')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u7961')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u4445')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u4d5f')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u61cb')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u5176')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ub987')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u906a')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u4317')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u93ad')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u825a')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u7ff8')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u533a')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\u5617')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ufcc6')).toBeTruthy
      expect(Character.isUnicodeIdentifierStart('\ue398')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ueab6')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue7bc')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf8ab')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue27f')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uebea')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ueedc')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf091')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2785')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u287b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf042')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u20f9')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u23d6')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udc5b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ued16')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u1b6b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue7ba')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf7fa')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2125')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uea97')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue624')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ufbb8')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2730')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udb89')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue30d')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2e24')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf03e')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uda27')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u28fc')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u9ffe')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ude19')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0b70')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uddfc')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ued53')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue8cb')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udccc')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u00a3')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0bed')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0c68')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf47b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0f96')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue9c3')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf784')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uef4b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udee1')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2f61')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf622')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u19f9')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ud86a')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ued83')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf7e4')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uecce')).toBeFalsy

      // BUG in JDK? A699 should be "Ll", Java says "Cn"
      // expect(Character.isUnicodeIdentifierStart('\ua699')).toBeFalsy

      expect(Character.isUnicodeIdentifierStart('\uaa5f')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udf24')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2e0e')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf322')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue137')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ued19')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u21ab')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue972')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udbf2')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf54c')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u4dd3')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2769')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue363')).toBeFalsy

      // BUG in JDK? 1BBB should be "Lo", Java says "Cn"
      // expect(Character.isUnicodeIdentifierStart('\u1bbb')).toBeFalsy

      expect(Character.isUnicodeIdentifierStart('\ueae7')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2bf3')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue704')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u1c7f')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf52b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue9e3')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u259b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf250')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf42f')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ue244')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u20d9')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ua881')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0ee6')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2203')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0fc7')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u07fc')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udb86')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2a70')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2bb7')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uecf0')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ude48')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0a3b')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u20b8')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf898')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u23e6')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ud8ba')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uda1e')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\udc12')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u2a06')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\u0888')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\ud9ec')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf81f')).toBeFalsy
      expect(Character.isUnicodeIdentifierStart('\uf817')).toBeFalsy
    }

    it("should provide isUnicodeIdentifierPart") {
      // 100 randomly generated positives and 100 randomly generated negatives

      expect(Character.isUnicodeIdentifierPart('\u48d3')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u0905')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8f51')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u9bcb')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ud358')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u1538')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\uffcf')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u83ec')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3a89')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ub63a')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ufe24')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u2d62')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u15ca')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u4fa4')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u47d1')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u831c')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u84e6')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u7783')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ua03c')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u6ecf')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u147f')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u67a9')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8b6c')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3410')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u2cc0')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ua332')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u9733')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u5df3')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3fd7')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u6611')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u55b4')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8bc8')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u6f74')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u6c97')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u6a86')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u6000')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u614f')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u206e')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ua801')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u9edf')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ub42c')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u7fcd')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8a60')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u182f')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u5d0a')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\uaf9c')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u9d4b')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u5088')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\uc1a6')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ubbe4')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\uad25')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u4653')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8add')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3d1c')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u80a8')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u810e')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\uc1d2')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ub984')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u9d13')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u37c2')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u13cd')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u53f9')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u98b7')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u57f3')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ub554')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u0176')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ua318')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u9704')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8d52')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u940a')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u0fa5')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u38d1')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3b33')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u93bb')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u03bd')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u4c88')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ud67d')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ubcbf')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3867')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u4368')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8f2d')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u049a')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u4c01')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u5589')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u5e71')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ua1fd')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3a4a')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\uc111')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ub465')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u95af')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ubf2c')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8488')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u4317')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u6b77')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8995')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u7467')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u16b7')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u3ca0')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u5332')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\u8654')).toBeTruthy
      expect(Character.isUnicodeIdentifierPart('\ua8c8')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue3ca')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uebee')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u270e')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf0ac')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue9ec')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u296a')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u33fd')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue5f4')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ueb01')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf38b')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2e6f')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uea69')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf155')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u0f0e')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ueb80')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ud959')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue25e')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf566')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue4a3')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uec44')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u3297')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u3214')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u1bfd')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u4dd0')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uea99')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u309b')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf592')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf4dd')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\udfaf')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\udd38')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf820')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uaacd')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uff5b')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ude36')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue33b')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\udbce')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue1f6')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf78a')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ueb44')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uebd4')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u1df7')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2f10')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u1cbf')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2362')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uebeb')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2ede')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u221d')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2021')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\udf41')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u05f5')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u24ab')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uee15')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf175')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf35c')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\udc7b')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ud883')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf341')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ueec6')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2f57')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uff64')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue6a4')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uec34')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u22a5')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf5ac')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u3360')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u28b0')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf678')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue0e4')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u233f')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u0afa')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2013')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ud7af')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ud98e')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ud8a5')).toBeFalsy

      // BUG in JDK? A79E should be "Lu", Java says "Cn"
      // expect(Character.isUnicodeIdentifierPart('\ua79e')).toBeFalsy

      expect(Character.isUnicodeIdentifierPart('\u1806')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue07a')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2748')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uabad')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uec5c')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue832')).toBeFalsy

      // BUG in JDK? 08A9 should be "Lo", Java says "Cn"
      // expect(Character.isUnicodeIdentifierPart('\u08a9')).toBeFalsy

      expect(Character.isUnicodeIdentifierPart('\ue4bd')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u208a')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf840')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf570')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uef1e')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2bd4')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue385')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\udc18')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u0af0')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u244a')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf01e')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\uf114')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\ue9c4')).toBeFalsy

      // BUG in JDK? AAF4 should be "Lm", Java says "Cn"
      // expect(Character.isUnicodeIdentifierPart('\uaaf4')).toBeFalsy

      expect(Character.isUnicodeIdentifierPart('\uf7b9')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\udd2f')).toBeFalsy
      expect(Character.isUnicodeIdentifierPart('\u2d2c')).toBeFalsy
    }

    it("should provide isUpperCase") {
      for (c <- 'a' to 'z')
        expect(Character.isUpperCase(c)).toBeFalsy
      for (c <- 'A' to 'Z')
        expect(Character.isUpperCase(c)).toBeTruthy
      for (c <- '0' to '9')
        expect(Character.isUpperCase(c)).toBeFalsy

      // example form issue #1646
      expect(Character.isUpperCase('(')).toBeFalsy
      expect(Character.isUpperCase(')')).toBeFalsy

      // 100 randomly chosen uppeer cases
      expect(Character.isUpperCase('\uA78B')).toBeTruthy
      expect(Character.isUpperCase('\u01D1')).toBeTruthy
      expect(Character.isUpperCase('\uA680')).toBeTruthy
      expect(Character.isUpperCase('\u01E2')).toBeTruthy
      expect(Character.isUpperCase('\u00C1')).toBeTruthy
      expect(Character.isUpperCase('\u038F')).toBeTruthy
      expect(Character.isUpperCase('\u04FC')).toBeTruthy
      expect(Character.isUpperCase('\u0232')).toBeTruthy
      expect(Character.isUpperCase('\u1E86')).toBeTruthy
      expect(Character.isUpperCase('\u1E66')).toBeTruthy
      expect(Character.isUpperCase('\u1E12')).toBeTruthy
      expect(Character.isUpperCase('\u1F09')).toBeTruthy
      expect(Character.isUpperCase('\uA758')).toBeTruthy
      expect(Character.isUpperCase('\u01B1')).toBeTruthy
      expect(Character.isUpperCase('\u0230')).toBeTruthy
      expect(Character.isUpperCase('\u2CA2')).toBeTruthy
      expect(Character.isUpperCase('\u0154')).toBeTruthy
      expect(Character.isUpperCase('\uA77E')).toBeTruthy
      expect(Character.isUpperCase('\u2C69')).toBeTruthy
      expect(Character.isUpperCase('\u1EDE')).toBeTruthy
      expect(Character.isUpperCase('\u2CBC')).toBeTruthy
      expect(Character.isUpperCase('\u2CED')).toBeTruthy
      expect(Character.isUpperCase('\u2CAC')).toBeTruthy
      expect(Character.isUpperCase('\u0549')).toBeTruthy
      expect(Character.isUpperCase('\u00D5')).toBeTruthy
      expect(Character.isUpperCase('\u0494')).toBeTruthy
      expect(Character.isUpperCase('\u2CCC')).toBeTruthy
      expect(Character.isUpperCase('\u0162')).toBeTruthy
      expect(Character.isUpperCase('\uFF22')).toBeTruthy
      expect(Character.isUpperCase('\u24C4')).toBeTruthy
      expect(Character.isUpperCase('\u041A')).toBeTruthy
      expect(Character.isUpperCase('\u2124')).toBeTruthy
      expect(Character.isUpperCase('\u1FBB')).toBeTruthy
      expect(Character.isUpperCase('\u1FD9')).toBeTruthy
      expect(Character.isUpperCase('\uA662')).toBeTruthy
      expect(Character.isUpperCase('\u0504')).toBeTruthy
      expect(Character.isUpperCase('\u2C14')).toBeTruthy
      expect(Character.isUpperCase('\uA760')).toBeTruthy
      expect(Character.isUpperCase('\u1E44')).toBeTruthy
      expect(Character.isUpperCase('\u10A2')).toBeTruthy
      expect(Character.isUpperCase('\u017B')).toBeTruthy
      expect(Character.isUpperCase('\u054D')).toBeTruthy
      expect(Character.isUpperCase('\u053E')).toBeTruthy
      expect(Character.isUpperCase('\u1E22')).toBeTruthy
      expect(Character.isUpperCase('\u013F')).toBeTruthy
      expect(Character.isUpperCase('\u1E2A')).toBeTruthy
      expect(Character.isUpperCase('\uA760')).toBeTruthy
      expect(Character.isUpperCase('\u24C3')).toBeTruthy
      expect(Character.isUpperCase('\u049A')).toBeTruthy
      expect(Character.isUpperCase('\u10BD')).toBeTruthy
      expect(Character.isUpperCase('\u1EEA')).toBeTruthy
      expect(Character.isUpperCase('\u0397')).toBeTruthy
      expect(Character.isUpperCase('\u1F3D')).toBeTruthy
      expect(Character.isUpperCase('\u050A')).toBeTruthy
      expect(Character.isUpperCase('\uFF23')).toBeTruthy
      expect(Character.isUpperCase('\u2C01')).toBeTruthy
      expect(Character.isUpperCase('\u10C4')).toBeTruthy
      expect(Character.isUpperCase('\u1EA2')).toBeTruthy
      expect(Character.isUpperCase('\u2C72')).toBeTruthy
      expect(Character.isUpperCase('\u0554')).toBeTruthy
      expect(Character.isUpperCase('\u01E8')).toBeTruthy
      expect(Character.isUpperCase('\u10A4')).toBeTruthy
      expect(Character.isUpperCase('\u1F0C')).toBeTruthy
      expect(Character.isUpperCase('\u0520')).toBeTruthy
      expect(Character.isUpperCase('\u00C5')).toBeTruthy
      expect(Character.isUpperCase('\u10AC')).toBeTruthy
      expect(Character.isUpperCase('\u2CA4')).toBeTruthy
      expect(Character.isUpperCase('\uA73A')).toBeTruthy
      expect(Character.isUpperCase('\u1EFC')).toBeTruthy
      expect(Character.isUpperCase('\u0049')).toBeTruthy
      expect(Character.isUpperCase('\u10A4')).toBeTruthy
      expect(Character.isUpperCase('\uA652')).toBeTruthy
      expect(Character.isUpperCase('\u1E0E')).toBeTruthy
      expect(Character.isUpperCase('\u04E2')).toBeTruthy
      expect(Character.isUpperCase('\u0174')).toBeTruthy
      expect(Character.isUpperCase('\u00CE')).toBeTruthy
      expect(Character.isUpperCase('\u042F')).toBeTruthy
      expect(Character.isUpperCase('\u053F')).toBeTruthy
      expect(Character.isUpperCase('\u0222')).toBeTruthy
      expect(Character.isUpperCase('\u1F3F')).toBeTruthy
      expect(Character.isUpperCase('\uA77E')).toBeTruthy
      expect(Character.isUpperCase('\u01AC')).toBeTruthy
      expect(Character.isUpperCase('\u2C20')).toBeTruthy
      expect(Character.isUpperCase('\u1ECC')).toBeTruthy
      expect(Character.isUpperCase('\u04DE')).toBeTruthy
      expect(Character.isUpperCase('\u004F')).toBeTruthy
      expect(Character.isUpperCase('\u2CE2')).toBeTruthy
      expect(Character.isUpperCase('\u0124')).toBeTruthy
      expect(Character.isUpperCase('\u1F4A')).toBeTruthy
      expect(Character.isUpperCase('\u0112')).toBeTruthy
      expect(Character.isUpperCase('\u016E')).toBeTruthy
      expect(Character.isUpperCase('\u24B7')).toBeTruthy
      expect(Character.isUpperCase('\u211A')).toBeTruthy
      expect(Character.isUpperCase('\uA72A')).toBeTruthy
      expect(Character.isUpperCase('\u0388')).toBeTruthy
      expect(Character.isUpperCase('\uA726')).toBeTruthy
      expect(Character.isUpperCase('\u0052')).toBeTruthy
      expect(Character.isUpperCase('\u1EB0')).toBeTruthy
      expect(Character.isUpperCase('\u0197')).toBeTruthy
      expect(Character.isUpperCase('\u014A')).toBeTruthy

      // 100 randomly chosen non uppeer cases
      expect(Character.isUpperCase('\u58C5')).toBeFalsy
      expect(Character.isUpperCase('\uF319')).toBeFalsy
      expect(Character.isUpperCase('\u1FBC')).toBeFalsy
      expect(Character.isUpperCase('\u8143')).toBeFalsy
      expect(Character.isUpperCase('\u468A')).toBeFalsy
      expect(Character.isUpperCase('\uEAA8')).toBeFalsy
      expect(Character.isUpperCase('\u3D3A')).toBeFalsy
      expect(Character.isUpperCase('\uB168')).toBeFalsy
      expect(Character.isUpperCase('\u6747')).toBeFalsy
      expect(Character.isUpperCase('\uAA8D')).toBeFalsy
      expect(Character.isUpperCase('\u26B7')).toBeFalsy
      expect(Character.isUpperCase('\uDFE3')).toBeFalsy
      expect(Character.isUpperCase('\u51AD')).toBeFalsy
      expect(Character.isUpperCase('\uB4FC')).toBeFalsy
      expect(Character.isUpperCase('\uE23A')).toBeFalsy
      expect(Character.isUpperCase('\u9465')).toBeFalsy
      expect(Character.isUpperCase('\uAE66')).toBeFalsy
      expect(Character.isUpperCase('\u2EA3')).toBeFalsy
      expect(Character.isUpperCase('\u2D05')).toBeFalsy
      expect(Character.isUpperCase('\u2033')).toBeFalsy
      expect(Character.isUpperCase('\u4BFA')).toBeFalsy
      expect(Character.isUpperCase('\u89B6')).toBeFalsy
      expect(Character.isUpperCase('\u9A3F')).toBeFalsy
      expect(Character.isUpperCase('\u4FCD')).toBeFalsy
      expect(Character.isUpperCase('\u770B')).toBeFalsy
      expect(Character.isUpperCase('\u17B4')).toBeFalsy
      expect(Character.isUpperCase('\u08A6')).toBeFalsy
      expect(Character.isUpperCase('\uA202')).toBeFalsy
      expect(Character.isUpperCase('\uE726')).toBeFalsy
      expect(Character.isUpperCase('\u8605')).toBeFalsy
      expect(Character.isUpperCase('\uE24F')).toBeFalsy
      expect(Character.isUpperCase('\u588C')).toBeFalsy
      expect(Character.isUpperCase('\u3AF6')).toBeFalsy
      expect(Character.isUpperCase('\u313F')).toBeFalsy
      expect(Character.isUpperCase('\u8804')).toBeFalsy
      expect(Character.isUpperCase('\uE02A')).toBeFalsy
      expect(Character.isUpperCase('\u89C5')).toBeFalsy
      expect(Character.isUpperCase('\u3B9E')).toBeFalsy
      expect(Character.isUpperCase('\uF678')).toBeFalsy
      expect(Character.isUpperCase('\uD142')).toBeFalsy
      expect(Character.isUpperCase('\u19E5')).toBeFalsy
      expect(Character.isUpperCase('\u8C7A')).toBeFalsy
      expect(Character.isUpperCase('\uBF0C')).toBeFalsy
      expect(Character.isUpperCase('\u5EC6')).toBeFalsy
      expect(Character.isUpperCase('\u6EA3')).toBeFalsy
      expect(Character.isUpperCase('\uCD19')).toBeFalsy
      expect(Character.isUpperCase('\uA6F3')).toBeFalsy
      expect(Character.isUpperCase('\uA5FD')).toBeFalsy
      expect(Character.isUpperCase('\uEAA7')).toBeFalsy
      expect(Character.isUpperCase('\u89DB')).toBeFalsy
      expect(Character.isUpperCase('\u2657')).toBeFalsy
      expect(Character.isUpperCase('\u667A')).toBeFalsy
      expect(Character.isUpperCase('\u131C')).toBeFalsy
      expect(Character.isUpperCase('\u5906')).toBeFalsy
      expect(Character.isUpperCase('\uAA61')).toBeFalsy
      expect(Character.isUpperCase('\uCDD3')).toBeFalsy
      expect(Character.isUpperCase('\u1952')).toBeFalsy
      expect(Character.isUpperCase('\uD93B')).toBeFalsy
      expect(Character.isUpperCase('\uE5B4')).toBeFalsy
      expect(Character.isUpperCase('\uF5D1')).toBeFalsy
      expect(Character.isUpperCase('\uFC49')).toBeFalsy
      expect(Character.isUpperCase('\u51D6')).toBeFalsy
      expect(Character.isUpperCase('\u1D29')).toBeFalsy
      expect(Character.isUpperCase('\u2A30')).toBeFalsy
      expect(Character.isUpperCase('\u881A')).toBeFalsy
      expect(Character.isUpperCase('\u2EA5')).toBeFalsy
      expect(Character.isUpperCase('\u866C')).toBeFalsy
      expect(Character.isUpperCase('\uCAF5')).toBeFalsy
      expect(Character.isUpperCase('\uF5F1')).toBeFalsy
      expect(Character.isUpperCase('\u97B4')).toBeFalsy
      expect(Character.isUpperCase('\uF5F5')).toBeFalsy
      expect(Character.isUpperCase('\u46FD')).toBeFalsy
      expect(Character.isUpperCase('\u538B')).toBeFalsy
      expect(Character.isUpperCase('\u320B')).toBeFalsy
      expect(Character.isUpperCase('\u6F1F')).toBeFalsy
      expect(Character.isUpperCase('\u85B4')).toBeFalsy
      expect(Character.isUpperCase('\u887E')).toBeFalsy
      expect(Character.isUpperCase('\uEA84')).toBeFalsy
      expect(Character.isUpperCase('\uFFE2')).toBeFalsy
      expect(Character.isUpperCase('\u48B2')).toBeFalsy
      expect(Character.isUpperCase('\u3D8D')).toBeFalsy
      expect(Character.isUpperCase('\u071B')).toBeFalsy
      expect(Character.isUpperCase('\u70D9')).toBeFalsy
      expect(Character.isUpperCase('\u86FD')).toBeFalsy
      expect(Character.isUpperCase('\u9303')).toBeFalsy
      expect(Character.isUpperCase('\u6620')).toBeFalsy
      expect(Character.isUpperCase('\uB503')).toBeFalsy
      expect(Character.isUpperCase('\u9D5A')).toBeFalsy
      expect(Character.isUpperCase('\u7909')).toBeFalsy
      expect(Character.isUpperCase('\u772C')).toBeFalsy
      expect(Character.isUpperCase('\u93D8')).toBeFalsy
      expect(Character.isUpperCase('\uE969')).toBeFalsy
      expect(Character.isUpperCase('\u327F')).toBeFalsy
      expect(Character.isUpperCase('\u8473')).toBeFalsy
      expect(Character.isUpperCase('\u478B')).toBeFalsy
      expect(Character.isUpperCase('\uB916')).toBeFalsy
      expect(Character.isUpperCase('\uBBFA')).toBeFalsy
      expect(Character.isUpperCase('\uDC65')).toBeFalsy
      expect(Character.isUpperCase('\u65E9')).toBeFalsy
      expect(Character.isUpperCase('\u97FF')).toBeFalsy
    }

    it("should provide isLowerCase") {
      for (c <- 'a' to 'z')
        expect(Character.isLowerCase(c)).toBeTruthy
      for (c <- 'A' to 'Z')
        expect(Character.isLowerCase(c)).toBeFalsy
      for (c <- '0' to '9')
        expect(Character.isLowerCase(c)).toBeFalsy

      // example form issue #1646
      expect(Character.isLowerCase('(')).toBeFalsy
      expect(Character.isLowerCase(')')).toBeFalsy

      // 100 randomly chosen lower cases
      expect(Character.isLowerCase('\u1D9F')).toBeTruthy
      expect(Character.isLowerCase('\u1ED9')).toBeTruthy
      expect(Character.isLowerCase('\u01E9')).toBeTruthy
      expect(Character.isLowerCase('\u2177')).toBeTruthy
      expect(Character.isLowerCase('\u2CB7')).toBeTruthy
      expect(Character.isLowerCase('\u03CD')).toBeTruthy
      expect(Character.isLowerCase('\u1D2A')).toBeTruthy
      expect(Character.isLowerCase('\u04C4')).toBeTruthy
      expect(Character.isLowerCase('\u1D20')).toBeTruthy
      expect(Character.isLowerCase('\u1F20')).toBeTruthy
      expect(Character.isLowerCase('\u2093')).toBeTruthy
      expect(Character.isLowerCase('\u2CC7')).toBeTruthy
      expect(Character.isLowerCase('\uA76F')).toBeTruthy
      expect(Character.isLowerCase('\u24E4')).toBeTruthy
      expect(Character.isLowerCase('\u019E')).toBeTruthy
      expect(Character.isLowerCase('\uA77A')).toBeTruthy
      expect(Character.isLowerCase('\u217E')).toBeTruthy
      expect(Character.isLowerCase('\u1D6C')).toBeTruthy
      expect(Character.isLowerCase('\u1D1D')).toBeTruthy
      expect(Character.isLowerCase('\u2CA3')).toBeTruthy
      expect(Character.isLowerCase('\u1FE0')).toBeTruthy
      expect(Character.isLowerCase('\u2C31')).toBeTruthy
      expect(Character.isLowerCase('\u021F')).toBeTruthy
      expect(Character.isLowerCase('\u1FB2')).toBeTruthy
      expect(Character.isLowerCase('\u1DB7')).toBeTruthy
      expect(Character.isLowerCase('\uA77A')).toBeTruthy
      expect(Character.isLowerCase('\u03C5')).toBeTruthy
      expect(Character.isLowerCase('\u1FC3')).toBeTruthy
      expect(Character.isLowerCase('\u0449')).toBeTruthy
      expect(Character.isLowerCase('\u1DB0')).toBeTruthy
      expect(Character.isLowerCase('\u2093')).toBeTruthy
      expect(Character.isLowerCase('\u2099')).toBeTruthy
      expect(Character.isLowerCase('\u010B')).toBeTruthy
      expect(Character.isLowerCase('\u03CA')).toBeTruthy
      expect(Character.isLowerCase('\u017C')).toBeTruthy
      expect(Character.isLowerCase('\u1F97')).toBeTruthy
      expect(Character.isLowerCase('\u1D6F')).toBeTruthy
      expect(Character.isLowerCase('\u03D0')).toBeTruthy
      expect(Character.isLowerCase('\u210A')).toBeTruthy
      expect(Character.isLowerCase('\u1E45')).toBeTruthy
      expect(Character.isLowerCase('\u0270')).toBeTruthy
      expect(Character.isLowerCase('\u01F9')).toBeTruthy
      expect(Character.isLowerCase('\u0289')).toBeTruthy
      expect(Character.isLowerCase('\u0151')).toBeTruthy
      expect(Character.isLowerCase('\u029C')).toBeTruthy
      expect(Character.isLowerCase('\u24D6')).toBeTruthy
      expect(Character.isLowerCase('\u1D54')).toBeTruthy
      expect(Character.isLowerCase('\u1DB2')).toBeTruthy
      expect(Character.isLowerCase('\u1D28')).toBeTruthy
      expect(Character.isLowerCase('\u01E5')).toBeTruthy
      expect(Character.isLowerCase('\uA791')).toBeTruthy
      expect(Character.isLowerCase('\u1FB4')).toBeTruthy
      expect(Character.isLowerCase('\u0127')).toBeTruthy
      expect(Character.isLowerCase('\u0440')).toBeTruthy
      expect(Character.isLowerCase('\u2C89')).toBeTruthy
      expect(Character.isLowerCase('\u1D69')).toBeTruthy
      expect(Character.isLowerCase('\u2CD5')).toBeTruthy
      expect(Character.isLowerCase('\u00F4')).toBeTruthy
      expect(Character.isLowerCase('\u0185')).toBeTruthy
      expect(Character.isLowerCase('\u1D50')).toBeTruthy
      expect(Character.isLowerCase('\u1F90')).toBeTruthy
      expect(Character.isLowerCase('\u1FF4')).toBeTruthy
      expect(Character.isLowerCase('\u1FB4')).toBeTruthy
      expect(Character.isLowerCase('\u02A9')).toBeTruthy
      expect(Character.isLowerCase('\u1ECB')).toBeTruthy
      expect(Character.isLowerCase('\u2095')).toBeTruthy
      expect(Character.isLowerCase('\u2D16')).toBeTruthy
      expect(Character.isLowerCase('\u2D08')).toBeTruthy
      expect(Character.isLowerCase('\u057D')).toBeTruthy
      expect(Character.isLowerCase('\u1FE1')).toBeTruthy
      expect(Character.isLowerCase('\u1D04')).toBeTruthy
      expect(Character.isLowerCase('\u0525')).toBeTruthy
      expect(Character.isLowerCase('\u1F14')).toBeTruthy
      expect(Character.isLowerCase('\u02B1')).toBeTruthy
      expect(Character.isLowerCase('\u1D89')).toBeTruthy
      expect(Character.isLowerCase('\u1DB6')).toBeTruthy
      expect(Character.isLowerCase('\u1DBC')).toBeTruthy
      expect(Character.isLowerCase('\u02E3')).toBeTruthy
      expect(Character.isLowerCase('\u1FD1')).toBeTruthy
      expect(Character.isLowerCase('\u1E0B')).toBeTruthy
      expect(Character.isLowerCase('\u01E5')).toBeTruthy
      expect(Character.isLowerCase('\u1E9C')).toBeTruthy
      expect(Character.isLowerCase('\u0436')).toBeTruthy
      expect(Character.isLowerCase('\uA74B')).toBeTruthy
      expect(Character.isLowerCase('\u057D')).toBeTruthy
      expect(Character.isLowerCase('\u0185')).toBeTruthy
      expect(Character.isLowerCase('\u0459')).toBeTruthy
      expect(Character.isLowerCase('\u1D48')).toBeTruthy
      expect(Character.isLowerCase('\u2D22')).toBeTruthy
      expect(Character.isLowerCase('\u03B6')).toBeTruthy
      expect(Character.isLowerCase('\uA737')).toBeTruthy
      expect(Character.isLowerCase('\u0573')).toBeTruthy
      expect(Character.isLowerCase('\u1D1F')).toBeTruthy
      expect(Character.isLowerCase('\u0441')).toBeTruthy
      expect(Character.isLowerCase('\u2D1A')).toBeTruthy
      expect(Character.isLowerCase('\u1D5D')).toBeTruthy
      expect(Character.isLowerCase('\u0078')).toBeTruthy
      expect(Character.isLowerCase('\u2C71')).toBeTruthy
      expect(Character.isLowerCase('\u1D45')).toBeTruthy
      expect(Character.isLowerCase('\u03FB')).toBeTruthy

      // 100 randomly chosen non lower cases
      expect(Character.isLowerCase('\uFEC4')).toBeFalsy
      expect(Character.isLowerCase('\u0ED4')).toBeFalsy
      expect(Character.isLowerCase('\u1FF1')).toBeFalsy
      expect(Character.isLowerCase('\uEC6E')).toBeFalsy
      expect(Character.isLowerCase('\u35DF')).toBeFalsy
      expect(Character.isLowerCase('\uE68E')).toBeFalsy
      expect(Character.isLowerCase('\uD09E')).toBeFalsy
      expect(Character.isLowerCase('\u782F')).toBeFalsy
      expect(Character.isLowerCase('\u3915')).toBeFalsy
      expect(Character.isLowerCase('\u622A')).toBeFalsy
      expect(Character.isLowerCase('\uB5DB')).toBeFalsy
      expect(Character.isLowerCase('\u6238')).toBeFalsy
      expect(Character.isLowerCase('\u6080')).toBeFalsy
      expect(Character.isLowerCase('\uB659')).toBeFalsy
      expect(Character.isLowerCase('\u66F0')).toBeFalsy
      expect(Character.isLowerCase('\u32A4')).toBeFalsy
      expect(Character.isLowerCase('\u0F4D')).toBeFalsy
      expect(Character.isLowerCase('\u6766')).toBeFalsy
      expect(Character.isLowerCase('\u7C03')).toBeFalsy
      expect(Character.isLowerCase('\u2A8D')).toBeFalsy
      expect(Character.isLowerCase('\u9332')).toBeFalsy
      expect(Character.isLowerCase('\uBB46')).toBeFalsy
      expect(Character.isLowerCase('\u785F')).toBeFalsy
      expect(Character.isLowerCase('\u3286')).toBeFalsy
      expect(Character.isLowerCase('\uB0EA')).toBeFalsy
      expect(Character.isLowerCase('\u4CFB')).toBeFalsy
      expect(Character.isLowerCase('\u2878')).toBeFalsy
      expect(Character.isLowerCase('\uC7EC')).toBeFalsy
      expect(Character.isLowerCase('\u41DD')).toBeFalsy
      expect(Character.isLowerCase('\uFF2B')).toBeFalsy
      expect(Character.isLowerCase('\u1E76')).toBeFalsy
      expect(Character.isLowerCase('\uA6B8')).toBeFalsy
      expect(Character.isLowerCase('\uAD91')).toBeFalsy
      expect(Character.isLowerCase('\u06B7')).toBeFalsy
      expect(Character.isLowerCase('\uB573')).toBeFalsy
      expect(Character.isLowerCase('\u62D6')).toBeFalsy
      expect(Character.isLowerCase('\u3D9A')).toBeFalsy
      expect(Character.isLowerCase('\uC6F6')).toBeFalsy
      expect(Character.isLowerCase('\u5607')).toBeFalsy
      expect(Character.isLowerCase('\uE23A')).toBeFalsy
      expect(Character.isLowerCase('\u3CFB')).toBeFalsy
      expect(Character.isLowerCase('\uBFF2')).toBeFalsy
      expect(Character.isLowerCase('\u9E76')).toBeFalsy
      expect(Character.isLowerCase('\uF388')).toBeFalsy
      expect(Character.isLowerCase('\uFB4D')).toBeFalsy
      expect(Character.isLowerCase('\uE47A')).toBeFalsy
      expect(Character.isLowerCase('\uF7EE')).toBeFalsy
      expect(Character.isLowerCase('\u4709')).toBeFalsy
      expect(Character.isLowerCase('\u3376')).toBeFalsy
      expect(Character.isLowerCase('\u37DE')).toBeFalsy
      expect(Character.isLowerCase('\u3D8C')).toBeFalsy
      expect(Character.isLowerCase('\u0C86')).toBeFalsy
      expect(Character.isLowerCase('\uB00E')).toBeFalsy
      expect(Character.isLowerCase('\u4F95')).toBeFalsy
      expect(Character.isLowerCase('\u832F')).toBeFalsy
      expect(Character.isLowerCase('\uB0D4')).toBeFalsy
      expect(Character.isLowerCase('\u985B')).toBeFalsy
      expect(Character.isLowerCase('\u4054')).toBeFalsy
      expect(Character.isLowerCase('\u391D')).toBeFalsy
      expect(Character.isLowerCase('\u1886')).toBeFalsy
      expect(Character.isLowerCase('\uCDF7')).toBeFalsy
      expect(Character.isLowerCase('\u2930')).toBeFalsy
      expect(Character.isLowerCase('\u3133')).toBeFalsy
      expect(Character.isLowerCase('\uB252')).toBeFalsy
      expect(Character.isLowerCase('\u902B')).toBeFalsy
      expect(Character.isLowerCase('\u72F3')).toBeFalsy
      expect(Character.isLowerCase('\u4DA6')).toBeFalsy
      expect(Character.isLowerCase('\uCE2D')).toBeFalsy
      expect(Character.isLowerCase('\u24F8')).toBeFalsy
      expect(Character.isLowerCase('\u1744')).toBeFalsy
      expect(Character.isLowerCase('\u5098')).toBeFalsy
      expect(Character.isLowerCase('\uAF3E')).toBeFalsy
      expect(Character.isLowerCase('\uE7E8')).toBeFalsy
      expect(Character.isLowerCase('\uB3CD')).toBeFalsy
      expect(Character.isLowerCase('\uD777')).toBeFalsy
      expect(Character.isLowerCase('\u6CCD')).toBeFalsy
      expect(Character.isLowerCase('\u2668')).toBeFalsy
      expect(Character.isLowerCase('\u195A')).toBeFalsy
      expect(Character.isLowerCase('\uD4A2')).toBeFalsy
      expect(Character.isLowerCase('\u60C0')).toBeFalsy
      expect(Character.isLowerCase('\uF8C7')).toBeFalsy
      expect(Character.isLowerCase('\u1DED')).toBeFalsy
      expect(Character.isLowerCase('\u08E7')).toBeFalsy
      expect(Character.isLowerCase('\uEBAE')).toBeFalsy
      expect(Character.isLowerCase('\uDD78')).toBeFalsy
      expect(Character.isLowerCase('\u2FCF')).toBeFalsy
      expect(Character.isLowerCase('\uF3CE')).toBeFalsy
      expect(Character.isLowerCase('\u344F')).toBeFalsy
      expect(Character.isLowerCase('\u5AA9')).toBeFalsy
      expect(Character.isLowerCase('\u0FA6')).toBeFalsy
      expect(Character.isLowerCase('\u627F')).toBeFalsy
      expect(Character.isLowerCase('\u23C4')).toBeFalsy
      expect(Character.isLowerCase('\u0F6A')).toBeFalsy
      expect(Character.isLowerCase('\u1DF9')).toBeFalsy
      expect(Character.isLowerCase('\u2BFC')).toBeFalsy
      expect(Character.isLowerCase('\uF4E3')).toBeFalsy
      expect(Character.isLowerCase('\u413A')).toBeFalsy
      expect(Character.isLowerCase('\u6103')).toBeFalsy
      expect(Character.isLowerCase('\u863E')).toBeFalsy
      expect(Character.isLowerCase('\u9918')).toBeFalsy
    }
  }
}
