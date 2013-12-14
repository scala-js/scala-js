/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package javalib

import scala.scalajs.js
import scala.scalajs.test.JasmineTest

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

    it("should respond to `compareTo`") {
      expect("Scala.js".compareTo("Scala")).toBeGreaterThan(0)
      expect("Scala.js".compareTo("Scala.js")).toBe(0)
      expect("Scala.js".compareTo("banana")).toBeLessThan(0)
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

    it("should respond to `indexOf`") {
      expect("Scala.js".indexOf("js")).toBe(6)
      expect("Scala.js".indexOf("Scala.js")).toBe(0)
      expect("ananas".indexOf("na")).toBe(1)
      expect("Scala.js".indexOf("Java")).toBe(-1)
    }

    it("should respond to `lastIndexOf`") {
      expect("Scala.js".lastIndexOf("Scala.js")).toBe(0)
      expect("ananas".lastIndexOf("na")).toBe(3)
      expect("Scala.js".lastIndexOf("Java")).toBe(-1)
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

    it("should respond to `codePointAt`") {
      expect("Scala.js".codePointAt(5)).toBe('.')
      expect("Scala.js".codePointAt(6)).not.toBe('.')
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
      expect("Scala.js".split("a")).toEqual(js.Array("Sc", "l", ".js"))
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
      expect(String.format("%x", new java.lang.Byte(-4.toByte))).toEqual("fc")
    }

  }
}
