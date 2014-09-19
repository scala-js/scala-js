/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.javalib

import org.scalajs.jasminetest.JasmineTest

import scala.scalajs.js

object ObjectTest extends JasmineTest {

  describe("java.lang.Object") {

    it("should provide `equals`") {
      case class xy(x: Int, y: Int)

      val l = List(xy(1, 2), xy(2, 1))
      val xy12 = xy(1, 2)

      expect(l.contains(xy12)).toBeTruthy
      expect(l.exists(_ == xy12)).toBeTruthy // the workaround
    }

    it("everything but null should be an Object") {
      expect((()             : Any).isInstanceOf[Object]).toBeTruthy
      expect((true           : Any).isInstanceOf[Object]).toBeTruthy
      expect(('a'            : Any).isInstanceOf[Object]).toBeTruthy
      expect((1.toByte       : Any).isInstanceOf[Object]).toBeTruthy
      expect((658.toShort    : Any).isInstanceOf[Object]).toBeTruthy
      expect((60000          : Any).isInstanceOf[Object]).toBeTruthy
      expect((12345678910112L: Any).isInstanceOf[Object]).toBeTruthy
      expect((6.5f           : Any).isInstanceOf[Object]).toBeTruthy
      expect((12.4           : Any).isInstanceOf[Object]).toBeTruthy
      expect((new Object     : Any).isInstanceOf[Object]).toBeTruthy
      expect(("hello"        : Any).isInstanceOf[Object]).toBeTruthy
      expect((List(1)        : Any).isInstanceOf[Object]).toBeTruthy
      expect((Array(1)       : Any).isInstanceOf[Object]).toBeTruthy
      expect((Array(Nil)     : Any).isInstanceOf[Object]).toBeTruthy
      expect((new js.Object  : Any).isInstanceOf[Object]).toBeTruthy
      expect((js.Array(5)    : Any).isInstanceOf[Object]).toBeTruthy
    }

    it("null should not be an Object") {
      expect((null: Any).isInstanceOf[Object]).toBeFalsy
    }

    it("everything should cast to Object successfully, including null") {
      expect(() => (()             : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (true           : Any).asInstanceOf[Object]).not.toThrow
      expect(() => ('a'            : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (1.toByte       : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (658.toShort    : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (60000          : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (12345678910112L: Any).asInstanceOf[Object]).not.toThrow
      expect(() => (6.5f           : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (12.4           : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (new Object     : Any).asInstanceOf[Object]).not.toThrow
      expect(() => ("hello"        : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (List(1)        : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (Array(1)       : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (Array(Nil)     : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (new js.Object  : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (js.Array(5)    : Any).asInstanceOf[Object]).not.toThrow
      expect(() => (null           : Any).asInstanceOf[Object]).not.toThrow
    }

  }
}
