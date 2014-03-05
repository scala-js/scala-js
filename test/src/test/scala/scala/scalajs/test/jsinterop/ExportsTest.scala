/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package jsinterop

import scala.scalajs.js
import js.annotation.JSExport
import scala.scalajs.test.JasmineTest

object ExportsTest extends JasmineTest {

  describe("@JSExport") {

    it("should offer exports for methods with implicit name") {
      class Foo {
        @JSExport
        def bar(): Int = 42
        @JSExport
        def double(x: Int): Int = x*2
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.bar)).toBe("function")
      expect(foo.bar()).toEqual(42)
      expect(foo.double(3)).toEqual(6)
    }

    it("should offer exports for methods with explicit name") {
      class Foo {
        @JSExport("theAnswer")
        def bar(): Int = 42
        @JSExport("doubleTheParam")
        def double(x: Int): Int = x*2
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(foo.bar).toBeUndefined
      expect(js.typeOf(foo.theAnswer)).toBe("function")
      expect(foo.theAnswer()).toEqual(42)
      expect(foo.doubleTheParam(3)).toEqual(6)
    }

    it("should offer exports for properties with implicit name") {
      class Foo {
        private[this] var myY: String = "hello"
        @JSExport
        val answer: Int = 42
        @JSExport
        var x: Int = 3
        @JSExport
        def doubleX: Int = x*2
        @JSExport
        def y: String = myY + " get"
        @JSExport
        def y_=(v: String): Unit = myY = v + " set"
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.answer)).toBe("number")
      expect(foo.answer).toEqual(42)
      expect(foo.x).toEqual(3)
      expect(foo.doubleX).toEqual(6)
      foo.x = 23
      expect(foo.x).toEqual(23)
      expect(foo.doubleX).toEqual(46)
      expect(foo.y).toEqual("hello get")
      foo.y = "world"
      expect(foo.y).toEqual("world set get")
    }

    it("should offer exports for properties with explicit name") {
      class Foo {
        private[this] var myY: String = "hello"
        @JSExport("answer")
        val answerScala: Int = 42
        @JSExport("x")
        var xScala: Int = 3
        @JSExport("doubleX")
        def doubleXScala: Int = xScala*2
        @JSExport("y")
        def yGetter: String = myY + " get"
        @JSExport("y")
        def ySetter_=(v: String): Unit = myY = v + " set"
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(foo.answerScala).toBeUndefined
      expect(js.typeOf(foo.answer)).toBe("number")
      expect(foo.answer).toEqual(42)
      expect(foo.x).toEqual(3)
      expect(foo.doubleX).toEqual(6)
      foo.x = 23
      expect(foo.x).toEqual(23)
      expect(foo.doubleX).toEqual(46)
      expect(foo.y).toEqual("hello get")
      foo.y = "world"
      expect(foo.y).toEqual("world set get")
    }

    it("should offer multiple exports for methods") {
      class Foo {
        @JSExport("foobar")
        def foo(): Int = 42
        @JSExport("foobar")
        def bar(x: Int): Int = x*2
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.foobar)).toBe("function")
      expect(foo.foobar()).toEqual(42)
      expect(foo.foobar(3)).toEqual(6)
    }

    it("should offer exports for objects with implicit name") {
      val accessor = js.Dynamic.global.selectDynamic("ExportsTest$ExportedObject")
      expect(accessor).toBeDefined
      expect(js.typeOf(accessor)).toEqual("function")
      val obj = accessor()
      expect(obj).toBeDefined
      expect(js.typeOf(obj)).toEqual("object")
      expect(obj.witness).toEqual("witness")
    }

    it("should offer exports for objects with explicit name") {
      val accessor = js.Dynamic.global.TheExportedObject
      expect(accessor).toBeDefined
      expect(js.typeOf(accessor)).toEqual("function")
      val obj = accessor()
      expect(obj).toBeDefined
      expect(js.typeOf(obj)).toEqual("object")
      expect(obj.witness).toEqual("witness")
    }

    it("should offer exports for classes with implicit name") {
      val constr = js.Dynamic.global.selectDynamic("ExportsTest$ExportedClass")
      expect(constr).toBeDefined
      expect(js.typeOf(constr)).toEqual("function")
      val obj = js.Dynamic.newInstance(constr)(5)
      expect(obj.x).toEqual(5)
    }

    it("should offer exports for classes with explicit name") {
      val constr = js.Dynamic.global.selectDynamic("TheExportedClass")
      expect(constr).toBeDefined
      expect(js.typeOf(constr)).toEqual("function")
      val obj = js.Dynamic.newInstance(constr)(5)
      expect(obj.x).toEqual(5)
    }

  } // describe

  @JSExport
  @JSExport("TheExportedObject")
  object ExportedObject {
    @JSExport
    def witness: String = "witness"
  }

  @JSExport
  @JSExport("TheExportedClass")
  class ExportedClass(_x: Int) {
    @JSExport
    val x = _x
  }

}
