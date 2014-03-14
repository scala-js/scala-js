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
import js.annotation._
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

    it("should offer overloaded exports for methods") {
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

    it("should offer multiple exports for the same method") {
      class Foo {
        @JSExport
        @JSExport("b")
        @JSExport("c")
        def a(): Int = 1
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.a)).toBe("function")
      expect(js.typeOf(foo.b)).toBe("function")
      expect(js.typeOf(foo.c)).toBe("function")

      expect(foo.a()).toEqual(1)
      expect(foo.b()).toEqual(1)
      expect(foo.c()).toEqual(1)
    }

    it("should inherit exports from traits") {
      trait Foo {
        @JSExport
        def x: Int

        @JSExport
        def method(x: Int): Int
      }

      class Bar extends Foo {
        val x = 1
        def method(x: Int) = 2 * x
      }

      val bar = (new Bar).asInstanceOf[js.Dynamic]
      expect(bar.x).toEqual(1)
      expect(js.typeOf(bar.method)).toBe("function")
      expect(bar.method(2)).toEqual(4)
    }

    it("should offer overloading with inherited exports") {
      class A {
        @JSExport
        def foo(x: Int) = 2*x
      }

      class B extends A{
        @JSExport("foo")
        def bar(x: String) = s"Hello $x"
      }

      val b = (new B).asInstanceOf[js.Dynamic]
      expect(js.typeOf(b.foo)).toBe("function")
      expect(b.foo(1)).toEqual(2)
      expect(b.foo("World")).toEqual("Hello World")
    }

    it("should offer exports for generic methods") {
      class Foo {
        @JSExport
        def gen[T <: AnyRef](x: T) = x
      }

      val x = (new Object).asInstanceOf[js.Any]

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.gen)).toBe("function")
      expect(foo.gen(x)).toBe(x)
    }

    it("should offer exports for lambda return types") {
      class Foo {
        @JSExport
        def lambda(x: Int) = (y: Int) => x + y
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.lambda)).toBe("function")

      val lambda = foo.lambda(5).asInstanceOf[Function1[Int,Int]]

      expect(lambda(4)).toEqual(9)
    }

    it("should offer exports for multi parameter lists") {
      class Foo {
        @JSExport
        def multiParam(x: Int)(y: Int): Int = x + y
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.multiParam)).toBe("function")
      expect(foo.multiParam(5,6)).toEqual(11)
    }

    it("should offer exports for default arguments") {
      class Foo {
        @JSExport
        def defArg(x: Int = 1) = x
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.defArg)).toBe("function")
      expect(foo.defArg(5)).toEqual(5)
    }

    it("should offer exports for weird stuff") {
      class UhOh {
        // Something no one should export
        @JSExport
        def ahem[T : Comparable](x: T)(implicit y: Int) = ???
      }

      val x = (new UhOh).asInstanceOf[js.Dynamic]
      expect(js.typeOf(x.ahem)).toBe("function")
    }

    it("should offer exports with value class return types") {
      class Foo {
        @JSExport
        def vc(x: Int) = new SomeValueClass(x)
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]
      expect(js.typeOf(foo.vc)).toBe("function")

      // Unboxed return value
      expect(foo.vc(5)).toBe(5)
    }

    it("should offer exports for overridden methods with refined return type") {
      class A
      class B extends A

      class C1 {
        @JSExport
        def x: A = new A
      }

      class C2 extends C1 {
        override def x: B = new B
      }

      val c2 = (new C2).asInstanceOf[js.Dynamic]
      expect(c2.x.isInstanceOf[B]).toBeTruthy
    }

    it("should offer exports for objects with implicit name") {
      val accessor = js.Dynamic.global.ExportedObject
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

    it("should offer exports for objects with qualified name") {
      val accessor = js.Dynamic.global.qualified.testobject.ExportedObject
      expect(accessor).toBeDefined
      expect(js.typeOf(accessor)).toEqual("function")
      val obj = accessor()
      expect(obj).toBeDefined
      expect(js.typeOf(obj)).toEqual("object")
      expect(obj.witness).toEqual("witness")
    }

    it("should offer exports for classes with implicit name") {
      val constr = js.Dynamic.global.ExportedClass
      expect(constr).toBeDefined
      expect(js.typeOf(constr)).toEqual("function")
      val obj = js.Dynamic.newInstance(constr)(5)
      expect(obj.x).toEqual(5)
    }

    it("should offer exports for classes with explicit name") {
      val constr = js.Dynamic.global.TheExportedClass
      expect(constr).toBeDefined
      expect(js.typeOf(constr)).toEqual("function")
      val obj = js.Dynamic.newInstance(constr)(5)
      expect(obj.x).toEqual(5)
    }

    it("should offer exports for classes with qualified name") {
      val constr = js.Dynamic.global.qualified.testclass.ExportedClass
      expect(constr).toBeDefined
      expect(js.typeOf(constr)).toEqual("function")
      val obj = js.Dynamic.newInstance(constr)(5)
      expect(obj.x).toEqual(5)
    }

    it("should correctly disambiguate overloads involving longs") {

      class Foo {
        @JSExport
        def foo(x: Int) = 1
        @JSExport
        def foo(x: Long) = 2
      }

      val foo = (new Foo).asInstanceOf[js.Dynamic]

      // Create a long factory we can call dynamically to retrieve an unboxed
      // long which is typed as a js.Any
      object LongFactory {
        @JSExport
        def aLong = 1L
      }
      val trueJsLong = LongFactory.asInstanceOf[js.Dynamic].aLong

      expect(foo.foo(1)).toEqual(1)
      expect(foo.foo(trueJsLong)).toEqual(2)
    }

    it("should support exporting under 'org' namespace - #364") {
      val accessor = js.Dynamic.global.org.ExportedUnderOrgObject
      expect(js.typeOf(accessor)).toEqual("function")
      val obj = accessor()
      expect(obj).toBe(ExportedUnderOrgObject.asInstanceOf[js.Any])
    }

  } // describe

  describe("@JSExportDescendentObjects") {

    it("should offer auto exports for objects extending a trait") {
      val accessor =
        js.Dynamic.global.scala.scalajs.test.jsinterop.AutoExportedTraitObject
      expect(accessor).toBeDefined
      expect(js.typeOf(accessor)).toEqual("function")
      val obj = accessor()
      expect(obj).toBeDefined
      expect(obj).toBe(AutoExportedTraitObject.asInstanceOf[js.Any])
    }

    it("should offer auto exports for objects extending a class") {
      val accessor =
        js.Dynamic.global.scala.scalajs.test.jsinterop.AutoExportedClassObject
      expect(accessor).toBeDefined
      expect(js.typeOf(accessor)).toEqual("function")
      val obj = accessor()
      expect(obj).toBeDefined
      expect(obj).toBe(AutoExportedClassObject.asInstanceOf[js.Any])
    }

  }

}

@JSExport
@JSExport("TheExportedObject")
@JSExport("qualified.testobject.ExportedObject") // purposefully halfway the same as ExportedClass
object ExportedObject {
  @JSExport
  def witness: String = "witness"
}

@JSExport
@JSExport("TheExportedClass")
@JSExport("qualified.testclass.ExportedClass") // purposefully halfway the same as ExportedObject
class ExportedClass(_x: Int) {
  @JSExport
  val x = _x
}

@JSExport("org.ExportedUnderOrgObject")
object ExportedUnderOrgObject

@JSExportDescendentObjects
trait AutoExportTrait

object AutoExportedTraitObject extends AutoExportTrait

@JSExportDescendentObjects
class AutoExportClass

object AutoExportedClassObject extends AutoExportClass

class SomeValueClass(val i: Int) extends AnyVal
