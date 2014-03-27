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

    it("should offer exports for variable argument methods - #393") {
      class A {
        @JSExport
        def foo(i: String*) = i.mkString("|")
      }

      val a = (new A).asInstanceOf[js.Dynamic]

      expect(a.foo()).toEqual("")
      expect(a.foo("a", "b", "c")).toEqual("a|b|c")
      expect(a.foo("a", "b", "c", "d")).toEqual("a|b|c|d")
    }

    it("should correctly overload in view of difficult repeated parameter lists") {
      class A {
        @JSExport
        def foo(a: String, b: String, i: Int, c: String) = 1

        @JSExport
        def foo(a: String*) = 2

        @JSExport
        def foo(x: Int)(a: Int*) = x * 100000 + a.sum
      }

      val a = (new A).asInstanceOf[js.Dynamic]

      expect(a.foo()).toEqual(2)
      expect(a.foo("asdf")).toEqual(2)
      expect(a.foo("asdf", "foo")).toEqual(2)
      expect(a.foo("asdf", "foo", "bar")).toEqual(2)
      expect(a.foo("asdf", "foo", 1, "bar")).toEqual(1)
      expect(a.foo("asdf", "foo", "foo", "bar")).toEqual(2)
      expect(a.foo(5, 1, 2, 3, 10)).toEqual(500016)
      expect(a.foo(1)).toEqual(100000)
    }

    it("should offer exports with default arguments") {
      class A {
        var oneCount: Int = 0
        def one = {
          oneCount += 1
          1
        }
        @JSExport
        def foo(a: Int = one)(b: Int = a + one)(c: Int = b + one) =
          a + b + c
      }

      val a = new A
      val jsa = a.asInstanceOf[js.Dynamic]

      expect(jsa.foo()).toEqual(6)
      expect(a.oneCount).toEqual(3)

      expect(jsa.foo(2)).toEqual(9)
      expect(a.oneCount).toEqual(5)

      expect(jsa.foo(2,4)).toEqual(11)
      expect(a.oneCount).toEqual(6)

      expect(jsa.foo(2,4,10)).toEqual(16)
      expect(a.oneCount).toEqual(6)

      expect(jsa.foo((),4,10)).toEqual(15)
      expect(a.oneCount).toEqual(7)

      expect(jsa.foo((),4)).toEqual(10)
      expect(a.oneCount).toEqual(9)
    }

    it("should correctly overload methods in presence of default parameters") {
      class A {
        @JSExport
        def foo(a: Int)(b: Int = 5)(c: Int = 7) = 1000 + a + b + c

        @JSExport
        def foo(a: Int, b: String) = 2

        @JSExport
        def foo(a: Int, b: Int, c: String) = 3
      }

      val a = (new A).asInstanceOf[js.Dynamic]

      expect(a.foo(1)).toEqual(1013)
      expect(a.foo(1, 4)).toEqual(1012)
      expect(a.foo(1, 4, 5)).toEqual(1010)
      expect(a.foo(1, "foo")).toEqual(2)
      expect(a.foo(1, 2, "foo")).toEqual(3)

    }

    it("should prefer overloads taking a js.Undefined over methods with default parameters") {
      class A {
        @JSExport
        def foo(a: Int)(b: String = "asdf") = s"$a $b"

        @JSExport
        def foo(a: Int, b: js.Undefined) = "woot"
      }

      val a = (new A).asInstanceOf[js.Dynamic]

      expect(a.foo(1)).toEqual("1 asdf")
      expect(a.foo(2, "omg")).toEqual("2 omg")
      expect(a.foo(1, ())).toEqual("woot")

    }

    it("should correctly overload methods in presence of default parameters and repeated parameters") {
      class A {
        @JSExport
        def foo(x: Int, y: Int = 1) = x + y
        @JSExport
        def foo(x: String*) = x.mkString("|")
      }

      val a = (new A).asInstanceOf[js.Dynamic]

      expect(a.foo(1)).toEqual(2)
      expect(a.foo(1, 2)).toEqual(3)
      expect(a.foo()).toEqual("")
      expect(a.foo("foo")).toEqual("foo")
      expect(a.foo("foo","bar")).toEqual("foo|bar")

    }


    xit("should correctly box repeated parameter lists with value classes") {
      // Only in v0.5
      class A {
        @JSExport
        def vc(vcs: SomeValueClass*) = vcs.map(_.i).sum
      }

      val vc1 = new SomeValueClass(1).asInstanceOf[js.Any]
      val vc2 = new SomeValueClass(2).asInstanceOf[js.Any]
      val a = (new A).asInstanceOf[js.Dynamic]

      expect(a.foo(vc1, vc2)).toEqual(3)
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
