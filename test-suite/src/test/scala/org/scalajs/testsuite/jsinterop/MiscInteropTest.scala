/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

import org.scalajs.jasminetest.JasmineTest

object MiscInteropTest extends JasmineTest {

  describe("scala.scalajs.js.package") {

    it("should provide an equivalent to `typeof x`") {
      import js.typeOf
      expect(typeOf(5)).toEqual("number")
      expect(typeOf(false)).toEqual("boolean")
      expect(typeOf("hello")).toEqual("string")
      expect(typeOf(null)).toEqual("object")
      expect(typeOf(new js.Object)).toEqual("object")
      expect(typeOf(())).toEqual("undefined")
      expect(typeOf(() => 42)).toEqual("function")
    }

    it("js.constructorOf[T] for native classes") {
      expect(js.constructorOf[js.RegExp]).toBe(js.Dynamic.global.RegExp)
      expect(js.constructorOf[js.Array[_]]).toBe(js.Dynamic.global.Array)
      expect(js.constructorOf[js.Array[Int]]).toBe(js.Dynamic.global.Array)
    }

    it("js.constructorOf[T] for Scala.js-defined JS classes") {
      val concreteCtor = (new ConcreteJSClass).asInstanceOf[js.Dynamic].constructor
      val concreteProto = concreteCtor.prototype.asInstanceOf[js.Object]
      val abstractProto = js.Object.getPrototypeOf(concreteProto)
      val abstractCtor = abstractProto.asInstanceOf[js.Dynamic].constructor

      expect(js.constructorOf[ConcreteJSClass]).toBe(concreteCtor)
      expect(js.constructorOf[AbstractJSClass]).toBe(abstractCtor)

      val concreteInstance = js.Dynamic.newInstance(js.constructorOf[ConcreteJSClass])()
      expect((concreteInstance: Any).isInstanceOf[ConcreteJSClass]).toBeTruthy

      val instance = js.Dynamic.newInstance(
          js.constructorOf[OtherwiseUnreferencedJSClass])(35)
      expect(instance.x).toEqual(35)
    }

  }

  describe("scala.scalajs.js.Object") {

    it("should provide an equivalent to `p in o`") {
      import js.Object.{ hasProperty => hasProp }
      val o = js.Dynamic.literal(foo = 5, bar = "foobar").asInstanceOf[js.Object]
      expect(hasProp(o, "foo")).toBeTruthy
      expect(hasProp(o, "foobar")).toBeFalsy
      expect(hasProp(o, "toString")).toBeTruthy // in prototype
    }

    it("should respect evaluation order for `hasProperty`") {
      import js.Object.{ hasProperty => hasProp }
      var indicator = 3
      def o(): js.Object = {
        indicator += 4
        js.Dynamic.literal(x = 5).asInstanceOf[js.Object]
      }
      def p(): String = {
        indicator *= 2
        "x"
      }
      expect(hasProp(o(), p())).toBeTruthy
      expect(indicator).toEqual(14)
    }

    it("should provide equivalent of JS for-in loop of {} - #13") {
      val obj = js.eval("var dictionaryTest13 = { a: 'Scala.js', b: 7357 }; dictionaryTest13;")
      val dict = obj.asInstanceOf[js.Dictionary[js.Any]]
      var propCount = 0
      var propString = ""

      for (prop <- js.Object.properties(dict)) {
        propCount += 1
        propString += dict(prop)
      }

      expect(propCount).toEqual(2)
      expect(propString).toEqual("Scala.js7357")
    }

    it("should provide equivalent of JS for-in loop of [] - #13") {
      val obj = js.eval("var arrayTest13 = [ 7, 3, 5, 7 ]; arrayTest13;")
      val array = obj.asInstanceOf[js.Dictionary[js.Any]]
      var propCount = 0
      var propString = ""

      for (prop <- js.Object.properties(array)) {
        propCount += 1
        propString += array(prop)
      }

      expect(propCount).toEqual(4)
      expect(propString).toEqual("7357")
    }

    it("should compile js.undefined") {
      expect(() => js.undefined.asInstanceOf[js.Dynamic].toFixed()).toThrow
    }

    it("should allow to define direct subtraits of js.Any") {
      val f = js.Dynamic.literal(
        foo = (x: Int) => x + 1
      ).asInstanceOf[DirectSubtraitOfJSAny]

      expect(f.foo(5)).toEqual(6)
    }

    it("should allow to define direct subclasses of js.Any") {
      val f = js.Dynamic.literal(
        bar = (x: Int) => x + 2
      ).asInstanceOf[DirectSubclassOfJSAny]

      expect(f.bar(5)).toEqual(7)
    }

  }

  describe("Emitted classes") {

    unlessAny("rhino", "fullopt-stage").
    it("should have a meaningful `name` property") {
      def nameOf(obj: Any): js.Any =
        obj.asInstanceOf[js.Dynamic].constructor.name

      expect(nameOf(new SomeScalaClass)).toContain("SomeScalaClass")
      expect(nameOf(new SomeJSClass)).toContain("SomeJSClass")
    }

  }

  @ScalaJSDefined
  abstract class AbstractJSClass extends js.Object

  @ScalaJSDefined
  class ConcreteJSClass extends AbstractJSClass

  @ScalaJSDefined
  class OtherwiseUnreferencedJSClass(val x: Int) extends js.Object

  trait DirectSubtraitOfJSAny extends js.Any {
    def foo(x: Int): Int = js.native
  }

  class DirectSubclassOfJSAny extends js.Any {
    def bar(x: Int): Int = js.native
  }

  class SomeScalaClass

  @ScalaJSDefined
  class SomeJSClass extends js.Object

}
