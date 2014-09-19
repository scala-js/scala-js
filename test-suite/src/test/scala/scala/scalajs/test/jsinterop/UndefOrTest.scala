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
import scala.scalajs.test.JasmineTest

import js.annotation.JSExport

object UndefOrTest extends JasmineTest {

  def some[A](v: A): js.UndefOr[A] = v
  def none[A]: js.UndefOr[A] = js.undefined

  describe("scala.scalajs.js.UndefOr[A]") {

    it("convert A to js.UndefOr[A]") {
      val x: js.UndefOr[Int] = 42
      expect(x.isEmpty).toBeFalsy
      expect(x.isDefined).toBeTruthy
      expect(x.nonEmpty).toBeTruthy
      expect(x.get).toEqual(42)
    }

    it("convert undefined to js.UndefOr[A]") {
      val x: js.UndefOr[Int] = js.undefined
      expect(x.isEmpty).toBeTruthy
      expect(x.isDefined).toBeFalsy
      expect(x.nonEmpty).toBeFalsy
      expect(() => x.get).toThrow
    }

    it("convert to js.Any when A <% js.Any") {
      val x: js.UndefOr[Int] = 42
      expect(x).toEqual(42)

      val y: js.UndefOr[String] = js.undefined
      expect(y).toBeUndefined
    }

    it("getOrElse") {
      expect(some("hello").getOrElse("ko")).toEqual("hello")
      expect(none[String].getOrElse("ok")).toEqual("ok")

      var defaultComputed = false
      expect(some("test") getOrElse {
        defaultComputed = true
        "ko"
      }).toEqual("test")
      expect(defaultComputed).toBeFalsy
    }

    it("orNull") {
      expect(some("hello").orNull).toEqual("hello")
      expect(none[String].orNull).toBeNull
    }

    it("map") {
      expect(some(62).map(_ / 3)).toEqual(62 / 3)
      expect(none[Int].map(_ / 3)).toBeUndefined
    }

    it("fold") {
      expect(some(3).fold(10)(_ * 2)).toEqual(6)
      expect(none[Int].fold(10)(_ * 2)).toEqual(10)
    }

    it("flatMap") {
      def f(x: Int): js.UndefOr[Int] = if (x > 0) x+3 else js.undefined
      expect(some(6).flatMap(f)).toEqual(9)
      expect(some(-6).flatMap(f)).toBeUndefined
      expect(none[Int].flatMap(f)).toBeUndefined
    }

    it("flatten") {
      expect(some(some(7)).flatten.isDefined).toBeTruthy
      expect(some(some(7)).flatten.get).toEqual(7)
      expect(some(none[Int]).flatten.isDefined).toBeFalsy
      expect(none[js.UndefOr[Int]].flatten.isDefined).toBeFalsy
    }

    it("filter") {
      expect(some(7).filter(_ > 0).isDefined).toBeTruthy
      expect(some(7).filter(_ > 0).get).toEqual(7)
      expect(some(7).filter(_ < 0).isDefined).toBeFalsy
      expect(none[Int].filter(_ < 0).isDefined).toBeFalsy
    }

    it("filterNot") {
      expect(some(7).filterNot(_ < 0).isDefined).toBeTruthy
      expect(some(7).filterNot(_ < 0).get).toEqual(7)
      expect(some(7).filterNot(_ > 0).isDefined).toBeFalsy
      expect(none[Int].filterNot(_ > 0).isDefined).toBeFalsy
    }

    it("exists") {
      expect(some(7).exists(_ > 0)).toBeTruthy
      expect(some(7).exists(_ < 0)).toBeFalsy
      expect(none[Int].exists(_ > 0)).toBeFalsy
    }

    it("forall") {
      expect(some(7).forall(_ > 0)).toBeTruthy
      expect(some(7).forall(_ < 0)).toBeFalsy
      expect(none[Int].forall(_ > 0)).toBeTruthy
    }

    it("foreach") {
      var witness1 = 3
      some(42).foreach(witness1 = _)
      expect(witness1).toEqual(42)

      var witness2 = 3
      none[Int].foreach(witness2 = _)
      expect(witness2).toEqual(3)
    }

    it("collect") {
      expect(some("hello") collect {
        case "hello" => "ok"
      }).toEqual("ok")
      expect(some("hello") collect {
        case "notthis" => "ko"
      }).toBeUndefined
      expect(none[String] collect {
        case "hello" => "ko"
      }).toBeUndefined
    }

    it("collect should call guard at most once") {
      var witness = 0
      def guard(x: String) = {
        witness += 1
        true
      }
      expect(some("hello") collect {
        case x @ "hello" if guard(x) => "ok"
      }).toEqual("ok")
      expect(witness).toEqual(1)
    }

    it("orElse") {
      expect(some(true) orElse some(false)).toBeTruthy
      expect(some("ok") orElse none).toEqual("ok")
      expect(none orElse some("yes")).toEqual("yes")
      expect(none orElse none).toBeUndefined
    }

    it("toList") {
      import scala.scalajs.js.JSConverters._

      expect(some("hello").toList.toJSArray).toEqual(js.Array("hello"))
      expect(none[String].toList.toJSArray).toEqual(js.Array())
    }

    it("toLeft and toRight") {
      expect(some("left").toLeft("right").isInstanceOf[Left[_, _]]).toBeTruthy
      expect(none[String].toLeft("right").isInstanceOf[Right[_, _]]).toBeTruthy
      expect(some("right").toRight("left").isInstanceOf[Right[_, _]]).toBeTruthy
      expect(none[String].toRight("left").isInstanceOf[Left[_, _]]).toBeTruthy
    }

    it("toOption") {
      expect(some("foo").toOption == Some("foo")).toBeTruthy
      expect(none.toOption == None).toBeTruthy
    }

  }

  describe("scala.scalajs.js.JSConverters.JSRichOption") {

    import js.JSConverters._

    it("should provide orUndefined") {
      expect(Some("asdf").orUndefined).toEqual("asdf")
      expect((None: Option[String]).orUndefined).toBeUndefined

      // This doesn't work on 2.10, since it doesn't infer
      // Nothing <:< js.Any to implicitly convert UndefOr[Nothing] to
      // js.Any
      // expect(None.orUndefined).toBeUndefined
    }

  }

}
