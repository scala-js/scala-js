/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.jsinterop

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Test

import org.scalajs.testsuite.utils.AssertThrows._
import org.scalajs.testsuite.utils.JSAssert._

class UndefOrTest {

  def some[A](v: A): js.UndefOr[A] = v
  def none[A]: js.UndefOr[A] = js.undefined

  // scala.scalajs.js.UndefOr[A]

  @Test def convert_A_to_js_UndefOr_A(): Unit = {
    val x: js.UndefOr[Int] = 42
    assertFalse(x.isEmpty)
    assertTrue(x.isDefined)
    assertTrue(x.nonEmpty)
    assertEquals(42, x.get)
  }

  @Test def convert_undefined_to_js_UndefOr_A(): Unit = {
    val x: js.UndefOr[Int] = js.undefined
    assertTrue(x.isEmpty)
    assertFalse(x.isDefined)
    assertFalse(x.nonEmpty)
    assertThrows(classOf[NoSuchElementException], x.get)
  }

  @Test def explicitly_convert_A_to_js_UndefOr_A(): Unit = {
    val x: js.UndefOr[Int] = js.defined(42)
    assertFalse(x.isEmpty)
    assertEquals(42, x.get)

    val f: js.UndefOr[js.Function1[Int, Int]] = js.defined((x: Int) => x + 1)
    assertFalse(f.isEmpty)
    assertEquals(6, f.get(5))
  }

  @Test def `convert_to_js_Any_when_A_<%_js_Any`(): Unit = {
    val x: js.UndefOr[Int] = 42
    assertEquals(42, x)

    val y: js.UndefOr[String] = js.undefined
    assertJSUndefined(y)
  }

  @Test def getOrElse(): Unit = {
    assertEquals("hello", some("hello").getOrElse("ko"))
    assertEquals("ok", none[String].getOrElse("ok"))

    var defaultComputed = false
    assertEquals("test", some("test") getOrElse {
      defaultComputed = true
      "ko"
    })
    assertFalse(defaultComputed)
  }

  @Test def orNull(): Unit = {
    assertEquals("hello", some("hello").orNull)
    assertNull(none[String].orNull)
  }

  @Test def map(): Unit = {
    assertEquals(62 / 3, some(62).map(_ / 3))
    assertJSUndefined(none[Int].map(_ / 3))
  }

  @Test def fold(): Unit = {
    assertEquals(6, some(3).fold(10)(_ * 2))
    assertEquals(10, none[Int].fold(10)(_ * 2))
  }

  @Test def flatMap(): Unit = {
    def f(x: Int): js.UndefOr[Int] = if (x > 0) x+3 else js.undefined
    assertEquals(9, some(6).flatMap(f))
    assertJSUndefined(some(-6).flatMap(f))
    assertJSUndefined(none[Int].flatMap(f))
  }

  @Test def flatten(): Unit = {
    assertTrue(some(some(7)).flatten.isDefined)
    assertEquals(7, some(some(7)).flatten.get)
    assertFalse(some(none[Int]).flatten.isDefined)
    assertFalse(none[js.UndefOr[Int]].flatten.isDefined)
  }

  @Test def filter(): Unit = {
    assertTrue(some(7).filter(_ > 0).isDefined)
    assertEquals(7, some(7).filter(_ > 0).get)
    assertFalse(some(7).filter(_ < 0).isDefined)
    assertFalse(none[Int].filter(_ < 0).isDefined)
  }

  @Test def filterNot(): Unit = {
    assertTrue(some(7).filterNot(_ < 0).isDefined)
    assertEquals(7, some(7).filterNot(_ < 0).get)
    assertFalse(some(7).filterNot(_ > 0).isDefined)
    assertFalse(none[Int].filterNot(_ > 0).isDefined)
  }

  @Test def contains(): Unit = {
    assertTrue(some(7).contains(7))
    assertFalse(some(7).contains(8))
    assertFalse(none[Int].contains(7))

    assertFalse(some(()).contains(()))
  }

  @Test def exists(): Unit = {
    assertTrue(some(7).exists(_ > 0))
    assertFalse(some(7).exists(_ < 0))
    assertFalse(none[Int].exists(_ > 0))
  }

  @Test def forall(): Unit = {
    assertTrue(some(7).forall(_ > 0))
    assertFalse(some(7).forall(_ < 0))
    assertTrue(none[Int].forall(_ > 0))
  }

  @Test def foreach(): Unit = {
    var witness1 = 3
    some(42).foreach(witness1 = _)
    assertEquals(42, witness1)

    var witness2 = 3
    none[Int].foreach(witness2 = _)
    assertEquals(3, witness2)
  }

  @Test def collect(): Unit = {
    assertEquals("ok", some("hello") collect {
      case "hello" => "ok"
    })
    assertTrue(js.isUndefined(some("hello") collect {
      case "notthis" => "ko"
    }))
    assertTrue(js.isUndefined(none[String] collect {
      case "hello" => "ko"
    }))
  }

  @Test def collect_should_call_guard_at_most_once(): Unit = {
    var witness = 0
    def guard(x: String): Boolean = {
      witness += 1
      true
    }
    assertEquals("ok", some("hello") collect {
      case x @ "hello" if guard(x) => "ok"
    })
    assertEquals(1, witness)
  }

  @Test def orElse(): Unit = {
    assertTrue((some(true) orElse some(false)).get)
    assertEquals("ok", some("ok") orElse none)
    assertEquals("yes", none orElse some("yes"))
    assertJSUndefined(none orElse none)

    // #2095
    assertEquals("ok", some("ok") orElse "yes")
    assertEquals("yes", none orElse "yes")
  }

  @Test def toList(): Unit = {
    assertEquals(List("hello"), some("hello").toList)
    assertEquals(List.empty[String], none[String].toList)
  }

  @Test def toLeft_and_toRight(): Unit = {
    assertTrue(some("left").toLeft("right").isInstanceOf[Left[_, _]])
    assertTrue(none[String].toLeft("right").isInstanceOf[Right[_, _]])
    assertTrue(some("right").toRight("left").isInstanceOf[Right[_, _]])
    assertTrue(none[String].toRight("left").isInstanceOf[Left[_, _]])
  }

  @Test def toOption(): Unit = {
    assertTrue(some("foo").toOption == Some("foo"))
    assertTrue(none.toOption == None)
  }

  // scala.scalajs.js.JSConverters.JSRichOption

  import js.JSConverters._

  @Test def should_provide_orUndefined(): Unit = {
    assertEquals("asdf", Some("asdf").orUndefined)
    assertJSUndefined((None: Option[String]).orUndefined)
    assertJSUndefined(None.orUndefined)
  }

}
