/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.scalajs.js
import org.scalajs.jasminetest.JasmineTest

object StackTraceTest extends JasmineTest {

  trait FooTrait {
    def f(x: Int): Int

    @noinline
    def h(x: Int): Int = f(x - 20)
  }

  class Foo extends FooTrait {
    @noinline
    def f(x: Int): Int = {
      if (x > 10)
        throw new IllegalArgumentException(x.toString)
      else
        x + 4
    }
  }

  class Bar {
    @noinline
    def g(x: Int): Int = new Foo().f(x * 2)
  }

  class Baz {
    val z = new Foo().h(50)
  }

  object Foobar {
    val z = new Bar().g(7)
  }

  private def verifyClassMethodNames(
      places: (String, String)*)(body: => Any): Unit = {
    try {
      body
      throw new AssertionError("body should have thrown an exception")
    } catch {
      case e: IllegalArgumentException =>
        val trace = e.getStackTrace()
        for ((className, methodName) <- places) {
          expect(trace exists { elem =>
            /* We use startsWith for class name because some VMs will add
             * additional information at the end of the class name, for some
             * reason.
             */
            val prefix = "org.scalajs.testsuite.library.StackTraceTest$"
            (elem.getClassName.startsWith(prefix + className) &&
                elem.getMethodName == methodName)
          }).toBeTruthy
        }
    }
  }

  describe("scala.scalajs.runtime.StackTrace") {

    when("nodejs").
    unlessAny("fullopt-stage", "strong-mode").
    it("decode class name and method name") {
      verifyClassMethodNames("Foo" -> "f") {
        new Foo().f(25)
      }

      verifyClassMethodNames("Foo" -> "f", "Bar" -> "g") {
        new Bar().g(7)
      }

      verifyClassMethodNames("Foo" -> "f", "FooTrait$class" -> "h") {
        new Foo().h(78)
      }

      verifyClassMethodNames("Foo" -> "f", "FooTrait$class" -> "h",
          "Baz" -> "<init>") {
        new Baz()
      }

      verifyClassMethodNames("Foo" -> "f", "Bar" -> "g",
          "Foobar$" -> "<clinit>", "Foobar$" -> "<init>") {
        Foobar.z
      }
    }

  }

}
