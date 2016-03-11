/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.library

import scala.scalajs.js

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

class StackTraceTest {
  import StackTraceTest._

  private def verifyClassMethodNames(
      places: (String, String)*)(body: => Any): Unit = {
    try {
      body
      throw new AssertionError("body should have thrown an exception")
    } catch {
      case e: IllegalArgumentException =>
        val trace = e.getStackTrace()
        for ((className, methodName) <- places) {
          assertTrue(trace exists { elem =>
            /* We use startsWith for class name because some VMs will add
             * additional information at the end of the class name, for some
             * reason + there can be a '$class' suffix for methods in impl
             * classes (when default methods are not used by scalac).
             */
            val prefix = "org.scalajs.testsuite.library.StackTraceTest$"
            (elem.getClassName.startsWith(prefix + className) &&
                elem.getMethodName == methodName)
          })
        }
    }
  }

  @Test def decode_class_name_and_method_name(): Unit = {
    assumeTrue("Assume node.js", executingInNodeJS)
    assumeFalse("Assume fullopt-stage", isInFullOpt)

    val Error = js.constructorOf[js.Error]
    val oldStackTraceLimit = Error.stackTraceLimit
    Error.stackTraceLimit = 20

    try {
      verifyClassMethodNames("Foo" -> "f") {
        new Foo().f(25)
      }

      verifyClassMethodNames("Foo" -> "f", "Bar" -> "g") {
        new Bar().g(7)
      }

      verifyClassMethodNames("Foo" -> "f", "FooTrait" -> "h") {
        new Foo().h(78)
      }

      verifyClassMethodNames("Foo" -> "f", "FooTrait" -> "h",
          "Baz" -> "<init>") {
        new Baz()
      }

      verifyClassMethodNames("Foo" -> "f", "Bar" -> "g",
          "Foobar$" -> "<clinit>", "Foobar$" -> "<init>") {
        Foobar.z
      }
    } finally {
      Error.stackTraceLimit = oldStackTraceLimit
    }
  }

}

object StackTraceTest {

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
}
