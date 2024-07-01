/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.testsuite.library

import scala.scalajs.js
import scala.scalajs.js.annotation.JSName

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import org.scalajs.testsuite.utils.Platform._

class StackTraceTest {
  import StackTraceTest._

  @noinline
  private def verifyClassMethodNames(
      places: (String, String)*)(body: => Any): Unit = {
    try {
      body
      throw new AssertionError("body should have thrown an exception")
    } catch {
      case e: RuntimeException => // common superclass of IllegalArgumentException and js.JavaScriptException
        val trace = e.getStackTrace()
        for ((className, methodName) <- places) {
          val found = trace.exists { elem =>
            /* We use startsWith for class name because some VMs will add
             * additional information at the end of the class name, for some
             * reason + there can be a '$class' suffix for methods in impl
             * classes (when default methods are not used by scalac).
             */
            val prefix = "org.scalajs.testsuite.library.StackTraceTest$"
            (elem.getClassName.startsWith(prefix + className) &&
                elem.getMethodName == methodName)
          }

          assertTrue(s"expected class: $className method: $methodName in:\n${trace.mkString("\n")}", found)
        }
    }
  }

  @Test def decodeClassNameAndMethodName(): Unit = {
    assumeTrue("Assume Node.js", executingInNodeJS)
    assumeFalse("Not good enough on WebAssembly yet", executingInWebAssembly)
    assumeFalse("Assume non-minified names", hasMinifiedNames)

    val Error = js.constructorOf[js.Error]
    val oldStackTraceLimit = Error.stackTraceLimit
    val oldThrowJSError = throwJSError
    Error.stackTraceLimit = 20

    try {
      for (jsError <- List(false, true)) {
        throwJSError = jsError

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

        /* For the test with a module initializer, we must use a different
         * module in each iteration, because exceptions happening during the
         * module initializer put their module in a corrupted state.
         */
        if (!jsError) {
          verifyClassMethodNames("Foo" -> "f", "Bar" -> "g",
              "Foobar1$" -> "<clinit>", "Foobar1$" -> "<init>") {
            Foobar1.z
          }
        } else {
          verifyClassMethodNames("Foo" -> "f", "Bar" -> "g",
              "Foobar2$" -> "<clinit>", "Foobar2$" -> "<init>") {
            Foobar2.z
          }
        }

        verifyClassMethodNames(
            "Foo" -> "f",
            "SJS" -> "m", // Scala method actually implementing m()
            "SJS" -> "n"  // Exported JS method forwarding to m()
        ) {
          new SJS().m()
        }
      }
    } finally {
      Error.stackTraceLimit = oldStackTraceLimit
      throwJSError = oldThrowJSError
    }
  }

}

object StackTraceTest {

  var throwJSError: Boolean = false

  trait FooTrait {
    def f(x: Int): Int

    @noinline
    def h(x: Int): Int = f(x - 20)
  }

  class Foo extends FooTrait {
    @noinline
    def f(x: Int): Int = {
      if (x > 10) {
        if (throwJSError)
          js.special.`throw`(new js.Error(x.toString()))
        else
          throw new IllegalArgumentException(x.toString)
      } else {
        x + 4
      }
    }
  }

  class Bar {
    @noinline
    def g(x: Int): Int = new Foo().f(x * 2)
  }

  class Baz {
    val z = new Foo().h(50)
  }

  object Foobar1 {
    val z = new Bar().g(7)
  }

  object Foobar2 {
    val z = new Bar().g(7)
  }

  class SJS extends js.Object {
    @JSName("n")
    @noinline
    def m(): Int = new Foo().f(20)
  }
}
