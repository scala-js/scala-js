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

package org.scalajs.nscplugin.test

import org.scalajs.nscplugin.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

class JSNewTargetTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """import scala.scalajs.js
    """

  @Test
  def illegalInScalaClass(): Unit = {

    """
    class A {
      js.`new`.target

      def this(x: Int) = {
        this()
        js.`new`.target
      }
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      js.`new`.target
      |               ^
      |newSource1.scala:8: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |        js.`new`.target
      |                 ^
    """

    """
    class A {
      def foo(x: Int): Unit =
        js.`new`.target
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |        js.`new`.target
      |                 ^
    """

    """
    class A extends js.Object {
      class B {
        js.`new`.target
      }
    }
    """ hasErrors
    """
      |newSource1.scala:5: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |        js.`new`.target
      |                 ^
    """

  }

  @Test
  def illegalInDefOrLazyVal(): Unit = {

    """
    class A extends js.Object {
      lazy val x = js.`new`.target
      def y: js.Dynamic = js.`new`.target
      def z(x: Int): Any = js.`new`.target
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      lazy val x = js.`new`.target
      |                            ^
      |newSource1.scala:5: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      def y: js.Dynamic = js.`new`.target
      |                                   ^
      |newSource1.scala:6: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      def z(x: Int): Any = js.`new`.target
      |                                    ^
    """

  }

  @Test
  def illegalInLambdaOrByName(): Unit = {

    """
    class A extends js.Object {
      val x = () => js.`new`.target
      val y = Option(null).getOrElse(js.`new`.target)
      val z: js.Function1[Int, Any] = (x: Int) => js.`new`.target
      val w: js.ThisFunction0[Any, Any] = (x: Any) => js.`new`.target
    }
    """ hasErrors
    """
      |newSource1.scala:4: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      val x = () => js.`new`.target
      |                             ^
      |newSource1.scala:5: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      val y = Option(null).getOrElse(js.`new`.target)
      |                                              ^
      |newSource1.scala:6: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      val z: js.Function1[Int, Any] = (x: Int) => js.`new`.target
      |                                                           ^
      |newSource1.scala:7: error: Illegal use of js.`new`.target.
      |It can only be used in the constructor of a JS class, as a statement or in the rhs of a val or var.
      |It cannot be used inside a lambda or by-name parameter, nor in any other location.
      |      val w: js.ThisFunction0[Any, Any] = (x: Any) => js.`new`.target
      |                                                               ^
    """

  }

}
