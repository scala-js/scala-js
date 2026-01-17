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

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.junit.Test

class BinaryCompatTest extends JSASTTest {

  @Test
  def emitDefaultAccessorsOfJSNativeDefs(): Unit = {
    val XDefaultAccessorName = MethodName("foo$default$1", Nil, IntRef)

    /* Check that, even with the fix to #4553, we still emit default accessors
     * for JS native defs, unless they are `= js.native`.
     */
    """
    import scala.scalajs.js, js.annotation._

    object Container {
      @js.native
      @JSGlobal("foo")
      def foo(x: Int = 5): Int = js.native

      def bar(x: Int): Int = x
    }
    """.hasExactly(1, "default accessor for x in foo") {
      case MethodDef(flags, MethodIdent(XDefaultAccessorName), _, _, _, _) =>
    }

    // Check that it is not emitted for `= js.native`.
    """
    import scala.scalajs.js, js.annotation._

    object Container {
      @js.native
      @JSGlobal("foo")
      def foo(x: Int = js.native): Int = js.native

      def bar(x: Int): Int = x
    }
    """.hasNot("default accessor for x in foo") {
      case MethodDef(flags, MethodIdent(XDefaultAccessorName), _, _, _, _) =>
    }

  }

}
