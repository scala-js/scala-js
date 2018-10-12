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

package org.scalajs.testsuite.jsinterop

import scala.scalajs.js
import scala.scalajs.js.annotation._

/** Elements of ScalaJSDefinedTest in `src/test/` to be compiled in a
 *  separate compiler run, to test separate compilation.
 */
object ScalaJSDefinedTestSeparateRun {

  class SimpleParentClass extends js.Object {
    def foo(x: Int): Int = x + 1
  }

  class SimpleChildClass extends SimpleParentClass {
    override def foo(x: Int): Int = x + 3
  }

  trait SimpleTrait extends js.Any {
    def foo(x: Int): Int
  }

}
