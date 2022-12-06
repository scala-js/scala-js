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

import org.junit.Assert._
import org.junit.Test

/** Extracted from JSOptionalTest212.scala because Scala 3 cannot infer the
 *  parameter type yet when using true union types.
 *
 *  See https://github.com/lampepfl/dotty/issues/11694
 */
class JSOptionalTest212FunParamInference {
  import JSOptionalTest212FunParamInference._

  @Test def traitWithOptionalFunction(): Unit = {
    val obj = new TraitWithOptionalFunction {
      override val f = js.defined(x => x + 1)
    }

    assertEquals("function", js.typeOf(obj.f))
    assertEquals(6, obj.f.get(5))
  }
}

object JSOptionalTest212FunParamInference {
  trait TraitWithOptionalFunction extends js.Object {
    val f: js.UndefOr[js.Function1[Int, Int]] = js.undefined
  }
}
