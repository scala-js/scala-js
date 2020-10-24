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

class NonNativeJSTypeTestScala2 {
  import NonNativeJSTypeTestScala2._

  /** Complements NonNativeJSTypeTest.default_values_for_fields(). */
  @Test def defaultValuesForFields(): Unit = {
    val obj = new DefaultFieldValues

    /* Value class fields are initialized to null, instead of a boxed
     * representation of the zero of their underlying types, as for a
     * Scala class.
     *
     * Scala 3 correctly unboxes `null` as the zero of the underlying type of
     * value classes, which means that this test "fails" in Scala 3, although
     * it is in fact an improvement.
     */
    assertNull(obj.asInstanceOf[js.Dynamic].valueClass)
  }

}

object NonNativeJSTypeTestScala2 {

  class DefaultFieldValues extends js.Object {
    var valueClass: SomeValueClass = _
  }

  class SomeValueClass(val i: Int) extends AnyVal

}
