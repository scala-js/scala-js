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

class ReflectTest extends DirectTest with TestHelpers {

  override def preamble: String =
    """import scala.scalajs.js, js.annotation._
       import scala.scalajs.reflect.annotation._
    """

  @Test
  def noEnableReflectiveInstantiationOnJSType: Unit = {
    """
    @EnableReflectiveInstantiation
    class A extends js.Object

    @EnableReflectiveInstantiation
    trait B extends js.Object

    @EnableReflectiveInstantiation
    object C extends js.Object

    @EnableReflectiveInstantiation
    @js.native
    @JSGlobal
    class D extends js.Object

    @EnableReflectiveInstantiation
    @js.native
    trait E extends js.Object

    @EnableReflectiveInstantiation
    @js.native
    @JSGlobal
    object F extends js.Object
    """.hasErrors("""
      |newSource1.scala:4: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:7: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:10: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:13: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:18: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:22: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
    """)
  }

}
