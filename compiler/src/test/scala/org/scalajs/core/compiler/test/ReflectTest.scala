package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
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
    @ScalaJSDefined
    class A extends js.Object

    @EnableReflectiveInstantiation
    @ScalaJSDefined
    trait B extends js.Object

    @EnableReflectiveInstantiation
    @ScalaJSDefined
    object C extends js.Object

    @EnableReflectiveInstantiation
    @js.native
    class D extends js.Object

    @EnableReflectiveInstantiation
    @js.native
    trait E extends js.Object

    @EnableReflectiveInstantiation
    @js.native
    object F extends js.Object
    """ hasErrors
    """
      |newSource1.scala:4: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:8: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:12: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:16: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:20: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
      |newSource1.scala:24: error: @EnableReflectiveInstantiation cannot be used on types extending js.Any.
      |    @EnableReflectiveInstantiation
      |     ^
    """
  }

}
