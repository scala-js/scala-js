package org.scalajs.jsenv.test

import org.junit.Test
import org.junit.Assert._

/** Tests that should succeed on any JSEnv */
trait BasicJSEnvTests extends JSEnvTest {

  @Test
  def failureTest: Unit = {

    """
    var a = {};
    a.foo();
    """.fails()

  }

  @Test
  def syntaxErrorTest: Unit = {

    """
    {
    """.fails()

  }

}
