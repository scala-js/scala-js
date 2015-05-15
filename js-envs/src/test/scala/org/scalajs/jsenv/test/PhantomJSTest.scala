package org.scalajs.jsenv.test

import org.scalajs.jsenv.phantomjs.PhantomJSEnv

import org.junit.Test

class PhantomJSTest extends JSEnvTest with ComTests {

  protected def newJSEnv = new PhantomJSEnv

  @Test
  def failureTest = {

    """
    var a = {};
    a.foo();
    """.fails()

  }

  @Test
  def syntaxErrorTest = {

    """
    {
    """.fails()

  }

}
