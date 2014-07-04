package scala.scalajs.sbtplugin.test.env

import scala.scalajs.sbtplugin.env.phantomjs.PhantomJSEnv

import org.junit.Test

class PhantomJSTest extends JSEnvTest {

  protected def newJSEnv = new PhantomJSEnv

  @Test
  def failureTest = {

    """
    var a = {};
    a.foo();
    """.fails()

  }

}
