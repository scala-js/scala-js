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

  @Test // Failed in Phantom - #2053
  def utf8Test: Unit = {

    """
    console.log("\u1234");
    """ hasOutput "\u1234\n";

  }

  @Test
  def allowScriptTags: Unit = {

    """
    console.log("<script></script>");
    """ hasOutput "<script></script>\n";

  }

}
