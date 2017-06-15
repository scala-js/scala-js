package org.scalajs.jsenv.test

import org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv

import org.junit.Test
import org.junit.Assert._

class JSDOMNodeJSEnvTest extends TimeoutComTests {

  protected def newJSEnv: JSDOMNodeJSEnv = new JSDOMNodeJSEnv()

  @Test
  def historyAPI: Unit = {
    """|console.log(window.location.href);
       |window.history.pushState({}, "", "/foo");
       |console.log(window.location.href);
    """.stripMargin hasOutput
    """|http://localhost/
       |http://localhost/foo
       |""".stripMargin
  }

}
