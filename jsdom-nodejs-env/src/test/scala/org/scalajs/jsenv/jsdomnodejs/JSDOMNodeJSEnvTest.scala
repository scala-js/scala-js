package org.scalajs.jsenv.jsdomnodejs

import org.scalajs.jsenv.test._

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
