package org.scalajs.jsenv.test

import org.scalajs.jsenv.nodejs.NodeJSEnv

import org.junit.Test
import org.junit.Assert._

class NodeJSTest extends TimeoutComTests {

  protected def newJSEnv: NodeJSEnv = new NodeJSEnv

  /** Node.js strips double percentage signs - #500 */
  @Test
  def percentageTest: Unit = {
    val counts = 1 to 15
    val argcs  = 1 to 3
    val strings = counts.map("%" * _)

    val strlists = for {
      count  <- argcs
      string <- strings
    } yield List.fill(count)(string)

    val codes = for {
      strlist <- strlists
    } yield {
      val args = strlist.map(s => s""""$s"""").mkString(", ")
      s"console.log($args);\n"
    }

    val result = strlists.map(_.mkString(" ") + "\n").mkString("")

    codes.mkString("").hasOutput(result)
  }

  /** Node.js console.log hack didn't allow to log non-Strings - #561 */
  @Test
  def nonStringTest: Unit = {

    """
    console.log(1);
    console.log(undefined);
    console.log(null);
    console.log({});
    console.log([1,2]);
    """ hasOutput
    """|1
       |undefined
       |null
       |[object Object]
       |1,2
       |""".stripMargin
  }

  @Test
  def slowJSEnvTest: Unit = {
    val com = comRunner("""
      setTimeout(function() {
        scalajsCom.init(function(msg) {
          scalajsCom.send("pong: " + msg);
        });
      }, 1000);
    """)

    val n = 20

    start(com)

    for (_ <- 1 to n)
      com.send("ping")

    for (_ <- 1 to n)
      assertEquals(com.receive(), "pong: ping")

    com.close()
    com.await(DefaultTimeout)
  }

}
