package scala.scalajs.sbtplugin.test.env

import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv

import org.junit.Test

class NodeJSTest extends JSEnvTest {

  protected def newJSEnv = new NodeJSEnv

  /** Node.js strips double percentage signs - #500 */
  @Test
  def percentageTest = {
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
  def nonStringTest = {

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

}
