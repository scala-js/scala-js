package envvars

import scala.scalajs.js
import scala.scalajs.js.annotation._

object Platform {
  def getEnvVarOpt(envVar: String): Option[String] =
    process.env.get(envVar)

  @js.native
  @JSGlobal("process")
  private object process extends js.Object {
    val env: js.Dictionary[String] = js.native
  }
}
