import scala.scalajs.js

object TestMain extends js.JSApp {
  def main(): Unit = {
    // Use JS console to make sure this doesn't work on the JVM
    js.Dynamic.global.console.log("Test main")
  }
}
