package sbttest.withDOM

import scala.scalajs.js

object TestApp extends js.JSApp {

  def main(): Unit = {
    Lib.appendDocument("Hello World")
    Lib.appendDocument("Still Here!")

    println(Lib.getElementsByTagName("p").head.innerHTML)
  }

}
