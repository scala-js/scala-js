package sbttest.noDOM

import scala.scalajs.js

object TestApp extends js.JSApp {

  def main(): Unit = {
    println(Lib.foo("Hello World"))
    println(Lib.sq(10))
  }

}
