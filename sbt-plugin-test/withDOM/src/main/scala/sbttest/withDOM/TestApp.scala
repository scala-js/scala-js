package sbttest.withDOM

object TestApp {

  def main(args: Array[String]): Unit = {
    Lib.appendDocument("Hello World")
    Lib.appendDocument("Still Here!")

    println(Lib.jQuery("p").text())
  }

}
