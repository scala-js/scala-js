package sbttest.noDOM

object TestApp {
  def main(args: Array[String]): Unit = {
    println(Lib.foo("Hello World"))
    println(Lib.sq(10))
  }
}
