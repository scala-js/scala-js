package org.scalajs.sbtplugin.test

object Main {
  def main(args: Array[String]): Unit = useData()

  @noinline
  def useData(): Unit = {
    val d = Data(5)
    println(d.count)
  }
}
