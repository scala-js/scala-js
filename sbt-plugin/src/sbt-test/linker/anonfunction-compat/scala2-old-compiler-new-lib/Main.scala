package org.scalajs.sbtplugin.test

object Main {
  def main(args: Array[String]): Unit = {
    val xs = List(2, 3, 5, 7, 11)
    xs.foreach(println(_))
    xs.map(i => i * 2).foreach(println(_))
  }
}
