/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation.JSName

object HelloWorld extends js.JSApp {
  def main() {
    rangeForeach1(10)
    println("---")
    rangeForeach2(10)
    println("---")
    rangeForeach3(10)
    println("---")
    rangeForeach4(10)
    println("---")
    rangeSum3(10)
  }

  def rangeForeach1(n: Int): Unit = {
    for (i <- 0 until n)
      println(i)
  }

  def rangeForeach2(n: Int): Unit = {
    for (i <- -3 to n)
      println(i)
  }

  def rangeForeach3(n: Int): Unit = {
    for (i <- 0 until n by 3)
      println(i)
  }

  def rangeForeach4(n: Int): Unit = {
    for (i <- -3 to n by 3)
      println(i)
  }

  def rangeSum3(n: Int): Unit = {
    val r = 0 until n by 3
    println(r.sum)
  }

  @noinline def println(x: Any): Unit = Console.println(x)
}
