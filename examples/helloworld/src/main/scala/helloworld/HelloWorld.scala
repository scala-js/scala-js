/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation._

object HelloWorld {
  def main(args: Array[String]): Unit = {
    println(foo(2, 3, 5))
    println(bar("hello", "world"))
  }

  def foo(xs: Int*): Int = xs.sum

  def bar(xs: String*): String = xs.foldLeft("")(_ + _)
}
