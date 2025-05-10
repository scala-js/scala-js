/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation._

object HelloWorld {
  def myGenerator(n: Int): js.Generator[Int, String, Int] = js.Generator[Int, String, Int] { implicit ev =>
    println("one")
    js.Generator.`yield`(42)
    println("two")
    var i = 0
    var j = 0
    while (i != n) {
      j += js.Generator.`yield`(j)
      i += 1
    }
    "result"
  }

  def main(args: Array[String]): Unit = {
    println("hello")

    /*
    // Works on JS but not on WebAssembly
    val g = myGenerator(5)
    for (k <- 0 until 8)
      js.Dynamic.global.console.log(g.next(k))
    */
  }
}
