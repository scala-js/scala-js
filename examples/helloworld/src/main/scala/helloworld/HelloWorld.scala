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

  def sleep(ms: Int): js.Promise[Unit] = {
    new js.Promise[Unit]({ (resolve, reject) =>
      js.timers.setTimeout(ms)(resolve(()))
    })
  }

  def myAsyncProgram(n: Int): js.Promise[Int] = js.async {
    var sum = 0
    var i = 0
    while (i != n) {
      js.await(HelloWorld.sleep(750))
      i += 1
      println(i)
      sum += i
    }
    i
  }

  def main(args: Array[String]): Unit = {
    /*
    // Works on JS but not on WebAssembly
    val g = myGenerator(5)
    for (k <- 0 until 8)
      js.Dynamic.global.console.log(g.next(k))
    */

    val p = myAsyncProgram(5)
    p.`then`[Unit]({ (i: Int) =>
      println(i)
    })
  }
}
