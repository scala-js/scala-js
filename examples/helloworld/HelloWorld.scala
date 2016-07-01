/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation.JSName

object HelloWorld extends js.JSApp {
  def main() {
    switchWithGuardsStat(3, 50)
    switchWithGuardsExpr(3, 50)
  }

  def switchWithGuardsStat(x: Int, y: Int): Unit = {
    x match {
      case 1            => println("one")
      case 2 if y < 10  => println("two special")
      case 2            => println("two")
      case 3 if y < 10  => println("three special")
      case 3 if y > 100 => println("three big special")
      case z if y > 100 => println("big " + z)
      case _            => println("None of those")
    }
  }

  def switchWithGuardsExpr(x: Int, y: Int): Unit = {
    val message = x match {
      case 1            => "one"
      case 2 if y < 10  => "two special"
      case 2            => "two"
      case 3 if y < 10  => "three special"
      case 3 if y > 100 => "three big special"
      case z if y > 100 => "big " + z
      case _            => "None of those"
    }
    println(message)
  }

  // To have a nicer JS display for demo
  @noinline
  def println(x: Any): Unit = Console.println(x)
}
