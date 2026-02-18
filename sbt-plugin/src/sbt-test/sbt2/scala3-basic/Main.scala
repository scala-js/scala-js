package test

import scala.scalajs.js

/* We use an `enum` to make sure that this code is compiled by Scala 3, and
 * that the correct scala3-library_sjs1 is used (it is required for the
 * automatic parent `scala.runtime.Enum`).
 */
enum Color {
  case Red, Green, Blue
}

object Main {
  def main(args: Array[String]): Unit = {
    println("Hello Scala.js")

    // Testing the Scala 3 runtime library
    println(Color.Red)
    assert(Color.Red.toString() == "Red")

    // Testing the undefOr2jsAny implicit conversion
    val x: js.Any = js.defined("")
  }
}
