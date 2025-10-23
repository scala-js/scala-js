/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import scala.scalajs.js
import js.annotation._

object HelloWorld {

  def main(args: Array[String]): Unit = {
    /*val x: Option[Int] = hide[Option[Int]](Some(5))
    println(x.get)
    println(x.get)*/
    import java.lang.{Integer => JInt}
    import java.util.{Arrays, Collections}

    // Create a sorted list of boxed Integers
    val list = Arrays.asList(1: JInt, 3: JInt, 5: JInt, 7: JInt)

    // Search for the value 3
    val key  = 3: JInt
    val idx  = Collections.binarySearch(list, key)

    if (idx >= 0)
      println(s"Found $key at index $idx")
    else
      println(s"$key not found (insertion point: ${-idx - 1})")
  }

  @noinline def hide[T](x: T): T = x

  @noinline def println(x: Any): Unit =
    Predef.println(x)
}