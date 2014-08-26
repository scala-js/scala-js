package scala.scalajs

import scala.collection.GenTraversableOnce

package object runtime {

  @inline final def genTraversableOnce2jsArray[A](
      col: GenTraversableOnce[A]): js.Array[A] = {
    val result = new js.Array[A]
    col.foreach(x => result.push(x))
    result
  }

  /** Information about the environment Scala.js runs in. */
  def environmentInfo: js.Dynamic = sys.error("stub")

}
