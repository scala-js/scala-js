package scala.scalajs.runtime

import scala.collection.GenTraversableOnce
import scala.scalajs.js

object Compat {

  type CompatTraversableOnce[+A] = GenTraversableOnce[A]

  @inline def genTraversableOnce2jsArrayImpl[A](
      col: CompatTraversableOnce[A]): js.Array[A] = {
    col match {
      case col: js.ArrayOps[A]     => col.result()
      case col: js.WrappedArray[A] => col.array
      case _ =>
        val result = new js.Array[A]
        col.foreach(x => result.push(x))
        result
    }
  }

}
