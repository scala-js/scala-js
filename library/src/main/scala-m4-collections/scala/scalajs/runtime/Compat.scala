package scala.scalajs.runtime

import scala.collection.IterableOnce

import scala.scalajs.js

private[runtime] object Compat {

  type CompatTraversableOnce[+A] = IterableOnce[A]

  @inline def toScalaVarArgsImpl[A](array: js.Array[A]): Seq[A] =
    WrappedVarArgs.wrap(array)

  @inline def genTraversableOnce2jsArrayImpl[A](
      col: CompatTraversableOnce[A]): js.Array[A] = {
    col match {
      case col: js.ArrayOps[A]     => col.result()
      case col: js.WrappedArray[A] => col.array
      case _ =>
        val result = new js.Array[A]
        col.iterator.foreach(x => result.push(x))
        result
    }
  }

}
