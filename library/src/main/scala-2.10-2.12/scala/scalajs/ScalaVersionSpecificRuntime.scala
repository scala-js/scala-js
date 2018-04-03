package scala.scalajs

import scala.collection.GenTraversableOnce

trait ScalaVersionSpecificRuntime {

  @inline final def genTraversableOnce2jsArray[A](
    col: GenTraversableOnce[A]): js.Array[A] = {
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
