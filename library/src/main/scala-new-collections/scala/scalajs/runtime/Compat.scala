package scala.scalajs.runtime

import scala.collection.IterableOnce

import scala.scalajs.js

private[runtime] object Compat {

  @inline def toScalaVarArgsImpl[A](array: js.Array[A]): Seq[A] =
    WrappedVarArgs.wrap(array)

  def toJSVarArgsImpl[A](seq: Seq[A]): js.Array[A] = {
    seq match {
      case seq: WrappedVarArgs[A] =>
        seq.unwrap.asInstanceOf[js.Array[A]]
      case _ =>
        val result = new js.Array[A]
        seq.foreach(x => result.push(x))
        result
    }
  }

}
