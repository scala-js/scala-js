/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.scalajs.runtime

import scala.collection.GenTraversableOnce
import scala.scalajs.js

private[runtime] object Compat {

  @inline def toScalaVarArgsImpl[A](array: js.Array[A]): Seq[A] =
    new js.WrappedArray(array)

  def toJSVarArgsImpl[A](seq: Seq[A]): js.Array[A] = {
    seq match {
      case seq: js.WrappedArray[A] =>
        seq.array
      case _ =>
        val result = new js.Array[A]
        seq.foreach(x => result.push(x))
        result
    }
  }

}
