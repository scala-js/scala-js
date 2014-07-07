/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.test
package typedarray

import scala.scalajs.js.Any.fromTraversableOnce
import scala.scalajs.js.typedarray._

import scala.reflect._

object ArraysTest extends javalib.ArraysTest {

  override def Array[T : ClassTag](v: T*): scala.Array[T] = classTag[T] match {
    case ClassTag.Byte =>
      new Int8Array(fromTraversableOnce(v.asInstanceOf[Seq[Byte]]))
      .toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Short =>
      new Int16Array(fromTraversableOnce(v.asInstanceOf[Seq[Short]]))
      .toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Int =>
      new Int32Array(fromTraversableOnce(v.asInstanceOf[Seq[Int]]))
      .toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Float =>
      new Float32Array(fromTraversableOnce(v.asInstanceOf[Seq[Float]]))
      .toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Double =>
      new Float64Array(fromTraversableOnce(v.asInstanceOf[Seq[Double]]))
      .toArray.asInstanceOf[scala.Array[T]]
    case _ => scala.Array(v: _*)
  }

  override def testBody(suite: => Unit) = {
    when("typedarray").
    describe("java.util.Arrays backed with TypedArrays")(suite)
  }

}
