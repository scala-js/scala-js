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

package org.scalajs.testsuite.typedarray

import scala.scalajs.js.typedarray._
import scala.scalajs.js.JSConverters._

import scala.reflect._

import org.scalajs.testsuite.javalib
import org.scalajs.testsuite.utils.Requires

object ArraysTest extends Requires.TypedArray

class ArraysTest extends javalib.util.ArraysTest {

  override def Array[T: ClassTag](v: T*): scala.Array[T] = classTag[T] match {
    case ClassTag.Byte =>
      new Int8Array(v.asInstanceOf[Seq[Byte]].toJSArray).toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Short =>
      new Int16Array(v.asInstanceOf[Seq[Short]].toJSArray).toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Int =>
      new Int32Array(v.asInstanceOf[Seq[Int]].toJSArray).toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Float =>
      new Float32Array(v.asInstanceOf[Seq[Float]].toJSArray).toArray.asInstanceOf[scala.Array[T]]
    case ClassTag.Double =>
      new Float64Array(v.asInstanceOf[Seq[Double]].toJSArray).toArray.asInstanceOf[scala.Array[T]]
    case _ => scala.Array(v: _*)
  }
}
