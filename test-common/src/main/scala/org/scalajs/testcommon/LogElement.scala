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

package org.scalajs.testcommon

private[scalajs] final class LogElement[T](val index: Int, val x: T)

private[scalajs] object LogElement {
  implicit def logElementSerializer[T: Serializer]: Serializer[LogElement[T]] = {
    new Serializer[LogElement[T]] {
      def serialize(x: LogElement[T], out: Serializer.SerializeState): Unit = {
        out.write(x.index)
        out.write(x.x)
      }

      def deserialize(in: Serializer.DeserializeState): LogElement[T] = {
        new LogElement(in.read[Int](), in.read[T]())
      }
    }
  }
}
