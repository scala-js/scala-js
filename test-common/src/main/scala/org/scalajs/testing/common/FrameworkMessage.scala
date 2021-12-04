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

package org.scalajs.testing.common

private[testing] final class FrameworkMessage(val workerId: Long, val msg: String)

private[testing] object FrameworkMessage {
  implicit object FrameworkMessageSerializer extends Serializer[FrameworkMessage] {
    def serialize(x: FrameworkMessage, out: Serializer.SerializeState): Unit = {
      out.write(x.workerId)
      out.write(x.msg)
    }

    def deserialize(in: Serializer.DeserializeState): FrameworkMessage = {
      new FrameworkMessage(in.read[Long](), in.read[String]())
    }
  }
}
