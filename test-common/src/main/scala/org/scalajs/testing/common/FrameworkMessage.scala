package org.scalajs.testing.common

private[testing] final class FrameworkMessage(val slaveId: Long, val msg: String)

private[testing] object FrameworkMessage {
  implicit object FrameworkMessageSerializer extends Serializer[FrameworkMessage] {
    def serialize(x: FrameworkMessage, out: Serializer.SerializeState): Unit = {
      out.write(x.slaveId)
      out.write(x.msg)
    }

    def deserialize(in: Serializer.DeserializeState): FrameworkMessage = {
      new FrameworkMessage(in.read[Long](), in.read[String]())
    }
  }
}
