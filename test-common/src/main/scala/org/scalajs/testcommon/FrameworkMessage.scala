package org.scalajs.testcommon

private[scalajs] final class FrameworkMessage(val slaveId: Long, val msg: String)

private[scalajs] object FrameworkMessage {
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
