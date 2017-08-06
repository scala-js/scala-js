package org.scalajs.testcommon

private[scalajs] final class ExecuteRequest(
    val taskInfo: TaskInfo, val loggerColorSupport: List[Boolean])

private[scalajs] object ExecuteRequest {
  implicit object ExecuteRequestSerializer extends Serializer[ExecuteRequest] {
    def serialize(x: ExecuteRequest, out: Serializer.SerializeState): Unit = {
      out.write(x.taskInfo)
      out.write(x.loggerColorSupport)
    }

    def deserialize(in: Serializer.DeserializeState): ExecuteRequest = {
      new ExecuteRequest(in.read[TaskInfo](), in.read[List[Boolean]]())
    }
  }
}
