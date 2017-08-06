package org.scalajs.testcommon

private[scalajs] final class RunnerArgs(
  val args: List[String], val remoteArgs: List[String])

private[scalajs] object RunnerArgs {
  implicit object RunnerArgsSerializer extends Serializer[RunnerArgs] {
    def serialize(x: RunnerArgs, out: Serializer.SerializeState): Unit = {
      out.write(x.args)
      out.write(x.remoteArgs)
    }

    def deserialize(in: Serializer.DeserializeState): RunnerArgs =
      new RunnerArgs(in.read[List[String]](), in.read[List[String]]())
  }
}
