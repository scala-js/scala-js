package org.scalajs.testing.common

private[testing] final class RunnerArgs(
    val runID: RunMux.RunID,
    val frameworkImpl: String,
    val args: List[String],
    val remoteArgs: List[String])

private[testing] object RunnerArgs {
  implicit object RunnerArgsSerializer extends Serializer[RunnerArgs] {
    def serialize(x: RunnerArgs, out: Serializer.SerializeState): Unit = {
      out.write(x.runID)
      out.write(x.frameworkImpl)
      out.write(x.args)
      out.write(x.remoteArgs)
    }

    def deserialize(in: Serializer.DeserializeState): RunnerArgs = {
      new RunnerArgs(in.read[Int](), in.read[String](),
          in.read[List[String]](), in.read[List[String]]())
    }
  }
}
