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

private[scalajs] final class RunnerArgs(
    val runID: RunMux.RunID,
    val frameworkImpl: String,
    val args: List[String],
    val remoteArgs: List[String])

private[scalajs] object RunnerArgs {
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
