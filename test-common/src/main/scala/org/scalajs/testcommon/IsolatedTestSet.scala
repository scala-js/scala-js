package org.scalajs.testcommon

import sbt.testing.TaskDef

/** A set of tests for execution.
 *
 *  This is used by secondary invocation mechanisms (e.g. HTML runner) that do
 *  not perform test execution "interactively". Instead a full set of frameworks
 *  and tests is detected upfront (typically by the sbt plugin) and then shipped
 *  for execution.
 */
private[scalajs] final class IsolatedTestSet(
    val testFrameworkNames: List[List[String]],
    val definedTests: List[TaskDef]
)

private[scalajs] object IsolatedTestSet {
  implicit object IsolatedTestSetSerializer extends Serializer[IsolatedTestSet] {
    def serialize(x: IsolatedTestSet, out: Serializer.SerializeState): Unit = {
      out.write(x.testFrameworkNames)
      out.write(x.definedTests)
    }

    def deserialize(in: Serializer.DeserializeState): IsolatedTestSet = {
      new IsolatedTestSet(in.read[List[List[String]]](), in.read[List[TaskDef]]())
    }
  }
}
