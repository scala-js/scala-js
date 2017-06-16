package org.scalajs.build

import sbt.testing.TaskDef

/** Accessor for renderTestDefinitions to avoid exposing things. */
object HTMLRunnerBuilderAccess {
  def renderTestDefinitions(
      frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): String = {
    org.scalajs.testadapter.HTMLRunnerBuilder.renderTestDefinitions(
        frameworkImplClassNames, taskDefs)
  }
}
