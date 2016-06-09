package org.scalajs.build

import sbt.testing.Framework

/** Accessor for renderTestDefinitions to avoid exposing things. */
object HTMLRunnerTemplateAccess {
  def renderTestDefinitions(loadedFrameworks: Map[sbt.TestFramework, Framework],
      definedTests: Seq[sbt.TestDefinition]): String = {
    org.scalajs.sbtplugin.HTMLRunnerTemplate.renderTestDefinitions(
        loadedFrameworks, definedTests)
  }
}
