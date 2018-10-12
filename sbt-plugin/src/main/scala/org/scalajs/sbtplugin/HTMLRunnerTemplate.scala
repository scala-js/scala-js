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

package org.scalajs.sbtplugin

import java.net.URI

import sbt.testing.{Framework, TaskDef}

import org.scalajs.core.ir.Utils
import org.scalajs.core.tools.json._
import org.scalajs.testcommon.Serializer

/** Template for the HTML runner. */
private[scalajs] object HTMLRunnerTemplate {

  def render(baseURI: URI, title: String, sjsFile: URI, jsdepsFile: URI,
      css: URI, loadedFrameworks: Map[sbt.TestFramework, Framework],
      definedTests: Seq[sbt.TestDefinition],
      sysProps: Map[String, String]): String = {
    def relURI(uri: URI) =
      htmlEscaped(Utils.relativize(baseURI, uri).toASCIIString)

    s"""
    <!DOCTYPE html>
    <html>
      <head>
        <title>${htmlEscaped(title)}</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <link rel="stylesheet" type="text/css" href="${relURI(css)}" />
        <script type="text/javascript">
          var __ScalaJSEnv = {
            javaSystemProperties: ${jsonToString(sysProps.toJSON)}
          };
        </script>
        <script type="text/javascript" src="${relURI(jsdepsFile)}"></script>
        <script type="text/javascript" src="${relURI(sjsFile)}"></script>
        <script type="text/javascript">
        ${renderTestDefinitions(loadedFrameworks, definedTests)}
        </script>
      </head>
      <body onload="org.scalajs.testinterface.HTMLRunner.main()" />
    </html>"""
  }

  def renderTestDefinitions(
      loadedFrameworks: Map[sbt.TestFramework, Framework],
      definedTests: Seq[sbt.TestDefinition]): String = {
    val frameworks = loadedFrameworks.map(_._1.implClassNames.toList).toList

    val tests = definedTests.map { t =>
      new TaskDef(t.name, t.fingerprint, t.explicitlySpecified, t.selectors)
    }.toList

    val testsString = Serializer.serialize(tests)

    s"""
      var definedTests = ${jsonToString(testsString.toJSON)};
      var testFrameworkNames = ${jsonToString(frameworks.toJSON)};
    """
  }

  private def htmlEscaped(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c :: Nil
  }
}
