/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin

import java.io.File
import java.io.Writer

import sbt.testing.{Framework, TaskDef}

import org.scalajs.core.tools.json._
import org.scalajs.testadapter.TaskDefSerializers._

/** Template for the HTML runner. */
private[scalajs] object HTMLRunnerTemplate {

  def render(title: String, sjsFile: File, jsdepsFile: File, css: File,
      loadedFrameworks: Map[sbt.TestFramework, Framework],
      definedTests: Seq[sbt.TestDefinition],
      sysProps: Map[String, String]): String = {
    s"""
    <!DOCTYPE html>
    <html>
      <head>
        <title>${htmlEscaped(title)}</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <link rel="stylesheet" type="text/css"
              href="${htmlEscaped(css.toURI.toASCIIString)}" />
        <script type="text/javascript">
          var __ScalaJSEnv = {
            javaSystemProperties: ${jsonToString(sysProps.toJSON)}
          };
        </script>
        <script type="text/javascript"
                src="${htmlEscaped(jsdepsFile.toURI.toASCIIString)}"></script>
        <script type="text/javascript"
                src="${htmlEscaped(sjsFile.toURI.toASCIIString)}"></script>
        <script type="text/javascript">
        ${renderTestDefinitions(loadedFrameworks, definedTests)}
        </script>
      </head>
      <body onload="org.scalajs.testinterface.HTMLRunner().main()" />
    </html>"""
  }

  def renderTestDefinitions(
      loadedFrameworks: Map[sbt.TestFramework, Framework],
      definedTests: Seq[sbt.TestDefinition]): String = {
    val frameworks = loadedFrameworks.map(_._1.implClassNames.toList).toList

    val tests = definedTests.map { t =>
      new TaskDef(t.name, t.fingerprint, t.explicitlySpecified, t.selectors)
    }.toList

    s"""
      var definedTests = ${jsonToString(tests.toJSON)};
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
