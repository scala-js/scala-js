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

package org.scalajs.testing.adapter

import java.io.{File, InputStream}
import java.net.URI
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, StandardCopyOption}

import sbt.testing.{Framework, TaskDef}

import org.scalajs.jsenv.{Input, UnsupportedInputException}
import org.scalajs.jsenv.JSUtils.escapeJS

import org.scalajs.testing.common._

/** Template for the HTML runner. */
object HTMLRunnerBuilder {

  private val tmpSuffixRE = """[a-zA-Z0-9-_.]*$""".r

  private def tmpFile(path: String, in: InputStream): URI = {
    try {
      /* - createTempFile requires a prefix of at least 3 chars
       * - we use a safe part of the path as suffix so the extension stays (some
       *   browsers need that) and there is a clue which file it came from.
       */
      val suffix = tmpSuffixRE.findFirstIn(path).orNull

      val f = File.createTempFile("tmp-", suffix)
      f.deleteOnExit()
      Files.copy(in, f.toPath(), StandardCopyOption.REPLACE_EXISTING)
      f.toURI()
    } finally {
      in.close()
    }
  }

  def writeToFile(output: File, title: String, input: Seq[Input],
      frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): Unit = {

    val jsFiles = input.map {
      case Input.Script(script) => script

      case _ =>
        throw new UnsupportedInputException(
            s"Unsupported input for the generation of an HTML runner: $input")
    }

    val jsFileURIs = jsFiles.map { f =>
      try {
        f.toFile.toURI
      } catch {
        case _: UnsupportedOperationException =>
          tmpFile(f.toString, Files.newInputStream(f))
      }
    }

    val cssURI = {
      val name = "test-runner.css"
      tmpFile(name, getClass.getResourceAsStream(name))
    }

    val tests = new IsolatedTestSet(frameworkImplClassNames, taskDefs)

    val htmlContent = render(title, jsFileURIs, cssURI, tests)

    Files.write(output.toPath, java.util.Arrays.asList(htmlContent), UTF_8)
  }

  private def render(title: String, jsFiles: Seq[URI],
      css: URI, tests: IsolatedTestSet): String = {
    def uristr(uri: URI) = htmlEscaped(uri.toASCIIString)

    s"""
    <!DOCTYPE html>
    <html>
      <head>
        <title>${htmlEscaped(title)}</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <link rel="stylesheet" type="text/css" href="${uristr(css)}" />
        <script type="text/javascript">
        ${injectBridgeMode(tests)}
        </script>
        ${(for (jsFile <- jsFiles) yield s"""
        <script type="text/javascript" src="${uristr(jsFile)}"></script>
        """).mkString("")}
      </head>
      <body></body>
    </html>"""
  }

  private def injectBridgeMode(tests: IsolatedTestSet): String = {
    val mode = TestBridgeMode.HTMLRunner(tests)
    val ser = Serializer.serialize[TestBridgeMode](mode)
    s"""var __ScalaJSTestBridgeMode = "${escapeJS(ser)}";"""
  }

  private def htmlEscaped(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c.toString()
  }
}
