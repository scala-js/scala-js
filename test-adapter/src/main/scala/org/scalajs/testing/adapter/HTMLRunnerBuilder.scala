/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testing.adapter

import scala.collection.JavaConverters._

import java.io.{File, InputStream}
import java.net.URI
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}

import sbt.testing.{Framework, TaskDef}

import org.scalajs.io._
import org.scalajs.io.JSUtils.escapeJS

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

  def writeToFile(output: File, title: String, jsFiles: Seq[VirtualBinaryFile],
      frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): Unit = {

    val jsFileURIs = jsFiles.map {
      case file: FileVirtualFile => file.file.toURI
      case file                  => tmpFile(file.path, file.inputStream)
    }

    val cssURI = {
      val name = "test-runner.css"
      tmpFile(name, getClass.getResourceAsStream(name))
    }

    val tests = new IsolatedTestSet(frameworkImplClassNames, taskDefs)

    val htmlContent = render(output.toURI, title, jsFileURIs, cssURI, tests)

    Files.write(output.toPath, List(htmlContent).asJava, StandardCharsets.UTF_8)
  }

  private def render(baseURI: URI, title: String, jsFiles: Seq[URI],
      css: URI, tests: IsolatedTestSet): String = {
    def relURI(uri: URI) =
      htmlEscaped(URIUtils.relativize(baseURI, uri).toASCIIString)

    s"""
    <!DOCTYPE html>
    <html>
      <head>
        <title>${htmlEscaped(title)}</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <link rel="stylesheet" type="text/css" href="${relURI(css)}" />
        <script type="text/javascript">
        ${injectInterfaceMode(tests)}
        </script>
        ${(for (jsFile <- jsFiles) yield s"""
        <script type="text/javascript" src="${relURI(jsFile)}"></script>
        """).mkString("")}
      </head>
      <body></body>
    </html>"""
  }

  private def injectInterfaceMode(tests: IsolatedTestSet): String = {
    val mode = TestInterfaceMode.HTMLRunner(tests)
    val ser = Serializer.serialize[TestInterfaceMode](mode)
    s"""var __ScalaJSTestInterfaceMode = "${escapeJS(ser)}";"""
  }

  private def htmlEscaped(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c :: Nil
  }
}
