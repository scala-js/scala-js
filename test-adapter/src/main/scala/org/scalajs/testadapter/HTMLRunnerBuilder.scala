/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import java.io.File
import java.net.URI

import sbt.testing.{Framework, TaskDef}

import org.scalajs.io._
import org.scalajs.io.JSUtils.escapeJS

import org.scalajs.jsenv.VirtualFileMaterializer

import org.scalajs.testcommon.Serializer

/** Template for the HTML runner. */
object HTMLRunnerBuilder {

  private val cssFile: MemVirtualTextFile = {
    val name = "test-runner.css"
    val inputStream = getClass.getResourceAsStream(name)
    val content = try {
      IO.readInputStreamToString(inputStream)
    } finally {
      inputStream.close()
    }
    new MemVirtualTextFile(name).withContent(content)
  }

  def writeToFile(output: File, title: String, jsFiles: Seq[VirtualJSFile],
      frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): Unit = {

    val jsFileCache = new VirtualFileMaterializer(true)
    val jsFileURIs = jsFiles.map {
      case file: FileVirtualFile => file.file.toURI
      case file                  => jsFileCache.materialize(file).toURI
    }
    val cssFileURI = jsFileCache.materialize(cssFile).toURI

    val htmlContent = render(output.toURI, title, jsFileURIs, cssFileURI,
        frameworkImplClassNames, taskDefs)

    val outputWriter = WritableFileVirtualTextFile(output).contentWriter
    try {
      outputWriter.write(htmlContent)
    } finally {
      outputWriter.close()
    }
  }

  private def render(baseURI: URI, title: String, jsFiles: Seq[URI],
      css: URI, frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): String = {
    def relURI(uri: URI) =
      htmlEscaped(URIUtils.relativize(baseURI, uri).toASCIIString)

    s"""
    <!DOCTYPE html>
    <html>
      <head>
        <title>${htmlEscaped(title)}</title>
        <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
        <link rel="stylesheet" type="text/css" href="${relURI(css)}" />
        ${(for (jsFile <- jsFiles) yield s"""
        <script type="text/javascript" src="${relURI(jsFile)}"></script>
        """).mkString("")}
        <script type="text/javascript">
        ${renderTestDefinitions(frameworkImplClassNames, taskDefs)}
        </script>
      </head>
      <body onload="org.scalajs.testinterface.HTMLRunner.main()" />
    </html>"""
  }

  /** Courtesy to our own build.
   *  This is a hack. The build should take care of its own mess, but oh well.
   */
  private[scalajs] def renderTestDefinitions(
      frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): String = {

    def mkVar[T: Serializer](name: String, value: T) =
      s"""var $name = "${escapeJS(Serializer.serialize(value))}";\n"""

    mkVar("definedTests", taskDefs) +
    mkVar("testFrameworkNames", frameworkImplClassNames)
  }

  private def htmlEscaped(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c :: Nil
  }
}
