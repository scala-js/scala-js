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

import java.io.{File, IOException}
import java.nio.CharBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes

import sbt.testing.{Framework, TaskDef}

import org.scalajs.jsenv.{Input, UnsupportedInputException}
import org.scalajs.jsenv.JSUtils.escapeJS

import org.scalajs.testing.common._

/** Template for the HTML runner. */
object HTMLRunnerBuilder {
  @deprecated("Use write instead", "1.2.0")
  def writeToFile(output: File, title: String, input: Seq[Input],
      frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): Unit = {
    val outputPath = output.toPath()
    val artifactsDir =
      Files.createTempDirectory(outputPath.getParent(), ".html-artifacts")

    sys.addShutdownHook {
      Files.walkFileTree(artifactsDir, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }

    write(outputPath, artifactsDir, title, input, frameworkImplClassNames, taskDefs)
  }

  def write(output: Path, artifactsDir: Path, title: String, input: Seq[Input],
      frameworkImplClassNames: List[List[String]],
      taskDefs: List[TaskDef]): Unit = {

    def artifactPath(name: String): (String, Path) = {
      val path = artifactsDir.resolve(name)
      val relPath = output.getParent().relativize(path)
      (joinRelPath(relPath), path)
    }

    def scriptTag(index: Int, tpe: String, content: Path) = {
      val (src, target) = artifactPath(f"input$index-${content.getFileName()}")
      Files.copy(content, target, StandardCopyOption.REPLACE_EXISTING)
      s"""<script defer type="$tpe" src="${htmlEscaped(src)}"></script>"""
    }

    val loadJSTags = input.zipWithIndex.map {
      case (Input.Script(script), i)   => scriptTag(i, "text/javascript", script)
      case (Input.ESModule(module), i) => scriptTag(i, "module", module)

      case _ =>
        throw new UnsupportedInputException(
            s"Unsupported input for the generation of an HTML runner: $input")
    }

    val bridgeModeStr = {
      val tests = new IsolatedTestSet(frameworkImplClassNames, taskDefs)
      val mode = TestBridgeMode.HTMLRunner(tests)
      Serializer.serialize[TestBridgeMode](mode)
    }

    val cssHref = {
      val name = "test-runner.css"
      val (href, target) = artifactPath(name)
      val in = getClass.getResourceAsStream(name)
      try Files.copy(in, target, StandardCopyOption.REPLACE_EXISTING)
      finally in.close()
      href
    }

    val htmlContent = s"""
      <!DOCTYPE html>
      <html>
        <head>
          <title>${htmlEscaped(title)}</title>
          <meta http-equiv="Content-Type" content="text/html; charset=UTF-8"/>
          <link rel="stylesheet" type="text/css" href="${htmlEscaped(cssHref)}" />
          <script type="text/javascript">
            var __ScalaJSTestBridgeMode = "${escapeJS(bridgeModeStr)}";
          </script>
          ${loadJSTags.mkString("\n")}
        </head>
        <body></body>
      </html>
    """

    Files.write(output, java.util.Arrays.asList(htmlContent), UTF_8)
  }

  private def htmlEscaped(str: String): String = str.flatMap {
    case '<' => "&lt;"
    case '>' => "&gt;"
    case '"' => "&quot;"
    case '&' => "&amp;"
    case c   => c.toString()
  }

  // <parts>.map(_.toString()).mkString("/")
  private def joinRelPath(p: Path): String = {
    require(p.getRoot() == null)

    val partsIter = p.iterator()
    val result = new StringBuilder()
    while (partsIter.hasNext()) {
      result.append(partsIter.next())
      if (partsIter.hasNext())
        result.append('/')
    }

    result.toString()
  }
}
