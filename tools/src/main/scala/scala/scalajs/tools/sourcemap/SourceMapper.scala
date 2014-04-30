/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.sourcemap

import scala.scalajs.tools.classpath.JSClasspath

import com.google.debugging.sourcemap._

class SourceMapper(classpath: JSClasspath) {

  def map(ste: StackTraceElement, columnNumber: Int): StackTraceElement = {
    val mapped = for {
      sourceMap <- findSourceMap(ste.getFileName)
    } yield map(ste, columnNumber, sourceMap)

    mapped.getOrElse(ste)
  }

  def map(ste: StackTraceElement, columnNumber: Int,
      sourceMap: String): StackTraceElement = {

    val sourceMapConsumer =
      SourceMapConsumerFactory
        .parse(sourceMap)
        .asInstanceOf[SourceMapConsumerV3]


    // StackTrace positions are 1-based. We need to adjust line and
    // column numbers because source map positions are 0-based
    val lineNumber = ste.getLineNumber - 1
    val column =
      if (columnNumber == -1) getFirstColumn(sourceMapConsumer, lineNumber)
      else columnNumber

    val originalMapping =
      sourceMapConsumer.getMappingForLine(lineNumber, column)

    if (originalMapping != null) {
      new StackTraceElement(
          ste.getClassName,
          ste.getMethodName,
          originalMapping.getOriginalFile,
          originalMapping.getLineNumber + 1)
    } else ste
  }

  /** both `lineNumber` and the resulting column are 0-based */
  private def getFirstColumn(sourceMapConsumer: SourceMapConsumerV3,
      lineNumber: Int) = {

    var column: Option[Int] = None

    sourceMapConsumer.visitMappings(
      new SourceMapConsumerV3.EntryVisitor {
        def visit(sourceName: String,
          symbolName: String,
          sourceStartPosition: FilePosition,
          startPosition: FilePosition,
          endPosition: FilePosition): Unit =
          if (!column.isDefined && startPosition.getLine == lineNumber)
            column = Some(startPosition.getColumn)
      })

    column.getOrElse(0)
  }

  private def findSourceMap(path: String) = {
    val candidates = classpath.jsFiles.filter(_.path == path)
    if (candidates.size != 1) None // better no sourcemap than a wrong one
    else candidates.head.sourceMap
  }
}
