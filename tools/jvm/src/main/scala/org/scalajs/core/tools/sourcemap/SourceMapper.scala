/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.sourcemap

import org.scalajs.core.tools.classpath._

import com.google.debugging.sourcemap._

class SourceMapper(classpath: CompleteClasspath) {

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

    /* **Attention**
     * - StackTrace positions are 1-based
     * - SourceMapConsumer.getMappingForLine() is 1-based
     */

    val lineNumber = ste.getLineNumber
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
          originalMapping.getLineNumber)
    } else ste
  }

  /** both `lineNumber` and the resulting column are 1-based */
  private def getFirstColumn(sourceMapConsumer: SourceMapConsumerV3,
      lineNumber1Based: Int) = {

    /* **Attention**
     * - SourceMapConsumerV3.EntryVisitor is 0-based!!!
     */

    val lineNumber = lineNumber1Based - 1

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

    val column0Based = column.getOrElse(0)
    column0Based + 1
  }

  private def findSourceMap(path: String) = {
    val candidates = classpath.allCode.filter(_.path == path)
    if (candidates.size != 1) None // better no sourcemap than a wrong one
    else candidates.head.sourceMap
  }
}
