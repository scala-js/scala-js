package scala.scalajs.sbtplugin.sourceMap

import org.mozilla.javascript.ScriptStackElement
import sbt.IO
import java.io.File
import com.google.debugging.sourcemap.SourceMapConsumerFactory
import com.google.debugging.sourcemap.SourceMapSection
import com.google.debugging.sourcemap.SourceMapConsumerV3
import com.google.debugging.sourcemap.FilePosition

object SourceMapper {
  def map(stack: Array[ScriptStackElement]) =
    for (el <- stack) yield {
      val sourceMapFile = new File(el.fileName + ".map")
      if (sourceMapFile.exists)
        mapScriptStackElement(el, sourceMapFile)
      else el
    }

  def mapScriptStackElement(el: ScriptStackElement, sourceMapFile: File): ScriptStackElement = {
    val sourceMapConsumer =
      SourceMapConsumerFactory
        .parse(IO.read(sourceMapFile))
        .asInstanceOf[SourceMapConsumerV3]

    val lineNumber = el.lineNumber
    val column = getFirstColumn(sourceMapConsumer, lineNumber)

    val originalMapping =
      sourceMapConsumer.getMappingForLine(lineNumber, column)

    new ScriptStackElement(
      originalMapping.getOriginalFile,
      el.functionName,
      originalMapping.getLineNumber)
  }

  def getFirstColumn(sourceMapConsumer: SourceMapConsumerV3, lineNumber: Int) = {

    var column: Option[Int] = None

    sourceMapConsumer.visitMappings(
      new SourceMapConsumerV3.EntryVisitor {

        def visit(sourceName: String,
          symbolName: String,
          sourceStartPosition: FilePosition,
          startPosition: FilePosition,
          endPosition: FilePosition): Unit =
          // we need to adjust line and column numbers because they are 0 based
          if (!column.isDefined && startPosition.getLine == lineNumber - 1)
            column = Some(startPosition.getColumn + 1)
      })

    column.getOrElse(1)
  }
}
