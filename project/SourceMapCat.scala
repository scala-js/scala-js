import sbt._

import java.io.PrintWriter

import com.google.debugging.sourcemap.{ FilePosition, _ }

object SourceMapCat {
  /** Concatenate JS files and their respective source maps
   *  In this implementation, source maps are assumed to be named after their
   *  JS file, with an additional .map extension (hence it likely ends in
   *  .js.map).
   */
  def catJSFilesAndTheirSourceMaps(inputs: Seq[File], output: File) {
    val outcode = new PrintWriter(output)
    val outMapFile = sourceMapOf(output)
    val outmap = new PrintWriter(outMapFile)
    val outmapGen = SourceMapGeneratorFactory.getInstance(SourceMapFormat.V3)

    var totalLineCount = 0

    for (input <- inputs) {
      val offset = totalLineCount

      val lines = IO.readLines(input)
      val lineCount = lines.size

      // Cat the file - remove references to source maps
      for (line <- lines) {
        if (line startsWith "//@ sourceMappingURL=") outcode.println()
        else outcode.println(line)
      }

      // Cat the source map
      val sourceMapFile = sourceMapOf(input)
      if (sourceMapFile.exists) {
        /* The source map exists.
         * Visit all the mappings in this source map, and add them to the
         * concatenated source map with the appropriate offset.
         */
        val consumer = new SourceMapConsumerV3
        consumer.parse(IO.read(sourceMapFile))

        consumer.visitMappings(new SourceMapConsumerV3.EntryVisitor {
          override def visit(sourceName: String, symbolName: String,
              sourceStartPos: FilePosition,
              startPos: FilePosition, endPos: FilePosition) {

            val offsetStartPos =
              new FilePosition(startPos.getLine+offset, startPos.getColumn)
            val offsetEndPos =
              new FilePosition(endPos.getLine+offset, endPos.getColumn)

            outmapGen.addMapping(sourceName, symbolName,
                sourceStartPos, offsetStartPos, offsetEndPos)
          }
        })
      } else {
        /* The source map does not exist.
         * This happens typically for corejslib.js and other helper files
         * written directly in JS.
         * We generate a fake line-by-line source map for these on the fly
         */
        val sourceName = input.getPath
        for (lineNumber <- 0 until lineCount) {
          val sourceStartPos = new FilePosition(lineNumber, 0)
          val startPos = new FilePosition(offset+lineNumber, 0)
          val endPos = new FilePosition(offset+lineNumber+1, 0)

          outmapGen.addMapping(sourceName, null,
              sourceStartPos, startPos, endPos)
        }
      }

      totalLineCount += lineCount
    }

    outcode.println("//@ sourceMappingURL=" + outMapFile.getName)

    outmapGen.appendTo(outmap, output.getName)

    outcode.close()
    outmap.close()
  }

  private def sourceMapOf(jsfile: File): File =
    jsfile.getParentFile / (jsfile.getName+".map")
}
