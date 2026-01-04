package build

import java.nio.charset.StandardCharsets.UTF_8

import sbt.{IO, MessageOnlyException}

object SourceFilePatches {
  sealed abstract class Patch

  object Patch {
    final case class Lines(lines: Seq[String]) extends Patch
    final case class ArrayElements(elements: Seq[Any]) extends Patch
  }

  private final val MaxLineLength = 80

  def patchFile(fileName: String)(patches: (String, Patch)*): Unit = {
    val patchMap = patches.toMap
    val file = new java.io.File(fileName)
    val lines = IO.readLines(file, UTF_8)

    val newLines = patches.foldLeft(lines) { (lines, patch) =>
      val beginLookup = s"// BEGIN GENERATED: [${patch._1}]"
      val endLookup = s"// END GENERATED: [${patch._1}]"
      val start = lines.indexWhere(_.endsWith(beginLookup))
      val end = lines.indexWhere(_.endsWith(endLookup))

      if (start < 0 || end < 0 || end < start)
        throw new MessageOnlyException(s"Cannot locate patch [${patch._1}] in $fileName")

      val (beginningAndOld, rest) = lines.splitAt(end)
      val beginning = beginningAndOld.take(start + 1)

      val indent = rest.head.takeWhile(_ == ' ')
      val formattedPatch = formatPatch(patch._2, indent)

      beginning ::: formattedPatch ::: rest
    }

    // Do not use IO.writeLines; it uses CRLF on Windows
    IO.write(file, newLines.mkString("", "\n", "\n").getBytes(UTF_8))
  }

  private def formatPatch(patch: Patch, indent: String): List[String] = {
    patch match {
      case Patch.Lines(lines)            => formatLines(lines, indent)
      case Patch.ArrayElements(elements) => formatArrayElements(elements, indent)
    }
  }

  private def formatLines(lines: Seq[String], indent: String): List[String] =
    lines.toList.map(indent + _)

  private def formatArrayElements(elements: Seq[Any], indent: String): List[String] = {
    val lastIndex = elements.size - 1
    val lines = List.newBuilder[String]
    val lineBuilder = new java.lang.StringBuilder()

    for ((element, i) <- elements.iterator.zipWithIndex) {
      val toAdd = "" + element + (if (i == lastIndex) "" else ",")
      if (lineBuilder.length() + 1 + toAdd.length() >= MaxLineLength) {
        lines += lineBuilder.toString()
        lineBuilder.setLength(0)
      }

      if (lineBuilder.length() == 0)
        lineBuilder.append(indent)
      else
        lineBuilder.append(' ')

      lineBuilder.append(toAdd)
    }

    if (lineBuilder.length() != 0)
      lines += lineBuilder.toString()

    lines.result()
  }
}
