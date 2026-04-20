package build

import java.nio.charset.StandardCharsets.UTF_8

import sbt.{IO, MessageOnlyException}

/** Applies formatted and indented patches in-place to source files. */
object SourceFilePatches {
  sealed abstract class Patch

  object Patch {
    final case class Lines(lines: Seq[String]) extends Patch
    final case class ArrayElements(elements: Seq[String]) extends Patch

    object ArrayElements {
      def ints(elements: Seq[Int]): ArrayElements =
        ArrayElements(elements.map(_.toString()))

      def hexInts(elements: Seq[Int]): ArrayElements =
        ArrayElements(elements.map("0x" + _.toHexString))
    }
  }

  private final val MaxLineLength = 80

  private val BeginGeneratedLineRE =
    raw""" *// BEGIN GENERATED: \[(.*)\]""".r

  def patchFile(fileName: String)(patches: (String, Patch)*): Unit = {
    val patchMap = patches.toMap
    val file = new java.io.File(fileName)
    val lines = IO.readLines(file, UTF_8)

    // Ensure that we provide exactly the patches that are expected in the file
    val expectedPatchNames = lines.collect {
      case BeginGeneratedLineRE(patchName) => patchName
    }.sorted
    val actualPatchNames = patches.map(_._1).toList.sorted
    if (actualPatchNames != expectedPatchNames) {
      throw new MessageOnlyException(
          s"Some patches were missing or unexpected in $fileName\n" +
          s"Expected: ${expectedPatchNames.mkString(", ")}\n" +
          s"Actual:   ${actualPatchNames.mkString(", ")}")
    }

    val newLines = patches.foldLeft(lines) { case (lines, (patchName, patch)) =>
      val beginLookup = s"// BEGIN GENERATED: [$patchName]"
      val endLookup = s"// END GENERATED: [$patchName]"
      val start = lines.indexWhere(_.endsWith(beginLookup))
      val end = lines.indexWhere(_.endsWith(endLookup))

      if (start < 0 || end < 0 || end < start)
        throw new MessageOnlyException(s"Cannot locate patch [$patchName] in $fileName")

      // preserve any blanks before END
      val beforeEnd = 1 + lines.lastIndexWhere(_.nonEmpty, end - 1)
      val (beginningAndOld, rest) = lines.splitAt(beforeEnd)
      val beginning = beginningAndOld.take(start + 1)

      val indent = rest(end - beforeEnd).takeWhile(_ == ' ')
      if (indent != beginning.last.takeWhile(_ == ' '))
        throw new MessageOnlyException(s"Inconsistent indent for patch [$patchName] in $fileName")

      val formattedPatch = formatPatch(patch, indent)

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
