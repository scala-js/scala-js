package build

import java.lang.Character._

import scala.collection.mutable

import sbt.MessageOnlyException

import SourceFilePatches._

/** Generator for all the Unicode data tables and tests that we derived from
 *  the JDK.
 *
 *  Run it with the sbt task `javalibInternal/regerateUnicodeData`. That task
 *  updates source files in-place!
 */
object UnicodeDataGen {
  /** When updating this to a newer version, you should then regenerate the
   *  Unicode data so they stay in sync. The CI ensures that you do so. You
   *  will have to update the relevant `java.version` used in the
   *  `os-sensitive-ci.yml` CI workflow.
   *
   *  Trying to generate the Unicode data while the build is running on a
   *  different JDK version will result in an error.
   */
  final val ReferenceJDKVersion = 21

  def generateAll(detectedJDKVersion: Int): Unit = {
    if (detectedJDKVersion != ReferenceJDKVersion) {
      throw new MessageOnlyException(
        s"The reference JDK version to generate Unicode data is " +
        s"$ReferenceJDKVersion, but the build is running under " +
        s"$detectedJDKVersion. " +
        s"Make sure to run the build itself under JDK $ReferenceJDKVersion " +
        s"to generate Unicode data."
      )
    }

    generateCharacter()
    generateCharacterTest()
  }

  private final val FirstNonASCII = 0x0080

  /** First invalid code point.
   *
   *  When computing values by range, we go all the way up to the first
   *  invalid code point. Most of the methods of jl.Character have a
   *  well-defined result for invalid code points, i.e., negative or greater
   *  than MAX_CODE_POINT. We include `MAX_CODE_POINT + 1` on purpose in our
   *  ranges, so that there will always be a valid value for the invalid
   *  code points.
   */
  private final val FirstInvalidCP = MAX_CODE_POINT + 1

  // Helpers

  private def constantDef(name: String, value: Int): String =
    s"private final val $name = $value"

  private def cpToStr(cp: Int): String =
    String.valueOf(Character.toChars(cp)) // Character.toString(cp) does not compile on JDK 8

  private def formatCP(cp: Int): String =
    f"0x$cp%04x"

  // --- jl.Character ---

  private def generateCharacter(): Unit = {
    val titleCaseMappings = computeTitleCaseMappings()

    val unicodeBlocks = computeUnicodeBlocks()
    val unicodeBlockConstants = List(constantDef("BlockCount", unicodeBlocks.size))

    val charTypesFirst256 = (0 until 256).map(getType(_))
    val (charTypeIndicesDeltas, charTypes) = computeCharTypes()

    val nonASCIIZeroDigitCodePoints = computeZeroDigitCodePoints(FirstNonASCII)
    val mirroredIndices = computeMirroredIndices()
    val combiningClasses = computeCombiningClasses()

    patchFile("javalib/src/main/scala/java/lang/Character.scala")(
      "titlecase-mappings" -> Patch.Lines(titleCaseMappings),
      "unicode-block-constants" -> Patch.Lines(unicodeBlockConstants),
      "unicode-blocks" -> Patch.Lines(unicodeBlocks),
      "char-types-first-256" -> Patch.ArrayElements.ints(charTypesFirst256),
      "char-types-indices" -> Patch.ArrayElements.ints(charTypeIndicesDeltas),
      "char-types" -> Patch.ArrayElements.ints(charTypes),
      "non-ascii-zero-digits" -> Patch.ArrayElements(nonASCIIZeroDigitCodePoints.map(formatCP(_))),
      "mirrored-indices" -> Patch.ArrayElements.ints(mirroredIndices),
      "combining-classes" -> Patch.ArrayElements.ints(combiningClasses),
    )
  }

  private def computeTitleCaseMappings(): Array[String] = {
    val b = Array.newBuilder[String]

    for (cp <- 0 to MAX_CODE_POINT) {
      val titleCaseCP = toTitleCase(cp)
      val upperCaseCP = toUpperCase(cp)

      if (titleCaseCP != upperCaseCP)
        b += f"case 0x$cp%04x => 0x$titleCaseCP%04x"
    }

    b.result()
  }

  private def computeUnicodeBlocks(): Array[String] = {
    // JVMName -> (historicalName, properName)
    val historicalMap = Map(
      "GREEK" -> ("Greek", "Greek and Coptic"),
      "CYRILLIC_SUPPLEMENTARY" -> ("Cyrillic Supplementary", "Cyrillic Supplement"),
      "COMBINING_MARKS_FOR_SYMBOLS" -> ("Combining Marks For Symbols", "Combining Diacritical Marks for Symbols")
    )

    // Get the "proper name" for JVM block name
    val blockNameMap: Map[String, String] = {
      // We can use the "latest" source, because we only use it to generate mappings
      val blocksSourceURL = new java.net.URI("https://unicode.org/Public/UCD/latest/ucd/Blocks.txt").toURL()
      val source = scala.io.Source.fromURL(blocksSourceURL, "UTF-8")
      try {
        source
          .getLines()
          .filterNot {
            _.startsWith("#")
          }
          .flatMap { line =>
            line.split(';') match {
              case Array(_, name) =>
                val trimmed = name.trim
                val jvmName = trimmed.replaceAll(raw"[\s\-]", "_").toUpperCase
                Some(jvmName -> trimmed)
              case _ => None
            }
          }.toMap
      } finally {
        source.close()
      }
    }

    val blocksAndCharacters = (0 to MAX_CODE_POINT)
      .map(cp => UnicodeBlock.of(cp) -> cp).filterNot(_._1 == null)

    val orderedBlocks = blocksAndCharacters.map(_._1).distinct.toArray

    val blockLowAndHighCodePointsMap = {
      blocksAndCharacters.groupBy(_._1).mapValues { v =>
        val codePoints = v.map(_._2)
        (codePoints.min, codePoints.max)
      }
    }

    orderedBlocks.map { b =>
      val minCodePoint = "0x%04x".format(blockLowAndHighCodePointsMap(b)._1)
      val maxCodePoint = "0x%04x".format(blockLowAndHighCodePointsMap(b)._2)

      historicalMap.get(b.toString) match {
        case Some((historicalName, properName)) =>
          s"""val $b = addUnicodeBlock("$properName", "$historicalName", $minCodePoint, $maxCodePoint)"""
        case None =>
          val properBlockName = blockNameMap.getOrElse(b.toString, {
            throw new IllegalArgumentException("$b")
          })
          val jvmBlockName = properBlockName.toUpperCase.replaceAll("[\\s\\-_]", "_")
          assert(jvmBlockName == b.toString)
          s"""val $jvmBlockName = addUnicodeBlock("$properBlockName", $minCodePoint, $maxCodePoint)"""
      }
    }
  }

  private def computeCharTypes(): (Seq[Int], Seq[Int]) = {
    val indicesAndTypes = (256 to FirstInvalidCP)
      .map(i => (i, Character.getType(i)))
      .foldLeft[List[(Int, Int)]](Nil) {
        case (x :: xs, elem) if x._2 == elem._2 => x :: xs
        case (prevs, elem) => elem :: prevs
      }.reverse
    val charTypeIndices = indicesAndTypes.map(_._1).tail
    val charTypes = indicesAndTypes.map(_._2)

    (charTypeIndices, charTypes)
  }

  private def computeZeroDigitCodePoints(start: Int): Array[Int] = {
    checkDecimalDigitAssumptions()

    val b = Array.newBuilder[Int]
    for (cp <- start to MAX_CODE_POINT) {
      if (Character.digit(cp, 10) == 0)
        b += cp
    }

    b.result()
  }

  private def checkDecimalDigitAssumptions(): Unit = {
    for (cp <- 0 to MAX_CODE_POINT) {
      val d = Character.digit(cp, 10)
      if (d == 0) {
        // Every 0 digit is followed by digits from 1 to 9
        for (i <- 1 to 9) {
          val d2 = Character.digit(cp + i, 10)
          if (d2 != i) {
            throw new MessageOnlyException(
                s"Assumption broken: code point ${(cp + i).toHexString} " +
                s"should have digit $i but was $d2. " +
                s"It follows zero digit code point ${cp.toHexString}.")
          }
        }
      } else if (d >= 1 && d <= 9) {
        // Every 1-9 digit must come after a 0 digit at the appropriate distance
        val d2 = Character.digit(cp - d, 10)
        if (d2 != 0) {
          throw new MessageOnlyException(
              s"Assumption broken: code point ${cp.toHexString} with digit $d " +
              s"does not follow a zero digit code point. " +
              s"It should have been ${(cp - d).toHexString}} but its digit was $d2.")
        }
      }
    }
  }

  private def computeMirroredIndices(): Array[Int] = {
    val b = Array.newBuilder[Int]

    var lastPropValue = false

    for (cp <- 0 to FirstInvalidCP) {
      val propValue = isMirrored(cp)
      if (propValue != lastPropValue) {
        b += cp
        lastPropValue = propValue
      }
    }

    b.result()
  }

  private def computeCombiningClasses(): Array[Int] = {
    val b = Array.newBuilder[Int]

    /* The initial value is always 0 by construction, irrespective of the
     * constants we define. That's why we don't use CombiningClassIsNone here.
     */
    var currentCombiningClass = 0

    // We don't use FirstInvalidCP here because there is no valid value for invalid code points
    for (cp <- 0 to MAX_CODE_POINT) {
      val combiningClass = computeCombiningClass(cp)
      while (combiningClass != currentCombiningClass) {
        b += cp
        currentCombiningClass = (currentCombiningClass + 1) % 3
      }
    }

    b.result()
  }

  private val Lithuanian = java.util.Locale.forLanguageTag("lt")

  // Copied from jl.Character
  private final val CombiningClassIsNone = 0
  private final val CombiningClassIsAbove = 1
  private final val CombiningClassIsOther = 2

  /** Computes the combining class of a code point by testing how it behaves
   *  through Lithuanian's toLowerCase mapping.
   *
   *  See the comment in jl.String.toLowerCaseLithuanian.
   */
  private def computeCombiningClass(cp: Int): Int = {
    if (!isValidCodePoint(cp)) {
      CombiningClassIsNone
    } else {
      val cpStr = cpToStr(cp)
      if (("I" + cpStr).toLowerCase(Lithuanian).startsWith("i\u0307")) {
        // includes 0x0307 itself
        CombiningClassIsAbove
      } else if (("I" + cpStr + "\u0307").toLowerCase(Lithuanian).startsWith("i\u0307")) {
        CombiningClassIsOther
      } else {
        CombiningClassIsNone
      }
    }
  }

  private def generateCharacterTest(): Unit = {
    val constants = List(
      constantDef("ReferenceJDKVersion", ReferenceJDKVersion),
    )

    val allZeroDigitCodePoints = computeZeroDigitCodePoints(start = 0)

    val toLowerCaseCodePointDiffString =
      computeToCaseCodePointDiffString("toLowerCase", toLowerCase(_), _.toLowerCase())
    val toUpperCaseCodePointDiffString =
      computeToCaseCodePointDiffString("toUpperCase", toUpperCase(_), _.toUpperCase())
    val toTitleCaseCodePointDiffStringToUpperCase =
      computeToCaseCodePointDiffString("toTitleCase", toTitleCase(_), _.toUpperCase())

    patchFile("test-suite/shared/src/test/scala/org/scalajs/testsuite/javalib/lang/CharacterTest.scala")(
      "constants" -> Patch.Lines(constants),
      "all-zero-digits" -> Patch.ArrayElements(allZeroDigitCodePoints.map(formatCP(_))),
      "tolowercase-code-point-diff-string" -> Patch.Lines(toLowerCaseCodePointDiffString),
      "touppercase-code-point-diff-string" -> Patch.Lines(toUpperCaseCodePointDiffString),
      "totitlecase-code-point-diff-string-touppercase" -> Patch.Lines(toTitleCaseCodePointDiffStringToUpperCase),
    )
  }

  private def computeToCaseCodePointDiffString(methodName: String,
      cpToCase: Int => Int, strToCase: String => String): Array[String] = {

    val b = Array.newBuilder[String]

    for (cp <- 0 to MAX_CODE_POINT) {
      val cpStr = cpToStr(cp)
      val caseCP = cpToCase(cp)
      val caseCPStr = cpToStr(caseCP)

      if (strToCase(cpStr) != caseCPStr)
        b += s"assertEquals(${formatCP(caseCP)}, Character.$methodName(${formatCP(cp)})) // $cpStr => $caseCPStr"
    }

    b.result()
  }
}
