package build

import java.lang.Character._

import scala.collection.mutable

import sbt.MessageOnlyException

import SourceFilePatches._

object UnicodeDataGen {
  /** When updating this to a newer version, you should then regenerate the
   *  Unicode data so they stay in sync.
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

    generateUnicodeData()
    generateUnicodeDataTest()
  }

  private final val CodePointBits = 21 // number of bits required to store a code point
  private final val TypeBits = 5 // types are in [0, 30]
  private final val PropsShift = TypeBits + 1 // 1 bit for the alternatingTypes flag

  /** Starting code point of the first range we are going to use.
   *
   *  The first 128 code points are ASCII. We want fast access to their
   *  properties, because they are much more common than others. Therefore, we
   *  store them in a "direct" array indexed by code point.
   *
   *  We also need to include the code points up to 0x009f, because they are
   *  the last ones with the `NonWhitespaceISOControl` property. That property
   *  must not appear in the compressed range array, otherwise we are 1 bit
   *  short.
   *
   *  Once we're at 0x009f, we might as well go up to 0x00bf, because those
   *  code points have almost only ranges of size 1. It is more
   *  memory-efficient to store them in the direct array.
   *
   *  Starting with 0x00c0, we have the latin1 capital letters with diacritics,
   *  and so we start the range-based strategy.
   */
  private final val FirstRangeStart = 0x00c0

  private final val LastRangeCP = MAX_CODE_POINT + 1

  private final class BooleanProp(val name: String, val directOnly: Boolean,
      val testFun: Int => Boolean) {

    override def toString(): String = name
  }

  private def isOtherLowerCase(cp: Int): Boolean =
    isLowerCase(cp) && getType(cp) != LOWERCASE_LETTER

  private def isOtherUpperCase(cp: Int): Boolean =
    isUpperCase(cp) && getType(cp) != UPPERCASE_LETTER

  private def isOtherAlphabetic(cp: Int): Boolean =
    isAlphabetic(cp) && !isLetter(cp) && getType(cp) != LETTER_NUMBER

  private def isOtherIDStart(cp: Int): Boolean = {
    isUnicodeIdentifierStart(cp) && !(getType(cp) match {
      case UPPERCASE_LETTER | LOWERCASE_LETTER | TITLECASE_LETTER | MODIFIER_LETTER | OTHER_LETTER | LETTER_NUMBER =>
        true
      case _ =>
        false
    })
  }

  private def isOtherIDPart(cp: Int): Boolean = {
    isUnicodeIdentifierPart(cp) && !isUnicodeIdentifierStart(cp) && !isIdentifierIgnorable(cp) && !(getType(cp) match {
      case CONNECTOR_PUNCTUATION | DECIMAL_DIGIT_NUMBER | COMBINING_SPACING_MARK | NON_SPACING_MARK =>
        true
      case _ =>
        false
    })
  }

  private def isNonWhitespaceISOControl(cp: Int): Boolean =
    isISOControl(cp) && !isWhitespace(cp)

  private def isDirectOnlyWhitespace(cp: Int): Boolean =
    cp < FirstRangeStart && isWhitespace(cp)

  private val AllBooleanProps: List[BooleanProp] = List(
    new BooleanProp("OtherLowerCase", false, isOtherLowerCase(_)),
    new BooleanProp("OtherUpperCase", false, isOtherUpperCase(_)),
    new BooleanProp("OtherAlphabetic", false, isOtherAlphabetic(_)),
    new BooleanProp("OtherIDStart", false, isOtherIDStart(_)),
    new BooleanProp("OtherIDPart", false, isOtherIDPart(_)),
    new BooleanProp("NonWhitespaceISOControl", true, isNonWhitespaceISOControl(_)),
    new BooleanProp("DirectWhitespace", true, isDirectOnlyWhitespace(_)),
  )

  private val PropsBits = AllBooleanProps.count(!_.directOnly)
  private val CodePointShift = PropsShift + PropsBits

  assert(CodePointShift + CodePointBits <= 32, "Cannot fit everything into an Int")

  /** Compute the "alternate" type for a given type.
   *
   *  Empirically, we find many code point ranges where the types are not
   *  constant, but alternating between two possibilities. A common example is
   *  the pair 1/2 (Lu/Ll). In order to reduce the number of ranges that we
   *  need, we can encode this pattern with `alternatingTypes`.
   *
   *  This function computes the alternative of a given type. Again,
   *  empirically, useful pairs are:
   *
   *  - 1/2 for Lu/Ll
   *  - 21/22 for Ps/Pe
   *  - 29/30 for Pi/Pf
   *
   *  Conveniently, these three pairs satisfy `a = b ^ 3`, and equivalently
   *  `b = a ^ 3`. So that is the formula that we use.
   *
   *  See also `UnicodeData.getDataRanges`.
   */
  private def alternateType(tpe: Int): Int = tpe ^ 3

  /** Data associated with a code point or range. */
  private final case class Data(tpe: Int, alternatingTypes: Boolean, props: List[BooleanProp]) {
    def isAlternate(that: Data, offset: Int): Boolean = {
      val wantedTpe = if ((offset & 1) == 0) tpe else alternateType(tpe)
      that.tpe == wantedTpe && that.props == this.props
    }

    def toAlternating: Data = copy(alternatingTypes = true)
  }

  /** An entry in the range-based table. */
  private final case class Entry(firstCP: Int, data: Data)

  private def computeData(cp: Int, booleanProps: List[BooleanProp]): Data = {
    val tpe = Character.getType(cp)
    val props = booleanProps.filter(prop => prop.testFun(cp))
    Data(tpe, alternatingTypes = false, props)
  }

  private def dataToInt(data: Data, propFlags: Map[BooleanProp, Int]): Int = {
    var intData = data.tpe
    if (data.alternatingTypes)
      intData |= (1 << TypeBits)
    for (prop <- data.props)
      intData |= propFlags(prop)
    intData
  }

  private def constantDef(name: String, value: Int): String =
    s"private final val $name = $value"

  private def generateUnicodeData(): Unit = {
    // Compute the tables

    val dataDirect = computeDataDirect()
    val dataRanges = computeDataRanges()

    // Print the entries in CSV format, to analyze patterns in a spreadsheet
    println("Start;Type;AltTypes;" + AllBooleanProps.mkString(";"))
    for (e <- dataRanges) {
      val boolProps = AllBooleanProps.map(p => if (e.data.props.contains(p)) "1" else "0").mkString(";")
      println(s"${e.firstCP};${e.data.tpe};${e.data.alternatingTypes};$boolProps")
    }

    // Allocate bits for the flags, based on how often they appear

    val propFlags = computePropFlags(dataRanges, AllBooleanProps)

    // Prepare the contents for the flags

    val constants = List(
      constantDef("PropsBits", PropsBits),
      constantDef("FirstRangeStart", FirstRangeStart),
      constantDef("RangeCount", dataRanges.size),
    )

    val propDefinitions =
      for ((prop, flag) <- propFlags.toList.sortBy(_._2))
        yield constantDef(prop.name + "Prop", flag)

    val intDataDirect = dataDirect.map(dataToInt(_, propFlags))

    var prevCP = 0
    val intDataRanges = dataRanges.map { e =>
      val diff = e.firstCP - prevCP
      prevCP = e.firstCP
      val truncatedData = dataToInt(e.data, propFlags) & ((1 << CodePointShift) - 1)
      (diff << CodePointShift) | truncatedData
    }

    // Print some statistics about the properties and the entry table

    for (p <- AllBooleanProps) {
      val entries = dataRanges.filter(e => e.data.props.contains(p))
      val tpes = entries.map(_.data.tpe).distinct.sorted
      println(s"$p (${entries.size}): ${tpes.mkString(", ")}")

      if (p.directOnly && entries.nonEmpty) {
        throw new MessageOnlyException(
            s"Found direct-only property ${p.name} in ranges:\n" +
            entries.mkString("\n"))
      }
    }

    println("Total number of ranges: " + dataRanges.size)

    // Ad hoc data

    val ideographicRanges = computeAdHocRanges(isIdeographic(_))
    val mirroredRanges = computeAdHocRanges(isMirrored(_))

    // Apply the patches

    patchFile("javalib/src/main/scala/java/lang/UnicodeData.scala")(
      "constants" -> Patch.Lines(constants),
      "prop-definitions" -> Patch.Lines(propDefinitions),
      "unicode-data-direct" -> Patch.ArrayElements(intDataDirect),
      "unicode-data-ranges" -> Patch.ArrayElements(intDataRanges),
      "ideographic-ranges" -> Patch.ArrayElements(ideographicRanges),
      "mirrored-ranges" -> Patch.ArrayElements(mirroredRanges),
    )
  }

  private def computeDataDirect(): Array[Data] = {
    for (cp <- (0 until FirstRangeStart).toArray) yield
      computeData(cp, AllBooleanProps)
  }

  private def computeDataRanges(): Array[Entry] =
    combineEntries(computeDataRangesInitial(FirstRangeStart, AllBooleanProps))

  /** Compute the initial ranges of data. */
  private def computeDataRangesInitial(firstRangeStart: Int,
      booleanProps: List[BooleanProp]): Array[Entry] = {

    val b = Array.newBuilder[Entry]
    var lastEntry = Entry(firstRangeStart, computeData(firstRangeStart, booleanProps))
    b += lastEntry

    for (cp <- (firstRangeStart + 1) to LastRangeCP) {
      val data = computeData(cp, booleanProps)
      if (data != lastEntry.data) {
        lastEntry = Entry(cp, data)
        b += lastEntry
      }
    }

    b.result()
  }

  /** Combine entries that have an alternating type pattern. */
  private def combineEntries(initial: Array[Entry]): Array[Entry] = {
    val b = Array.newBuilder[Entry]

    val initialLen = initial.length
    var i = 0

    while (i < initialLen - 2) {
      val entry = initial(i)
      val firstCP = entry.firstCP
      val data = entry.data

      val next = initial(i + 1)
      val nextNext = initial(i + 2)

      if (next.firstCP == firstCP + 1 && nextNext.firstCP == firstCP + 2 &&
          next.data.isAlternate(data, offset = 1)) {
        /* We have at least two consecutive ranges of size 1 that are
         * "alternate" versions of each other. We can combine them.
         */
        val start = i
        i += 2
        while (i < initialLen - 1 && initial(i + 1).firstCP == initial(i).firstCP + 1 &&
            initial(i).data.isAlternate(data, i - start)) {
          i += 1
        }

        b += entry.copy(data = data.toAlternating)
      } else {
        // Do not combine
        b += entry
        i += 1
      }
    }

    // Add the remaining entries
    while (i < initialLen) {
      b += initial(i)
      i += 1
    }

    b.result()
  }

  /** Computes an assignment of flags for boolean props.
   *
   *  Properties that are more often set receive lower flag values, so that
   *  data values are more often short.
   */
  private def computePropFlags(dataRanges: Array[Entry],
      booleanProps: List[BooleanProp]): Map[BooleanProp, Int] = {

    val frequencies = mutable.HashMap(booleanProps.map(prop => prop -> 0): _*)
    for (entry <- dataRanges) {
      for (prop <- entry.data.props)
        frequencies(prop) += 1
    }

    // Sort by frequencies, but keep non-direct-only properties first
    val sortedProps = frequencies.toList.sortBy(f => (f._1.directOnly, -f._2, f._1.name))
    val allocs =
      for ((propFreq, index) <- sortedProps.zipWithIndex)
        yield propFreq._1 -> (1 << (PropsShift + index))
    allocs.toMap
  }

  /** Compute the ranges of an ad hoc boolean property.
   *
   *  An ad hoc property is one that has little to no correlation with types or
   *  other properties, which means it is best to store it in a an ad hoc
   *  array of ranges.
   *
   *  An ad hoc property is always assumed to be `false` from 0 until the first
   *  element of the array, then alternating `true` and `false`.
   */
  private def computeAdHocRanges(f: Int => Boolean): Array[Int] = {
    val b = Array.newBuilder[Int]

    var lastPropValue = false
    for (cp <- 0 to LastRangeCP) {
      val propValue = f(cp)
      if (propValue != lastPropValue) {
        b += cp
        lastPropValue = propValue
      }
    }

    b.result()
  }

  // --- Tests generation ---

  // For tests, we use exactly the public methods of jl.Character
  private val AllBooleanPropsForTests: List[BooleanProp] = List(
    new BooleanProp("isLowerCase", false, isLowerCase(_)),
    new BooleanProp("isUpperCase", false, isUpperCase(_)),
    new BooleanProp("isTitleCase", false, isTitleCase(_)),
    new BooleanProp("isDigit", false, isDigit(_)),
    new BooleanProp("isDefined", false, isDefined(_)),
    new BooleanProp("isLetter", false, isLetter(_)),
    new BooleanProp("isLetterOrDigit", false, isLetterOrDigit(_)),
    new BooleanProp("isAlphabetic", false, isAlphabetic(_)),
    new BooleanProp("isIdeographic", false, isIdeographic(_)),
    new BooleanProp("isJavaIdentifierStart", false, isJavaIdentifierStart(_)),
    new BooleanProp("isJavaIdentifierPart", false, isJavaIdentifierPart(_)),
    new BooleanProp("isUnicodeIdentifierStart", false, isUnicodeIdentifierStart(_)),
    new BooleanProp("isUnicodeIdentifierPart", false, isUnicodeIdentifierPart(_)),
    new BooleanProp("isIdentifierIgnorable", false, isIdentifierIgnorable(_)),
    new BooleanProp("isSpaceChar", false, isSpaceChar(_)),
    new BooleanProp("isWhitespace", false, isWhitespace(_)),
    new BooleanProp("isISOControl", false, isISOControl(_)),
    new BooleanProp("isMirrored", false, isMirrored(_)),
  )

  private def generateUnicodeDataTest(): Unit = {
    val entries = computeDataRangesInitial(firstRangeStart = 0, AllBooleanPropsForTests)
    val propFlags = computePropFlags(entries, AllBooleanPropsForTests)

    val constants = List(
      constantDef("ReferenceJDKVersion", ReferenceJDKVersion),
    )

    /* The entries are stored in flattened form. If we store them as a large
     * array of case class instances, we get a "Method too large" error when
     * emitting JVM bytecode.
     */
    val testEntriesFirstCPs = List.newBuilder[Int]
    val testEntriesLastCPs = List.newBuilder[Int]
    val testEntriesDatas = List.newBuilder[Int]
    var i = 0
    while (i != entries.length) {
      val entry = entries(i)
      val lastCP =
        if (i == entries.length - 1) LastRangeCP
        else entries(i + 1).firstCP - 1
      testEntriesFirstCPs += entry.firstCP
      testEntriesLastCPs += lastCP
      testEntriesDatas += dataToInt(entry.data, propFlags)
      i += 1
    }
    testEntriesFirstCPs += -1
    testEntriesLastCPs += -1
    testEntriesDatas += dataToInt(computeData(-1, AllBooleanPropsForTests), propFlags)

    val testProperties = AllBooleanPropsForTests.map { prop =>
      s"""assertEquals(s"${prop.name}($$cpStr)", entry.hasFlag(${propFlags(prop)}), ${prop.name}(cp))"""
    }

    patchFile("test-suite/shared/src/test/scala/org/scalajs/testsuite/javalib/lang/UnicodeDataTest.scala")(
      "constants" -> Patch.Lines(constants),
      "test-entries-firstcp" -> Patch.ArrayElements(testEntriesFirstCPs.result()),
      "test-entries-lastcp" -> Patch.ArrayElements(testEntriesLastCPs.result()),
      "test-entries-data" -> Patch.ArrayElements(testEntriesDatas.result()),
      "test-properties" -> Patch.Lines(testProperties),
    )
  }
}
