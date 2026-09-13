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

  /** Cut-off count of a property (lower bound) to allocate its flag in bits
   *  that are less significant than the code point.
   *
   *  This was found experimentally to optimize the encoded size of the
   *  database. When changing the reference version, you may want to tweak
   *  that value depending on the reported statistics.
   */
  private final val PropCountCutOffForLessSignificantBits = 50

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

  private final val CodePointBits = 21 // number of bits required to store a code point
  private final val TypeBits = 5 // types are in [0, 30]
  private final val AltTypesFlag = 1 << TypeBits
  private final val FirstPropShift = TypeBits + 1 // 1 bit for the alternatingTypes flag

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
   *  Finally, since we're there, we include 0x00a0 as well. It is one of the
   *  3 code points that have an exclusion in the spec of isWhitespace. With
   *  this one in the direct set, we need one fewer special case in that
   *  method.
   */
  private final val FirstRangeStart = 0x00a1

  // Helpers

  private def constantDef(name: String, value: Int): String =
    s"private final val $name = $value"

  private def constantDefHex(name: String, value: Int): String =
    s"private final val $name = 0x${value.toHexString}"

  private def formatCP(cp: Int): String =
    f"0x$cp%04x"

  private def formatCPuEscape(cp: Int): String =
    f"\\u$cp%04x"

  // --- jl.UnicodeData ---

  private final class BooleanProp(val name: String, val directOnly: Boolean,
      val testFun: Int => Boolean) {

    override def toString(): String = name
  }

  private def isLetterOrLetterNumber(cp: Int): Boolean =
    isLetter(cp) || getType(cp) == LETTER_NUMBER

  /** Does the given code point have the contributory property `Other_LowerCase`? */
  private def isOtherLowerCase(cp: Int): Boolean =
    isLowerCase(cp) && getType(cp) != LOWERCASE_LETTER

  /** Does the given code point have the contributory property `Other_UpperCase`? */
  private def isOtherUpperCase(cp: Int): Boolean =
    isUpperCase(cp) && getType(cp) != UPPERCASE_LETTER

  /** Does the given code point have the contributory property `Other_Alphabetic`? */
  private def isOtherAlphabetic(cp: Int): Boolean =
    isAlphabetic(cp) && !isLetterOrLetterNumber(cp) && !isLowerCase(cp) && !isUpperCase(cp)

  /** Does the given code point have the contributory property `Other_ID_Start`? */
  private def isOtherIDStart(cp: Int): Boolean =
    isUnicodeIdentifierStart(cp) && !isLetterOrLetterNumber(cp)

  /** Does the given code point have the contributory property `Other_ID_Continue`? */
  private def isOtherIDContinue(cp: Int): Boolean = {
    isUnicodeIdentifierPart(cp) &&
    !isUnicodeIdentifierStart(cp) &&
    !isIdentifierIgnorable(cp) &&
    !(getType(cp) match {
      case CONNECTOR_PUNCTUATION | DECIMAL_DIGIT_NUMBER |
          COMBINING_SPACING_MARK | NON_SPACING_MARK =>
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
    new BooleanProp("OtherIDContinue", false, isOtherIDContinue(_)),
    new BooleanProp("NonWhitespaceISOControl", true, isNonWhitespaceISOControl(_)),
    new BooleanProp("DirectWhitespace", true, isDirectOnlyWhitespace(_)),
  )

  assert(FirstPropShift + CodePointBits + AllBooleanProps.count(!_.directOnly) <= 32,
      "Cannot fit everything into an Int")

  /** Computes the "alternate" type for a given type.
   *
   *  See the comment at the top of `UnicodeData`.
   */
  private def alternateType(tpe: Int): Int = tpe ^ 3

  /** Data associated with a code point or range. */
  private final case class Data(tpe: Int, props: List[BooleanProp]) {
    def isAlternateOf(that: Data): Boolean =
      that.tpe == alternateType(tpe) && that.props == this.props
  }

  /** An entry in the range-based table. */
  private final case class Entry(firstCP: Int, data: Data, alternatingTypes: Boolean) {
    def toAlternating: Entry = {
      /* Even code points will have the type stored in the data.
       * Odd code points will get the alternate type.
       * If the first code point of this range is odd, we must therefore store
       * the alternate of its type.
       */
      val newData =
        if ((firstCP & 1) == 0) data
        else data.copy(tpe = alternateType(data.tpe))
      Entry(firstCP, newData, alternatingTypes = true)
    }

    def matches(cp: Int, otherData: Data): Boolean = {
      if (alternatingTypes && (cp & 1) != 0) data.isAlternateOf(otherData)
      else data == otherData
    }
  }

  private def computeData(cp: Int, booleanProps: List[BooleanProp]): Data = {
    val tpe = Character.getType(cp)
    val props = booleanProps.filter(_.testFun(cp))
    Data(tpe, props)
  }

  private def dataToInt(data: Data, propFlags: Map[BooleanProp, Int]): Int = {
    var intData = data.tpe
    for (prop <- data.props)
      intData |= propFlags(prop)
    intData
  }

  private def entryToInt(entry: Entry, propFlags: Map[BooleanProp, Int],
      codePointShift: Int, codePointMask: Int): Int = {
    val Entry(firstCP, data, alternatingTypes) = entry
    val truncatedData = dataToInt(data, propFlags) & ~codePointMask
    val altTypes = if (alternatingTypes) AltTypesFlag else 0
    (firstCP << codePointShift) | truncatedData | altTypes
  }

  private def generateUnicodeData(): Unit = {
    // Compute the range data

    val dataRanges = computeDataRanges()
    val sortedPropCounts = computePropCounts(dataRanges)

    printStatistics(dataRanges, sortedPropCounts)

    val lowerCaseRanges = computeCasingRanges(isUpperCase = false, forTests = false)
    val upperCaseRanges = computeCasingRanges(isUpperCase = true, forTests = false)

    val largestCasingCP = Math.max(lowerCaseRanges.last.firstCP, upperCaseRanges.last.firstCP)
    val casingCPBits = 32 - Integer.numberOfLeadingZeros(largestCasingCP)

    val caseIgnorableExtra = computeCaseIgnorableExtra()

    // Allocate bits for the flags, based on how often they appear

    val (propFlags, codePointShift) =
      computePropFlags(dataRanges, sortedPropCounts)
    val codePointMask = ((1 << CodePointBits) - 1) << codePointShift

    // Prepare the contents for the flags

    val constants = List(
      constantDef("CodePointShift", codePointShift),
      constantDef("FirstRangeStart", FirstRangeStart),
      constantDef("RangeCount", dataRanges.size),
      constantDef("CasingCPBits", casingCPBits),
    )

    val propDefinitions =
      for ((prop, flag) <- propFlags.toList.sortBy(_._2 ^ Int.MinValue)) // unsigned ordering
        yield constantDefHex(prop.name + "Prop", flag)

    // Build the tables

    val intDataDirect = computeDataDirect().map(dataToInt(_, propFlags))
    val intDataRanges = dataRanges.map(entryToInt(_, propFlags, codePointShift, codePointMask))

    // Ad hoc data

    val ideographicRanges = computeAdHocRanges(isIdeographic(_))
    val mirroredRanges = computeAdHocRanges(isMirrored(_))

    // Casing data

    val lowerCasingTables = computeCasingTables(lowerCaseRanges, casingCPBits)
    val upperCasingTables = computeCasingTables(upperCaseRanges, casingCPBits)

    // Apply the patches

    patchFile("javalib/src/main/scala/java/lang/UnicodeData.scala")(
      "constants" -> Patch.Lines(constants),
      "prop-definitions" -> Patch.Lines(propDefinitions),
      "unicode-data-direct" -> Patch.ArrayElements.hexInts(intDataDirect),
      "unicode-data-ranges" -> Patch.ArrayElements.hexInts(intDataRanges),
      "ideographic-ranges" -> Patch.ArrayElements(ideographicRanges.map(formatCP(_))),
      "mirrored-ranges" -> Patch.ArrayElements(mirroredRanges.map(formatCP(_))),
      "lowercase-entries" -> Patch.ArrayElements.hexInts(lowerCasingTables.entries),
      "lowercase-instructions" -> Patch.ArrayElements.hexInts(lowerCasingTables.instructions),
      "uppercase-entries" -> Patch.ArrayElements.hexInts(upperCasingTables.entries),
      "uppercase-instructions" -> Patch.ArrayElements.hexInts(upperCasingTables.instructions),
      "case-ignorable-extra" -> Patch.ArrayElements(caseIgnorableExtra.map(formatCP(_))),
    )
  }

  /** Prints some statistics about the entry table. */
  private def printStatistics(dataRanges: Array[Entry],
      sortedPropCounts: List[(BooleanProp, Int)]): Unit = {
    for ((prop, count) <- sortedPropCounts)
      println(s"$prop: $count")

    println("Ranges with alternatingTypes: " + dataRanges.count(_.alternatingTypes))
    println("Total number of ranges: " + dataRanges.size)
  }

  private def computeDataDirect(): Array[Data] = {
    for (cp <- (0 until FirstRangeStart).toArray) yield
      computeData(cp, AllBooleanProps)
  }

  /** Compute the data ranges. */
  private def computeDataRanges(): Array[Entry] = {
    val b = Array.newBuilder[Entry]

    val firstRangeData = computeData(FirstRangeStart, AllBooleanProps)
    var nextEntry = Entry(FirstRangeStart, firstRangeData, alternatingTypes = false)

    for (cp <- (FirstRangeStart + 1) to FirstInvalidCP) {
      val data = computeData(cp, AllBooleanProps)
      if (!nextEntry.matches(cp, data)) {
        if (nextEntry.firstCP + 1 == cp && nextEntry.data.isAlternateOf(data)) {
          // We can build an alternating range.
          assert(!nextEntry.alternatingTypes, nextEntry)
          nextEntry = nextEntry.toAlternating
        } else {
          b += nextEntry
          nextEntry = Entry(cp, data, alternatingTypes = false)
        }
      }
    }

    b += nextEntry
    b.result()
  }

  /** Computes how many times each property appears in the range data.
   *
   *  The result is sorted by decreasing counts.
   */
  private def computePropCounts(dataRanges: Array[Entry]): List[(BooleanProp, Int)] = {
    // Compute counts of props that apply to the ranges
    val rangeProps = AllBooleanProps.filter(!_.directOnly)
    val propCounts = mutable.HashMap(rangeProps.map(prop => prop -> 0): _*)
    for (entry <- dataRanges) {
      for (prop <- entry.data.props if !prop.directOnly)
        propCounts(prop) += 1
    }

    // Sort by counts, tie-break on the name for stability
    propCounts.toList.sortBy(f => (-f._2, f._1.name))
  }

  /** Computes an assignment of flags for boolean props.
   *
   *  Properties that are more often set receive lower flag values, so that
   *  data values are more often short.
   *
   *  Also computes the shift for the code point value. Frequent properties are
   *  assigned to bits less significant than the code point. Rare properties
   *  are assigned to the most significant bits. Direct-only properties are
   *  assigned to bits within the code point bits.
   *
   *  This flexibility optimizes for the smallest diffs of encoded data.
   */
  private def computePropFlags(dataRanges: Array[Entry],
      sortedRangeProps: List[(BooleanProp, Int)]): (Map[BooleanProp, Int], Int) = {

    val codePointShift =
      FirstPropShift + sortedRangeProps.count(_._2 >= PropCountCutOffForLessSignificantBits)

    val allocs = Map.newBuilder[BooleanProp, Int]

    /* Allocate flags for the props that apply to ranges.
     * Before or after the code point bits, depending on their counts.
     */
    for (((prop, count), index) <- sortedRangeProps.zipWithIndex) yield {
      val shift0 = FirstPropShift + index
      val shift =
        if (count >= PropCountCutOffForLessSignificantBits) shift0
        else shift0 + CodePointBits
      allocs += prop -> (1 << shift)
    }

    // Allocate flags for the direct-only props, on top of the code point
    val directOnlyProps = AllBooleanProps.filter(_.directOnly)
    for ((prop, index) <- directOnlyProps.sortBy(_.name).zipWithIndex) {
      val shift = codePointShift + index
      allocs += prop -> (1 << shift)
    }

    (allocs.result(), codePointShift)
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
    for (cp <- 0 to FirstInvalidCP) {
      val propValue = f(cp)
      if (propValue != lastPropValue) {
        b += cp
        lastPropValue = propValue
      }
    }

    b.result()
  }

  /** Casing "instruction", as described in `UnicodeData.scala` under "Casing algorithms".
   *
   *  Inline deltas and large deltas are represented by the same case class in
   *  the generator. The transformation to bits in `computeCasingTables` chooses
   *  the best representation.
   *
   *  `AlternatingDelta` explicitly carries the same-parity parity. It is only
   *  valid to create one with the parity that will be implicit for the target
   *  casing operation (+1 for lowercasing, -1 for uppercasing).
   */
  private sealed abstract class CasingInstr

  private object CasingInstr {
    final case class SimpleDelta(delta: Int) extends CasingInstr
    final case class AlternatingDelta(delta: Int) extends CasingInstr
    final case class TwoCharsWithSimple(simple: Char, c1: Char, c2: Char) extends CasingInstr
    final case class ThreeCharsNoSimple(c1: Char, c2: Char, c3: Char) extends CasingInstr
    final case object UpperSigmaLowerCasing extends CasingInstr
    final case object GreekYpogegrammeniProsgegrammeniRangeUpperCasing extends CasingInstr

    final case class FullMappingForTests(simple: Int, full: String) extends CasingInstr
  }

  private final case class CasingEntry(firstCP: Int, instr: CasingInstr) {
    def matches(cp: Int, cpInstr: CasingInstr): Boolean = {
      (this.instr, cpInstr) match {
        case (_, _: CasingInstr.AlternatingDelta) =>
          throw new AssertionError(s"unexpected cpInstr $cpInstr for $cp")
        case (CasingInstr.AlternatingDelta(altDelta), CasingInstr.SimpleDelta(cpDelta)) =>
          if ((cp & 1) != (firstCP & 1))
            cpDelta == 0
          else
            cpDelta == altDelta
        case _ =>
          this.instr == cpInstr
      }
    }

    def toAlternatingOption(alternatingDelta: Int): Option[CasingEntry] = instr match {
      case CasingInstr.SimpleDelta(delta) if delta == alternatingDelta =>
        Some(CasingEntry(firstCP, CasingInstr.AlternatingDelta(delta)))
      case _ =>
        None
    }
  }

  private def computeCasingInstr(cp: Int, isUpperCase: Boolean, forTests: Boolean): CasingInstr = {
    if (!isUpperCase && cp == 0x03a3 && !forTests) {
      // Σ has a dedicated lowercasing algorithm
      CasingInstr.UpperSigmaLowerCasing
    } else if (isUpperCase && (cp >= 0x1f80 && cp <= 0x1faf) && !forTests) {
      /* A large range of Greek letters with a two-char uppercase where the
       * first one is a unique delta. They get a dedicated instruction.
       */
      CasingInstr.GreekYpogegrammeniProsgegrammeniRangeUpperCasing
    } else if (!Character.isValidCodePoint(cp)) {
      CasingInstr.SimpleDelta(0)
    } else {
      val cpStr = Character.toString(cp)
      val simpleMapping =
        if (isUpperCase) Character.toUpperCase(cp)
        else Character.toLowerCase(cp)
      val fullMapping =
        if (isUpperCase) cpStr.toUpperCase()
        else cpStr.toLowerCase()

      if (fullMapping == Character.toString(simpleMapping)) {
        CasingInstr.SimpleDelta(simpleMapping - cp)
      } else if (forTests) {
        CasingInstr.FullMappingForTests(simpleMapping, fullMapping)
      } else {
        assert(simpleMapping <= Char.MaxValue, s"$cp; $simpleMapping")
        fullMapping.length() match {
          case 2 =>
            CasingInstr.TwoCharsWithSimple(simpleMapping.toChar,
                fullMapping.charAt(0), fullMapping.charAt(1))
          case 3 =>
            assert(simpleMapping == cp, s"$cp; $simpleMapping")
            CasingInstr.ThreeCharsNoSimple(fullMapping.charAt(0),
                fullMapping.charAt(1), fullMapping.charAt(2))
          case len =>
            throw new AssertionError(
                s"$cp; full mapping has $len characters")
        }
      }
    }
  }

  private def computeCasingRanges(isUpperCase: Boolean, forTests: Boolean): Array[CasingEntry] = {
    val b = Array.newBuilder[CasingEntry]

    val alternatingDelta = if (isUpperCase) -1 else 1

    val firstCP = if (forTests) 0 else FirstRangeStart
    val firstInstr = computeCasingInstr(firstCP, isUpperCase, forTests)
    var nextEntry = CasingEntry(firstCP, firstInstr)

    for (cp <- (firstCP + 1) to FirstInvalidCP) {
      val instr = computeCasingInstr(cp, isUpperCase, forTests)

      if (!nextEntry.matches(cp, instr)) {
        nextEntry.toAlternatingOption(alternatingDelta) match {
          case Some(altEntry) if nextEntry.firstCP + 1 == cp && altEntry.matches(cp, instr) &&
              altEntry.matches(cp + 1, computeCasingInstr(cp + 1, isUpperCase, forTests)) =>
            // Build an alternating range
            nextEntry = altEntry

          case _ =>
            // Start a new entry
            b += nextEntry
            nextEntry = CasingEntry(cp, instr)
        }
      }
    }

    b += nextEntry
    b.result()
  }

  /** A pair of tables for a given casing operation.
   *
   *  See `UnicodeData.scala` under "Casing algorithms" for their format
   *  definition.
   */
  private final case class CasingTables(entries: Array[Int], instructions: Array[Int])

  private def computeCasingTables(entries: Array[CasingEntry], codePointBits: Int): CasingTables = {
    val instructions = mutable.ArrayBuffer.empty[Int]

    val instructionFlag = 1 << codePointBits

    val payloadShift = 1 + codePointBits
    val payloadBits = 32 - payloadShift
    val largeDeltaThreshold = 1 << (payloadBits - 1) // -1 because of the sign bit
    val alternatingDeltaSentinel = -largeDeltaThreshold

    def isSmallDelta(delta: Int): Boolean =
      Math.abs(delta) < largeDeltaThreshold

    def pack2Chars(lo: Char, hi: Char): Int =
      lo.toInt | (hi.toInt << 16)

    def encodeInstruction(instr: CasingInstr): Array[Int] = instr match {
      case CasingInstr.SimpleDelta(delta) =>
        Array((delta << 3) | 0x00)
      case CasingInstr.AlternatingDelta(_) =>
        throw new AssertionError(s"$instr should always be encoded in the payload")
      case CasingInstr.TwoCharsWithSimple(simple, c1, c2) =>
        Array(pack2Chars(0x01, simple), pack2Chars(c1, c2))
      case CasingInstr.ThreeCharsNoSimple(c1, c2, c3) =>
        Array(pack2Chars(0x02, c1), pack2Chars(c2, c3))
      case CasingInstr.UpperSigmaLowerCasing =>
        Array(0x03)
      case CasingInstr.GreekYpogegrammeniProsgegrammeniRangeUpperCasing =>
        Array(0x04)
      case CasingInstr.FullMappingForTests(_, _) =>
        throw new AssertionError(s"Unexpected test-only instr $instr")
    }

    def addInstruction(encodedInstr: Array[Int]): Int = {
      val existing = instructions.indexOfSlice(encodedInstr)
      if (existing >= 0) {
        existing
      } else {
        val startIndex = instructions.size
        instructions ++= encodedInstr
        startIndex
      }
    }

    val entryTable = entries.map { entry =>
      entry.instr match {
        case CasingInstr.SimpleDelta(delta) if isSmallDelta(delta) =>
          // (very common) optimization: fit the delta directly in the entry bits
          (delta << payloadShift) | entry.firstCP

        case _ if entry.firstCP > Char.MaxValue =>
          throw new AssertionError(s"unexpected non-simple supplementary CP entry: $entry")

        case CasingInstr.AlternatingDelta(_) =>
          (alternatingDeltaSentinel << payloadShift) | entry.firstCP

        case instr =>
          /* General case: build an instruction sequence, add it to the
           * instruction buffer (or find an existing copy), and enter its
           * start index in the payload field.
           */
          val encodedInstruction = encodeInstruction(instr)
          val startIndex = addInstruction(encodedInstruction)
          (startIndex << payloadShift) | instructionFlag | entry.firstCP
      }
    }

    val instructionsTable = instructions.toArray

    CasingTables(entryTable, instructionsTable)
  }

  /** Computes the list of case-ignorable "extra" code points.
   *
   *  These are code points whose `WorkBreak` property is `MidLetter`,
   *  `MidNumLet` or `Single_Quote`.
   *
   *  Together with code points with general categories Mn, Me, Cf, Lm and Sk,
   *  they have the `Case_Ignorable` derived property, used in the `Final_Sigma`
   *  context.
   */
  private def computeCaseIgnorableExtra(): Array[Int] = {
    /* In theory, we should be able to compute this as the set of code points
     * `C` such that
     *
     * - the general category of `C` is not among Mn Me Cf Lm Sk, and
     * - `CΣ` lowercases to 'σ', and
     * - `αCΣ` lowercases to 'ς'.
     *
     * Unfortunately, before v28, the JDK does not use the right property when
     * lowercasing Σ. See https://bugs.openjdk.org/browse/JDK-8133167.
     *
     * Since the list is short, we hard-code it instead.
     *
     * See https://www.unicode.org/Public/UNIDATA/auxiliary/WordBreakProperty.txt
     * for the WordBreak property.
     */

    Array(
      // Single_Quote
      0x0027, // Po  APOSTROPHE

      // MidLetter
      0x003a, // Po  COLON
      0x00b7, // Po  MIDDLE DOT
      0x0387, // Po  GREEK ANO TELEIA
      0x055f, // Po  ARMENIAN ABBREVIATION MARK
      0x05f4, // Po  HEBREW PUNCTUATION GERSHAYIM
      0x2027, // Po  HYPHENATION POINT
      0xfe13, // Po  PRESENTATION FORM FOR VERTICAL COLON
      0xfe55, // Po  SMALL COLON
      0xff1a, // Po  FULLWIDTH COLON

      // MidNumLet
      0x002e, // Po  FULL STOP
      0x2018, // Pi  LEFT SINGLE QUOTATION MARK
      0x2019, // Pf  RIGHT SINGLE QUOTATION MARK
      0x2024, // Po  ONE DOT LEADER
      0xfe52, // Po  SMALL FULL STOP
      0xff07, // Po  FULLWIDTH APOSTROPHE
      0xff0e, // Po  FULLWIDTH FULL STOP
    ).sorted
  }

  // --- jl.Character ---

  private def generateCharacter(): Unit = {
    val titleCaseMappings = computeTitleCaseMappings()

    val unicodeBlocks = computeUnicodeBlocks()
    val unicodeBlockConstants = List(constantDef("BlockCount", unicodeBlocks.size))

    val nonASCIIZeroDigitCodePoints = computeZeroDigitCodePoints(FirstNonASCII)
    val combiningClasses = computeCombiningClasses()

    patchFile("javalib/src/main/scala/java/lang/Character.scala")(
      "titlecase-mappings" -> Patch.Lines(titleCaseMappings),
      "unicode-block-constants" -> Patch.Lines(unicodeBlockConstants),
      "unicode-blocks" -> Patch.Lines(unicodeBlocks),
      "non-ascii-zero-digits" -> Patch.ArrayElements(nonASCIIZeroDigitCodePoints.map(formatCP(_))),
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
      val cpStr = Character.toString(cp)
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

  // --- Tests generation ---

  // For tests, we use exactly the public methods of jl.Character
  private val AllBooleanPropsForTests: List[(String, Int => Boolean)] = List(
    ("isLowerCase", isLowerCase(_)),
    ("isUpperCase", isUpperCase(_)),
    ("isTitleCase", isTitleCase(_)),
    ("isDigit", isDigit(_)),
    ("isDefined", isDefined(_)),
    ("isLetter", isLetter(_)),
    ("isLetterOrDigit", isLetterOrDigit(_)),
    ("isAlphabetic", isAlphabetic(_)),
    ("isIdeographic", isIdeographic(_)),
    ("isJavaIdentifierStart", isJavaIdentifierStart(_)),
    ("isJavaIdentifierPart", isJavaIdentifierPart(_)),
    ("isUnicodeIdentifierStart", isUnicodeIdentifierStart(_)),
    ("isUnicodeIdentifierPart", isUnicodeIdentifierPart(_)),
    ("isIdentifierIgnorable", isIdentifierIgnorable(_)),
    ("isSpaceChar", isSpaceChar(_)),
    ("isWhitespace", isWhitespace(_)),
    ("isISOControl", isISOControl(_)),
    ("isMirrored", isMirrored(_)),
  )

  private def generateUnicodeDataTest(): Unit = {
    val constants = List(
      constantDef("ReferenceJDKVersion", ReferenceJDKVersion),
    )

    val booleanPropsWithFlags = for {
      ((name, fun), index) <- AllBooleanPropsForTests.zipWithIndex
    } yield {
      (name, fun, 1 << (TypeBits + index))
    }

    // Compute all the entries and their data
    val testEntriesFirstCPs = List.newBuilder[Int]
    val testEntriesLastCPs = List.newBuilder[Int]
    val testEntriesDatas = List.newBuilder[Int]
    var lastData = -1
    for (cp <- 0 to FirstInvalidCP) {
      var data = getType(cp)
      for ((_, test, flag) <- booleanPropsWithFlags) {
        if (test(cp))
          data |= flag
      }
      if (data != lastData) {
        lastData = data
        if (cp != 0)
          testEntriesLastCPs += (cp - 1)
        testEntriesFirstCPs += cp
        testEntriesDatas += data
      }
    }
    testEntriesLastCPs += FirstInvalidCP

    // Also add an entry for -1
    testEntriesFirstCPs += -1
    testEntriesLastCPs += -1
    testEntriesDatas += lastData

    val testProperties = for {
      (name, _, flag) <- booleanPropsWithFlags
    } yield {
      s"""assertEquals(s"$name($$cpStr)", hasFlag(0x${flag.toHexString}), $name(cp))"""
    }

    // Entries for casing operations
    val upperCaseTestEntries = computeCasingTestTable(isUpperCase = true)
    val lowerCaseTestEntries = computeCasingTestTable(isUpperCase = false)

    patchFile("test-suite/shared/src/test/scala/org/scalajs/testsuite/javalib/lang/UnicodeDataTest.scala")(
      "constants" -> Patch.Lines(constants),
      "test-entries-firstcp" -> Patch.ArrayElements(testEntriesFirstCPs.result().map(formatCP(_))),
      "test-entries-lastcp" -> Patch.ArrayElements(testEntriesLastCPs.result().map(formatCP(_))),
      "test-entries-data" -> Patch.ArrayElements.hexInts(testEntriesDatas.result()),
      "test-properties" -> Patch.Lines(testProperties),
      "uppercase-test-entries" -> Patch.Lines(upperCaseTestEntries),
      "lowercase-test-entries" -> Patch.Lines(lowerCaseTestEntries),
    )
  }

  private def computeCasingTestTable(isUpperCase: Boolean): Seq[String] = {
    val entries = computeCasingRanges(isUpperCase, forTests = true)

    val b = Array.newBuilder[String]

    def entryLine(firstCPInt: Int, lastCPInt: Int, instr: CasingInstr): String = {
      val firstCP = formatCP(firstCPInt)
      val lastCP = formatCP(lastCPInt)

      instr match {
        case CasingInstr.SimpleDelta(delta) =>
          s"simpleCasingEntry($firstCP, $lastCP, $delta),"
        case CasingInstr.AlternatingDelta(delta) =>
          s"alternatingCasingEntry($firstCP, $lastCP, $delta),"
        case CasingInstr.FullMappingForTests(simple, full) =>
          val fullString = full.map(c => formatCPuEscape(c.toInt)).mkString("\"", "", "\"")
          s"fullCasingEntry($firstCP, $lastCP, ${formatCP(simple)}, $fullString),"
        case _ =>
          throw new AssertionError(s"Unexpected instr $instr for tests")
      }
    }

    for (i <- 0 until entries.size - 1) {
      val entry = entries(i)
      val firstCP = entry.firstCP
      val lastCP = entries(i + 1).firstCP - 1
      b += entryLine(firstCP, lastCP, entry.instr)
    }

    // Add an entry for the last range
    val lastEntry = entries.last
    b += entryLine(lastEntry.firstCP, FirstInvalidCP, lastEntry.instr)

    // Add an entry for -1
    b += entryLine(-1, -1, CasingInstr.SimpleDelta(0)).stripSuffix(",")

    b.result()
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
      val cpStr = Character.toString(cp)
      val caseCP = cpToCase(cp)
      val caseCPStr = Character.toString(caseCP)

      if (strToCase(cpStr) != caseCPStr)
        b += s"assertEquals(${formatCP(caseCP)}, Character.$methodName(${formatCP(cp)})) // $cpStr => $caseCPStr"
    }

    b.result()
  }
}
