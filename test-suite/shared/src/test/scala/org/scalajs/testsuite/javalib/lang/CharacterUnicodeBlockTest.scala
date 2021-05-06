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

package org.scalajs.testsuite.javalib.lang

import java.lang.Character.UnicodeBlock
import org.junit.Test
import org.junit.Assert._
import org.scalajs.testsuite.utils.AssertThrows.assertThrows

class CharacterUnicodeBlockTest {

  @Test def forNameNormalizations(): Unit = {
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Surrogates-Area"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Surrogates Area"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("SurrogatesArea"))
    assertEquals(UnicodeBlock.SURROGATES_AREA, UnicodeBlock.forName("SURROGATES_AREA"))

    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.forName("Basic Latin"))
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.forName("Basic LatiN"))
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.forName("BASIC_LATIN"))
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.forName("BASIC_LATIN"))

    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Basic Lat iN"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Basic Lat in"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Basic-Latin"))

    assertEquals(UnicodeBlock.ARABIC_EXTENDED_A, UnicodeBlock.forName("Arabic Extended-A"))
    assertEquals(UnicodeBlock.ARABIC_EXTENDED_A, UnicodeBlock.forName("ARABIC EXTENDED-A"))
    assertEquals(UnicodeBlock.ARABIC_EXTENDED_A, UnicodeBlock.forName("Arabic_Extended_A"))
    assertEquals(UnicodeBlock.ARABIC_EXTENDED_A, UnicodeBlock.forName("ARABIC_EXTENDED_A"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Arabic Extended_A"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Arabic_Extended-A"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Arabic_ExtendedA"))
    assertEquals(UnicodeBlock.ARABIC_EXTENDED_A, UnicodeBlock.forName("ArabicExtended-A"))
    assertEquals(UnicodeBlock.ARABIC_EXTENDED_A, UnicodeBlock.forName("ARABICEXTENDED-A"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Arabic ExtendedA"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("Arabic_ExtendedA"))
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("ArabicExtendedA"))

    assertEquals(UnicodeBlock.CJK_UNIFIED_IDEOGRAPHS_EXTENSION_A,
        UnicodeBlock.forName("CJK Unified Ideographs Extension A"))
    assertThrows(classOf[IllegalArgumentException],
        UnicodeBlock.forName("CJK Unified Ideographs Extension-A"))
  }

  @Test def  forNameHistorical(): Unit = {
    // scalastyle:off line.size.limit
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("GREEK_AND_COPTIC"))
    assertEquals(UnicodeBlock.GREEK, UnicodeBlock.forName("Greek and Coptic"))
    assertEquals(UnicodeBlock.GREEK, UnicodeBlock.forName("GreekandCoptic"))
    assertEquals(UnicodeBlock.GREEK, UnicodeBlock.forName("GREEK"))
    assertEquals(UnicodeBlock.GREEK, UnicodeBlock.forName("Greek"))

    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("COMBINING_DIACRITICAL_MARKS_FOR_SYMBOLS"))
    assertEquals(UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS, UnicodeBlock.forName("Combining Diacritical Marks for Symbols"))
    assertEquals(UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS, UnicodeBlock.forName("CombiningDiacriticalMarksforSymbols"))
    assertEquals(UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS, UnicodeBlock.forName("COMBINING_MARKS_FOR_SYMBOLS"))
    assertEquals(UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS, UnicodeBlock.forName("Combining Marks for Symbols"))
    assertEquals(UnicodeBlock.COMBINING_MARKS_FOR_SYMBOLS, UnicodeBlock.forName("CombiningMarksforSymbols"))

    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("CYRILLIC_SUPPLEMENT"))
    assertEquals(UnicodeBlock.CYRILLIC_SUPPLEMENTARY, UnicodeBlock.forName("Cyrillic Supplement"))
    assertEquals(UnicodeBlock.CYRILLIC_SUPPLEMENTARY, UnicodeBlock.forName("CyrillicSupplement"))
    assertEquals(UnicodeBlock.CYRILLIC_SUPPLEMENTARY, UnicodeBlock.forName("CYRILLIC_SUPPLEMENTARY"))
    assertEquals(UnicodeBlock.CYRILLIC_SUPPLEMENTARY, UnicodeBlock.forName("Cyrillic Supplementary"))
    assertEquals(UnicodeBlock.CYRILLIC_SUPPLEMENTARY, UnicodeBlock.forName("CyrillicSupplementary"))
    // scalastyle:on line.size.limit
  }

  @Test def ofIntOutOfRangeThrowsIllegalArgumentException(): Unit = {
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.of(Character.MAX_CODE_POINT + 1))
  }

  @Test def forNameNotFoundThrowsIllegalArgumentException(): Unit = {
    assertThrows(classOf[IllegalArgumentException], UnicodeBlock.forName("INVALID_NAME"))
  }

  @Test def ofChar(): Unit = {
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.of(0x0000.toChar))
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.of(0x007f.toChar))
    assertEquals(UnicodeBlock.MEETEI_MAYEK_EXTENSIONS, UnicodeBlock.of(0xaae0.toChar))
    assertEquals(UnicodeBlock.MEETEI_MAYEK_EXTENSIONS, UnicodeBlock.of(0xaaff.toChar))
    assertEquals(UnicodeBlock.SPECIALS, UnicodeBlock.of(0xfff0.toChar))
    assertEquals(UnicodeBlock.SPECIALS, UnicodeBlock.of(0xffff.toChar))
  }

  @Test def ofCodePoint(): Unit = {
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.of(0x0000))
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.of(0x007f))
    assertEquals(UnicodeBlock.MEETEI_MAYEK_EXTENSIONS, UnicodeBlock.of(0xaae0))
    assertEquals(UnicodeBlock.MEETEI_MAYEK_EXTENSIONS, UnicodeBlock.of(0xaaff))
    assertEquals(UnicodeBlock.SPECIALS, UnicodeBlock.of(0xfff0))
    assertEquals(UnicodeBlock.SPECIALS, UnicodeBlock.of(0xffff))

    assertEquals(UnicodeBlock.LINEAR_B_SYLLABARY, UnicodeBlock.of(0x10000))
    assertEquals(UnicodeBlock.LINEAR_B_SYLLABARY, UnicodeBlock.of(0x1007f))
    assertEquals(UnicodeBlock.ANCIENT_GREEK_MUSICAL_NOTATION, UnicodeBlock.of(0x1d200))
    assertEquals(UnicodeBlock.ANCIENT_GREEK_MUSICAL_NOTATION, UnicodeBlock.of(0x1d24f))
    assertEquals(UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_B, UnicodeBlock.of(0x100000))
    assertEquals(UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_B, UnicodeBlock.of(0x10ffff))
  }


  @Test def forNameString(): Unit = {
    // scalastyle:off line.size.limit
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.forName("BASIC_LATIN"))
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.forName("Basic Latin"))
    assertEquals(UnicodeBlock.BASIC_LATIN, UnicodeBlock.forName("BasicLatin"))
    assertEquals(UnicodeBlock.MEETEI_MAYEK_EXTENSIONS, UnicodeBlock.forName("MEETEI_MAYEK_EXTENSIONS"))
    assertEquals(UnicodeBlock.MEETEI_MAYEK_EXTENSIONS, UnicodeBlock.forName("Meetei Mayek Extensions"))
    assertEquals(UnicodeBlock.MEETEI_MAYEK_EXTENSIONS, UnicodeBlock.forName("MeeteiMayekExtensions"))
    assertEquals(UnicodeBlock.SPECIALS, UnicodeBlock.forName("SPECIALS"))
    assertEquals(UnicodeBlock.SPECIALS, UnicodeBlock.forName("Specials"))

    assertEquals(UnicodeBlock.LINEAR_B_SYLLABARY, UnicodeBlock.forName("LINEAR_B_SYLLABARY"))
    assertEquals(UnicodeBlock.LINEAR_B_SYLLABARY, UnicodeBlock.forName("Linear B Syllabary"))
    assertEquals(UnicodeBlock.LINEAR_B_SYLLABARY, UnicodeBlock.forName("LinearBSyllabary"))
    assertEquals(UnicodeBlock.ANCIENT_GREEK_MUSICAL_NOTATION, UnicodeBlock.forName("ANCIENT_GREEK_MUSICAL_NOTATION"))
    assertEquals(UnicodeBlock.ANCIENT_GREEK_MUSICAL_NOTATION, UnicodeBlock.forName("Ancient Greek Musical Notation"))
    assertEquals(UnicodeBlock.ANCIENT_GREEK_MUSICAL_NOTATION, UnicodeBlock.forName("AncientGreekMusicalNotation"))
    assertEquals(UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_B, UnicodeBlock.forName("SUPPLEMENTARY_PRIVATE_USE_AREA_B"))
    assertEquals(UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_B, UnicodeBlock.forName("Supplementary Private Use Area-B"))
    assertEquals(UnicodeBlock.SUPPLEMENTARY_PRIVATE_USE_AREA_B, UnicodeBlock.forName("SupplementaryPrivateUseArea-B"))
    // scalastyle:on line.size.limit
  }
}
