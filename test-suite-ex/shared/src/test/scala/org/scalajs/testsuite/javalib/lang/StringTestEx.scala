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

import java.util.Locale

import org.junit.Test
import org.junit.Assert._

/** Additional tests for java.lang.String that require `java.util.Locale`. */
class StringTestEx {
  val English = new Locale("en")
  val Lithuanian = new Locale("lt")
  val Turkish = new Locale("tr")
  val Azeri = new Locale("Az") // randomly test the lowercase normalization

  @Test def testToLowerCaseWithLocale(): Unit = {
    assertEquals("title", "TITLE".toLowerCase(English))
    assertEquals("title", "TITLE".toLowerCase(Lithuanian))
    assertEquals("tıtle", "TITLE".toLowerCase(Turkish))
    assertEquals("tıtle", "TITLE".toLowerCase(Azeri))

    // Incurving fencer sword sparkled and perforated a round watermelon
    assertEquals("įlinkdama fechtuotojo špaga sublykčiojusi pragręžė apvalų arbūzą",
        "ĮLINKDAMA FECHTUOTOJO ŠPAGA SUBLYKČIOJUSI PRAGRĘŽĖ APVALŲ ARBŪZĄ".toLowerCase(Lithuanian))

    // Patient with pajamas, trusted swarthy driver quickly
    assertEquals("pijamalı hasta, yağız şoföre çabucak güvendi.",
        "PİJAMALI HASTA, YAĞIZ ŞOFÖRE ÇABUCAK GÜVENDİ.".toLowerCase(Turkish))
    // same, with combining marks for the dotted Is
    assertEquals("pijamalı hasta, yağız şoföre çabucak güvendi.",
        "PI\u0307JAMALI HASTA, YAĞIZ ŞOFÖRE ÇABUCAK GÜVENDI\u0307.".toLowerCase(Turkish))

    assertEquals("iíìĩi\u0307",
        "IÍÌĨİ".toLowerCase(English))
    assertEquals("ıíìĩi",
        "IÍÌĨİ".toLowerCase(Turkish))
    assertEquals("ıíìĩi",
        "IÍÌĨİ".toLowerCase(Azeri))
    assertEquals("ii\u0307\u0301i\u0307\u0300i\u0307\u0303i\u0307",
        "IÍÌĨİ".toLowerCase(Lithuanian))
  }

  @Test def testToLowerCaseWithLocaleCornerCasesForLithuanian(): Unit = {
    // All the characters with an unconditional special translation
    assertEquals("i\u0307\u0300 i\u0307\u0300a\u033D",
        "\u00CC \u00CCA\u033D".toLowerCase(Lithuanian))
    assertEquals("i\u0307\u0301 i\u0307\u0301a\u033D",
        "\u00CD \u00CDA\u033D".toLowerCase(Lithuanian))
    assertEquals("i\u0307\u0303 i\u0307\u0303a\u033D",
        "\u0128 \u0128A\u033D".toLowerCase(Lithuanian))

    // All the characters with a special translation with More_Above
    assertEquals("i\u0307\u033D ia\u033D",
        "I\u033D IA\u033D".toLowerCase(Lithuanian))
    assertEquals("j\u0307\u033D ja\u033D",
        "J\u033D JA\u033D".toLowerCase(Lithuanian))
    assertEquals("\u012F\u0307\u033D \u012Fa\u033D",
        "\u012E\u033D \u012EA\u033D".toLowerCase(Lithuanian))

    // Can put another combining mark before the Above
    assertEquals("i\u0307\u0315\u033D i\u0315a\u033D",
        "I\u0315\u033D I\u0315A\u033D".toLowerCase(Lithuanian))
    assertEquals("i\u0307\uD834\uDD7C\u033D i\uD834\uDD7Ca\u033D",
        "I\uD834\uDD7C\u033D I\uD834\uDD7CA\u033D".toLowerCase(Lithuanian)) // 1D17C

    // But combining marks with combining class 0 will cut the link
    assertEquals("i\u034f\u033D",
        "I\u034f\u033D".toLowerCase(Lithuanian))
    assertEquals("i\uD804\uDC00\u033D",
        "I\uD804\uDC00\u033D".toLowerCase(Lithuanian)) // 11000
  }

  @Test def testToLowerCaseWithLocaleCornerCasesForTurkishAndAzeri(): Unit = {
    // Can put another combining mark between I and 0307, as long as its class is not Above
    assertEquals("i\u0315",
        "I\u0315\u0307".toLowerCase(Turkish))
    assertEquals("i\uD834\uDD7C",
        "I\uD834\uDD7C\u0307".toLowerCase(Turkish)) // 1D17C

    // But other Above combining marks will cut the link
    assertEquals("ı\u033D\u0307",
        "I\u033D\u0307".toLowerCase(Turkish))
    assertEquals("ı\uD834\uDD85\u0307",
        "I\uD834\uDD85\u0307".toLowerCase(Turkish)) // 1D185

    // Even combining marks with combining class 0 will cut the link
    assertEquals("ı\u034f\u0307",
        "I\u034f\u0307".toLowerCase(Turkish))
    assertEquals("ı\uD804\uDC00\u0307",
        "I\uD804\uDC00\u0307".toLowerCase(Turkish)) // 11000
  }

  @Test def testToUpperCaseWithLocale(): Unit = {
    // Incurving fencer sword sparkled and perforated a round watermelon
    assertEquals("ĮLINKDAMA FECHTUOTOJO ŠPAGA SUBLYKČIOJUSI PRAGRĘŽĖ APVALŲ ARBŪZĄ",
        "įlinkdama fechtuotojo špaga sublykčiojusi pragręžė apvalų arbūzą".toUpperCase(Lithuanian))

    // Patient with pajamas, trusted swarthy driver quickly
    assertEquals("PİJAMALI HASTA, YAĞIZ ŞOFÖRE ÇABUCAK GÜVENDİ.",
        "pijamalı hasta, yağız şoföre çabucak güvendi.".toUpperCase(Turkish))

    assertEquals("IÍÌĨI I\u0307\u0301I\u0307\u0300I\u0307\u0303I\u0307",
        "iíìĩı i\u0307\u0301i\u0307\u0300i\u0307\u0303i\u0307".toUpperCase(English))
    assertEquals("İÍÌĨI İ\u0307\u0301İ\u0307\u0300İ\u0307\u0303İ\u0307",
        "iíìĩı i\u0307\u0301i\u0307\u0300i\u0307\u0303i\u0307".toUpperCase(Turkish))
    assertEquals("İÍÌĨI İ\u0307\u0301İ\u0307\u0300İ\u0307\u0303İ\u0307",
        "iíìĩı i\u0307\u0301i\u0307\u0300i\u0307\u0303i\u0307".toUpperCase(Azeri))
    assertEquals("IÍÌĨI I\u0301I\u0300I\u0303I",
        "iíìĩı i\u0307\u0301i\u0307\u0300i\u0307\u0303i\u0307".toUpperCase(Lithuanian))
  }

  @Test def testToUpperCaseWithLocaleCornerCasesForLithuanian(): Unit = {
    // More characters with the Soft_Dotted property
    assertEquals("J\u0301J\u0300J\u0303J",
        "j\u0307\u0301j\u0307\u0300j\u0307\u0303j\u0307".toUpperCase(Lithuanian))
    assertEquals("\u1E2C\u0301\u1E2C\u0300\u1E2C\u0303\u1E2C",
        "\u1E2D\u0307\u0301\u1E2D\u0307\u0300\u1E2D\u0307\u0303\u1E2D\u0307".toUpperCase(Lithuanian))

    // This does not seem compliant to the spec to me, but that's what the JVM gives
    assertEquals("\u1D96\u0307\u0301\u1D96\u0307\u0300\u1D96\u0307\u0303\u1D96\u0307",
        "\u1D96\u0307\u0301\u1D96\u0307\u0300\u1D96\u0307\u0303\u1D96\u0307".toUpperCase(Lithuanian))

    // Can put another combining mark before the 0307, as long as its class is not Above
    assertEquals("I\u0315\u0301",
        "i\u0315\u0307\u0301".toUpperCase(Lithuanian))
    assertEquals("I\u031A\u0301",
        "i\u031A\u0307\u0301".toUpperCase(Lithuanian))
    assertEquals("I\u033C\u0301",
        "i\u033C\u0307\u0301".toUpperCase(Lithuanian))
    assertEquals("I\uD834\uDD7C\u0301",
        "i\uD834\uDD7C\u0307\u0301".toUpperCase(Lithuanian)) // 1D17C

    // But other Above combining marks will cut the link
    assertEquals("I\u033D\u0307\u0301",
        "i\u033D\u0307\u0301".toUpperCase(Lithuanian))
    assertEquals("I\u0346\u0307\u0301",
        "i\u0346\u0307\u0301".toUpperCase(Lithuanian))
    assertEquals("I\uD834\uDD85\u0307\u0301",
        "i\uD834\uDD85\u0307\u0301".toUpperCase(Lithuanian)) // 1D185

    // Even combining marks with combining class 0 will cut the link
    assertEquals("I\u034f\u0307\u0301",
        "i\u034f\u0307\u0301".toUpperCase(Lithuanian))
    assertEquals("I\uD804\uDC00\u0307\u0301",
        "i\uD804\uDC00\u0307\u0301".toUpperCase(Lithuanian)) // 1D185
  }
}
