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

package org.scalajs.linker.interface

import org.junit.Test
import org.junit.Assert._
import org.junit.function.ThrowingRunnable

class StandardConfigTest {
  @Test
  def jsHeader(): Unit = {
    def testValid(header: String): Unit = {
      assertTrue(header, StandardConfig.isValidJSHeader(header))
      assertEquals(header, StandardConfig().withJSHeader(header).jsHeader)
    }

    def testInvalid(header: String): Unit = {
      assertFalse(header, StandardConfig.isValidJSHeader(header))
      assertThrows(classOf[IllegalArgumentException], new ThrowingRunnable {
        def run(): Unit = StandardConfig().withJSHeader(header)
      })
    }

    // Valid whitespace
    testValid("")
    testValid("  \n")
    testValid("  \t  \n")
    testValid("\ufeff  \n")
    testValid("  \u00a0 \n")
    testValid("  \u3000  \n")
    testValid("  \n \t \n")

    // Missing \n at the end of whitespace
    testInvalid("  ")
    testInvalid("\t")
    testInvalid("\ufeff")

    // Invalid tokens
    testInvalid("foo\n")
    testInvalid("/a\n")
    testInvalid("/\n")
    testInvalid("/")
    testInvalid("α") // U+03B1 α Greek Small Letter Alpha
    testInvalid("\uD834\uDD1E") // U+1D11E 𝄞 Musical Symbol G Clef
    testInvalid("\uD834")
    testInvalid("\uDD1E")

    // Invalid newlines outside comments
    testInvalid("  \r  \n")
    testInvalid("  \r\n  \n")
    testInvalid("  \u000B  \n")
    testInvalid("  \u000C  \n")
    testInvalid("  \u2028  \n")
    testInvalid("  \u2029  \n")

    // Invalid newlines inside comments
    testInvalid("/*  \r  */\n")
    testInvalid("/*  \r\n  */\n")
    testInvalid("/*  \u000B  */\n")
    testInvalid("/*  \u000C  */\n")
    testInvalid("/*  \u2028  */\n")
    testInvalid("/*  \u2029  */\n")

    // Valid single-line comments
    testValid("//\n")
    testValid("// foo\n")
    testValid("// foo\tbar\n")
    testValid("// one\n// two\n")
    testValid("  \t // foo\n")
    testValid("// α\n") // U+03B1 α Greek Small Letter Alpha
    testValid("// \uD834\uDD1E\n") // U+1D11E 𝄞 Musical Symbol G Clef

    // Invalid single-line comments
    testInvalid("//")
    testInvalid("// foo")
    testInvalid("// foo\nbar\n")
    testInvalid("// \uD834\n")
    testInvalid("// \uDD1E\n")

    // Valid multi-line comments
    testValid("/**/\n")
    testValid("/* foo bar */\n")
    testValid("/* foo\n * bar */\n")
    testValid("/* α */\n") // U+03B1 α Greek Small Letter Alpha
    testValid("/* \uD834\uDD1E */\n") // U+1D11E 𝄞 Musical Symbol G Clef

    // Invalid multi-line comments
    testInvalid("/**/")
    testInvalid("/*\n")
    testInvalid("/*")
    testInvalid("/* foo\n")
    testInvalid("/* /* foo */ */\n") // multi-line comments do not nest
    testInvalid("/*/\n")
    testInvalid("/* \uD834 */\n")
    testInvalid("/* \uDD1E */\n")

    // Valid combination
    testValid("  // foo\n\t/* foo bar\nbaz hello\n*/\n/**///foo\n")

    // Invalid combination
    testInvalid(" ! // foo\n\t/* foo bar\nbaz hello\n*/\n/**///foo\n")
    testInvalid("  // foo\n!\t/* foo bar\nbaz hello\n*/\n/**///foo\n")
    testInvalid("  // foo\n\t/* foo bar\nbaz hello\n*/!\n/**///foo\n")
    testInvalid("  // foo\n\t/* foo bar\nbaz hello\n*/\n/**/!//foo\n")
    testInvalid("  // foo\n\t/* foo bar\nbaz hello\n*/\n/**//!/foo\n")
    testInvalid("  // foo\n\t/* foo bar\nbaz hello\n*/\n/**///foo\n!")
  }
}
