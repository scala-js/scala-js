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

package org.scalajs.linker.backend.emitter

import org.junit.Test
import org.junit.Assert._

class NameCompressorTest {
  @Test def testNameGenerator(): Unit = {
    // all the one-letter strings
    val letterStrings = (('a' to 'z') ++ ('A' to 'Z')).map(_.toString())

    // all the one-letter-or-digit strings
    val letterOrDigitStrings = ('0' to '9').map(_.toString()) ++ letterStrings

    val expectedOneCharIdents = letterStrings

    val expectedTwoCharIdents = for {
      firstChar <- letterStrings
      secondChar <- letterOrDigitStrings
      ident = firstChar + secondChar
      if ident != "do" && ident != "if" && ident != "in" // reserved JS identifiers that will be avoided
    } yield {
      ident
    }

    val firstFewExpectedThreeCharIdents = {
      letterOrDigitStrings.map("a0" + _) ++
      letterOrDigitStrings.map("a1" + _)
    }

    val expectedSequenceStart =
      expectedOneCharIdents ++ expectedTwoCharIdents ++ firstFewExpectedThreeCharIdents

    // Now actually test

    val namesToAvoid = NameGen.ReservedJSIdentifierNames
    val generator = new NameCompressor.NameGenerator(namesToAvoid)

    for (expected <- expectedSequenceStart)
      assertEquals(expected, generator.nextString())
  }
}
