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

package org.scalajs.testsuite.javalib.io

import java.io._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows.{assertThrows, assertThrowsNPEIfCompliant}

class FilterReaderTest {
  // use StringReader as delegate
  val str = "asdf"
  def newFilterReader: FilterReader = new FilterReader(new StringReader(str)) {}

  @Test def nullCtorArgThrows(): Unit = {
    assertThrowsNPEIfCompliant(new FilterReader(null) {})
  }

  // test delegation
  @Test def close(): Unit = {
    val fr = newFilterReader

    fr.close()
    fr.close() // multiple is fine
    assertThrows(classOf[IOException], fr.read())
  }

  @Test def markSupported(): Unit = {
    assertTrue(newFilterReader.markSupported)
  }

  @Test def read(): Unit = {
    val r = newFilterReader

    for (c <- str) {
      assertEquals(c, r.read().toChar)
    }
    assertEquals(-1, r.read())
  }
}
