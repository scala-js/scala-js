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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.testsuite.utils.AssertThrows._

class CharacterTestOnJDK11 {

  @Test def toStringCodePoint(): Unit = {
    assertEquals("\u0000", Character.toString(0))
    assertEquals("\u04D2", Character.toString(1234))
    assertEquals("\uD845", Character.toString(0xd845))
    assertEquals("\uDC54", Character.toString(0xdc54))
    assertEquals("\uFFFF", Character.toString(0xffff))

    assertEquals("\uD800\uDC00", Character.toString(0x10000))
    assertEquals("\uD808\uDF45", Character.toString(0x12345))
    assertEquals("\uDBFF\uDFFF", Character.toString(0x10ffff))

    assertThrows(classOf[IllegalArgumentException], Character.toString(0x110000))
    assertThrows(classOf[IllegalArgumentException], Character.toString(0x234567))
    assertThrows(classOf[IllegalArgumentException], Character.toString(-1))
    assertThrows(classOf[IllegalArgumentException], Character.toString(Int.MinValue))
  }

}
