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
import org.junit.Assume._

import org.scalajs.testsuite.utils.AssertThrows.assertThrows
import org.scalajs.testsuite.utils.Platform

class OutputStreamTestOnJDK11 {
  @Test def nullOutputStream(): Unit = {
    val stream = OutputStream.nullOutputStream()

    stream.write(1)
    stream.write(new Array[Byte](2))
    stream.write(new Array[Byte](2), 0, 1)

    stream.close()
    stream.close() // shouldn't throw

    assertThrows(classOf[IOException], stream.write(1))
    assertThrows(classOf[IOException], stream.write(new Array[Byte](0)))
    assertThrows(classOf[IOException], stream.write(new Array[Byte](0), 0, 0))

    stream.flush() // shouldn't throw
  }
}
