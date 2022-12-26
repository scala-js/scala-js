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

package org.scalajs.ir

import java.io.ByteArrayOutputStream

import org.junit.Test
import org.junit.Assert._

class VersionTest {
  import Version._

  private def testEq(x: Version, y: Version) = {
    assertTrue(x.sameVersion(y))
    assertTrue(y.sameVersion(x))
  }

  private def testNe(x: Version, y: Version) = {
    assertFalse(x.sameVersion(y))
    assertFalse(y.sameVersion(x))
  }

  @Test
  def testUnversioned(): Unit = {
    testNe(Unversioned, Unversioned)
    testNe(Unversioned, fromInt(1))
    testNe(Unversioned, fromLong(1L))
    testNe(Unversioned, fromBytes(new Array(2)))
    testNe(Unversioned, fromHash(new Array(20)))
    testNe(Unversioned, combine(fromInt(1), fromInt(2)))
  }

  @Test
  def testFromHash(): Unit = {
    val v = fromHash(Array.fill(20)(0))

    testEq(v, fromHash(Array.fill(20)(0)))
    testNe(v, fromHash(Array.fill(20)(1)))
  }

  @Test
  def testFromBytes(): Unit = {
    val v = fromBytes(Array(1))

    testEq(v, fromBytes(Array(1)))
    testNe(v, fromBytes(Array(2)))
    testNe(v, fromBytes(Array(1, 2)))
    testNe(v, fromBytes(Array()))
  }

  @Test
  def testFromInt(): Unit = {
    val v = fromInt(2)

    testEq(v, fromInt(2))
    testEq(v, fromBytes(Array(0, 0, 0, 2)))
    testNe(v, fromInt(3))
    testNe(v, fromBytes(Array(0)))
  }

  @Test
  def testFromLong(): Unit = {
    val v = fromLong(2L)

    testEq(v, fromLong(2L))
    testEq(v, fromBytes(Array[Byte](0, 0, 0, 0, 0, 0, 0, 2)))
    testNe(v, fromLong(3L))
    testNe(v, fromInt(2))
    testNe(v, fromBytes(Array[Byte](0)))
  }

  @Test
  def testCombine(): Unit = {
    val v = combine(fromBytes(Array(1)), fromBytes(Array(2)))

    testEq(v, combine(fromBytes(Array(1)), fromBytes(Array(2))))
    testNe(v, fromBytes(Array(1, 2)))
    testNe(v, combine())
    testNe(v, combine(fromBytes(Array(1))))

    testEq(combine(), combine())
  }

  @Test
  def testKinds(): Unit = {
    // Hash doesn't equal ephemeral.
    testNe(fromHash(Array.fill(20)(1)), fromBytes(Array.fill(20)(1)))

    // Combined doesn't equal hash or ephemeral
    val v = combine(fromBytes(Array.fill(11)(0)))

    // Internal representation of combined of the above.
    // (length 20, so it could be a hash).
    val a = Array[Byte](
        0, 0, 0, 1, // number of versions
        0, 0, 0, 12, // length of the version
        0x02, // type of the version (ephemeral)
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 // payload of the version
    )

    testNe(v, fromHash(a))
    testNe(v, fromBytes(a))
  }

  @Test
  def testIsHash(): Unit = {
    assertFalse(Unversioned.isHash)
    assertFalse(fromBytes(Array()).isHash)
    assertFalse(combine().isHash)
    assertTrue(fromHash(Array.fill(20)(0)).isHash)
    assertFalse(combine(fromHash(Array.fill(20)(0))).isHash)
  }

  @Test
  def testWriteHash(): Unit = {
    val out = new ByteArrayOutputStream

    fromHash(Array.fill(20)(1)).writeHash(out)

    assertArrayEquals(Array.fill[Byte](20)(1), out.toByteArray())
  }
}
