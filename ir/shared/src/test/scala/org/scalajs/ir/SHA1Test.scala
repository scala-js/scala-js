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

import java.lang.Byte.parseByte

import org.junit.Test
import org.junit.Assert._

class SHA1Test {
  @Test def testVector1(): Unit = {
    val expected = "a9993e364706816aba3e25717850c26c9cd0d89d"
    val actual = computeSHA1Full("abc")
    assertEquals(expected, actual)
  }

  @Test def testVector2(): Unit = {
    val expected = "da39a3ee5e6b4b0d3255bfef95601890afd80709"
    val actual = computeSHA1Full("")
    assertEquals(expected, actual)
  }

  @Test def testVector3(): Unit = {
    val expected = "84983e441c3bd26ebaae4aa1f95129e5e54670f1"
    val actual = computeSHA1Full("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    assertEquals(expected, actual)
  }

  @Test def testVector4(): Unit = {
    val expected = "a49b2446a02c645bf419f995b67091253a04a259"
    val builder = new SHA1.DigestBuilder
    builder.update(string2bytes("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghij"))
    builder.update(string2bytes("klmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"))
    val actual = hashString(builder.finalizeDigest())
    assertEquals(expected, actual)
  }

  @Test def testVector5(): Unit = {
    val expected = "34aa973cd4c4daa4f61eeb2bdbad27316534016f"
    val actual = computeSHA1Full("a" * 1000000)
    assertEquals(expected, actual)
  }

  private def computeSHA1Full(input: String): String = {
    val builder = new SHA1.DigestBuilder
    builder.update(string2bytes(input))
    hashString(builder.finalizeDigest())
  }

  private def string2bytes(s: String): Array[Byte] =
    s.toCharArray().map(_.toByte)

  private def hashString(hash: Array[Byte]): String =
    hash.map(b => "%02x".format(b & 0xff)).mkString
}
