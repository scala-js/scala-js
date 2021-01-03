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

import scala.language.implicitConversions

import org.junit.Test
import org.junit.Assert._

import Hashers._
import Names._
import OriginalName.NoOriginalName
import Printers._
import Trees._
import Types._

import TestIRBuilder._

class HashersTest {
  private def assertHashEquals(expected: String, actual: Option[TreeHash]): Unit = {
    assertTrue(actual.isDefined)
    assertEquals(expected, hashAsVersion(actual.get))
  }

  @Test def testHashAsVersion(): Unit = {
    val hash: TreeHash = new TreeHash(Array(
        0x10, 0x32, 0x54, 0x76, 0x98, 0xba, 0xdc, 0xfe, 0xc3, 0x7f,
        0x01, 0x23, 0x45, 0x67, 0x89, 0xab, 0xcd, 0xef, 0xbb, 0x34
    ).map(_.toByte))

    assertEquals("1032547698badcfec37f0123456789abcdefbb34", hashAsVersion(hash))
  }

  private val bodyWithInterestingStuff = Block(
      // All primitive literals, which exercise hashing of primitives
      BooleanLiteral(true),
      CharLiteral('A'),
      ByteLiteral(12),
      ShortLiteral(12345),
      IntLiteral(1234567890),
      LongLiteral(123456789101112L),
      FloatLiteral(151.189f),
      DoubleLiteral(151.189),

      /* String literals, which exercise hashing strings, and, underlying
       * that, hashing part of an Array[Byte], and hashing more than 64 bytes
       * at a time, forcing decomposition in 64-byte chunks.
       */
      s(""),
      s("hello"),
      s("wPtOG7TtwcP1Z3gBgUzm"),
      s("JEKzMO5kLpv7ZBu5FcSdIZngrMJTmZz90siAAxC7YCkBVp9M2DJRuI8jE278zRzjlvqC8syqM5G8Ujob"),
      s(
          "hU9TP2tpK0AQGyccLKotncR7PafADrjb1731xzvcp0MXKfcAQYnPniUUYphqwwj5LEt74QwSssGWh59q" +
          "dBifWTbHqgXAncHzMqTU07g4Pj6BaYmGAsMxeC9IRgiKfMSOFpLyrXFz7zsIRhywapYjXV"
      ),

      // A var ref that contains a Name, which exercises hashing an Array[Byte]
      ref("x", IntType),

      // Result value of type int, for consistency
      i(5)
  )

  @Test def testHashMethodDef(): Unit = {
    def test(expected: String, methodDef: MethodDef): Unit = {
      val hashedMethodDef = hashMethodDef(methodDef)
      assertHashEquals(expected, hashedMethodDef.hash)
    }

    val mIIMethodName = MethodName("m", List(I), I)

    test(
        "7da97841c609c48c668003895af8afe26c2b006f",
        MethodDef(MemberFlags.empty, mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false, rest = false)),
            IntType, None)(
            NoOptHints, None)
    )

    test(
        "50af8a6d5ee2ae3fa53beeb20692a67a749ec864",
        MethodDef(MemberFlags.empty, mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false, rest = false)),
            IntType, Some(bodyWithInterestingStuff))(
            NoOptHints, None)
    )
  }

  @Test def testHashJSMethodDef(): Unit = {
    def test(expected: String, methodDef: JSMethodDef): Unit = {
      val hashedMethodDef = hashJSMethodDef(methodDef)
      assertHashEquals(expected, hashedMethodDef.hash)
    }

    test(
        "c2e625c9d70272163025b30c29523c331dc9032e",
        JSMethodDef(MemberFlags.empty, s("m"),
            List(ParamDef("x", NON, AnyType, mutable = false, rest = false)),
            bodyWithInterestingStuff)(
            NoOptHints, None)
    )
  }

}
