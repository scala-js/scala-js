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

import java.io.ByteArrayOutputStream

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
  private def assertHashEquals(expected: String, actual: Version): Unit = {
    assertTrue(actual.isHash)

    val actualBytes = {
      val out = new ByteArrayOutputStream
      actual.writeHash(out)
      out.close()
      out.toByteArray()
    }

    val expectedBytes = expected.grouped(2)
      .map(Integer.parseInt(_, 16).toByte).toArray

    assertArrayEquals(expectedBytes, actualBytes)
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
      assertHashEquals(expected, hashedMethodDef.version)
    }

    val mIIMethodName = MethodName("m", List(I), I)

    test(
        "64940df7c6aae58962eb56f4aa6c6b085ca06c25",
        MethodDef(MemberFlags.empty, mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, None)(
            NoOptHints, UNV)
    )

    test(
        "309805e5680ffa1804811ff5c9ebc77e91846957",
        MethodDef(MemberFlags.empty, mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, Some(bodyWithInterestingStuff))(
            NoOptHints, UNV)
    )
  }

  @Test def testHashJSMethodDef(): Unit = {
    def test(expected: String, methodDef: JSMethodDef): Unit = {
      val hashedMethodDef = hashJSMethodDef(methodDef)
      assertHashEquals(expected, hashedMethodDef.version)
    }

    test(
        "c0f1ef1b22fd1cfdc9bba78bf3e0f433e9f82fc1",
        JSMethodDef(MemberFlags.empty, s("m"),
            List(ParamDef("x", NON, AnyType, mutable = false)), None,
            bodyWithInterestingStuff)(
            NoOptHints, UNV)
    )
  }

}
