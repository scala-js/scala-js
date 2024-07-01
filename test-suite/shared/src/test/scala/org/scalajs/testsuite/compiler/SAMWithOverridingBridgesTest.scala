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

package org.scalajs.testsuite.compiler

import org.junit.Test
import org.junit.Assert._

/* Issue: https://github.com/scala/bug/issues/10512
 * Test cases taken from: https://github.com/scala/scala/pull/6087
 */
class SAMWithOverridingBridgesTest {
  import SAMWithOverridingBridgesTest._

  @Test def testVariantA(): Unit = {
    import VariantA._
    import JsonEncoderInstances._

    implicitly[JsonEncoder[List[String]]].encode("" :: Nil)
  }

  @Test def testVariantB(): Unit = {
    import VariantB._

    val it = new It

    val s1: SAM_A = () => it
    val s2: SAM_A1 = () => it
    val s3: SAM_B = () => it
    val s4: SAM_B1 = () => it
    val s5: SAM_B2 = () => it
    val s6: SAM_C = () => it
    val s7: SAM_F = () => it
    val s8: SAM_F1 = () => it

    (s1(): A)

    (s2(): A)

    (s3(): B)
    (s3(): A)

    (s4(): B)
    (s4(): A)

    (s5(): B)
    (s5(): A)

    (s6(): C)
    (s6(): B)
    (s6(): A)

    (s7(): C)
    (s7(): B)
    (s7(): A)

    (s8(): C)
    (s8(): B)
    (s8(): A)
  }
}

object SAMWithOverridingBridgesTest {
  object VariantA {
    trait JsonValue
    class JsonObject extends JsonValue
    class JsonString extends JsonValue

    trait JsonEncoder[A] {
      def encode(value: A): JsonValue
    }

    trait JsonObjectEncoder[A] extends JsonEncoder[A] {
      def encode(value: A): JsonObject
    }

    object JsonEncoderInstances {
      implicit val stringEncoder: JsonEncoder[String] = {
        s => new JsonString
        // new JsonEncoder[String] {
        //   def encode(value: String): JsonString = new JsonString
        // }
      }

      implicit def listEncoder[A](
          implicit encoder: JsonEncoder[A]): JsonObjectEncoder[List[A]] = {
        l => new JsonObject
        // new JsonObjectEncoder[List[A]] {
        //   def encode(value: List[A]): JsonObject = new JsonObject
        // }
      }
    }
  }

  object VariantB {
    trait A
    trait B extends A
    trait C extends B
    class It extends C

    /* try as many weird diamondy things as I can think of */

    trait SAM_A {
      def apply(): A
    }

    trait SAM_A1 extends SAM_A {
      def apply(): A
    }

    trait SAM_B extends SAM_A1 {
      def apply(): B
    }

    trait SAM_B1 extends SAM_A1 {
      def apply(): B
    }

    trait SAM_B2 extends SAM_B with SAM_B1

    trait SAM_C extends SAM_B2 {
      def apply(): C
    }

    trait SAM_F extends (() => A) with SAM_C

    trait SAM_F1 extends (() => C) with SAM_F
  }
}
