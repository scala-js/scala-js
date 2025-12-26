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

package org.scalajs.linker.frontend.optimizer

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.testutils.TestIRBuilder._
import org.scalajs.linker.testutils.TreeDSL._

import IntegerDivisions._
import IntegerDivisions.UnsignedIntegral._

/** Tests for the specific shapes of trees produced for selected divisors.
 *
 *  These tests notably ensures that the best path is taken for each category.
 *  For example, that we don't produce the generic path for powers of 2.
 *
 *  We do not test specific shapes for Long operations.
 *  Since the same, generic logic is used for both Int and Long, it is not
 *  worth it. Testing Long operations would require a different set of TreeDSL
 *  helpers for operations on Longs, which would be inconvenient.
 */
class IntegerDivisionsTreeShapeTest {
  /* For tests, we always use the configuration without RuntimeLong. This
   * configuration produces code that is easier to interpret.
   *
   * For the details of the RuntimeLong-specific generated code, we rely on the
   * test suite.
   */
  val integerDivisions = new IntegerDivisions(useRuntimeLong = false)

  /** Main test helper. */
  private def testSpecificShape(op: BinaryOp.Code, divisor: Int)(expectedTree: Tree): Unit = {
    val actual = integerDivisions.makeOptimizedDivision(op, divisor)
    if (actual != expectedTree)
      fail(s"Expected:\n${expectedTree.show}\nbut got\n${actual.show}")
  }

  // Some shortcuts for subshapes that appear very often

  private val inum = VarRef(NumeratorArgName)(IntType)

  private def tVarDef(rhs: Tree): VarDef =
    VarDef(LocalIdent(tName), NON, IntType, mutable = false, rhs)

  private def hiVarDef(rhs: Tree): VarDef =
    VarDef(LocalIdent(hiName), NON, IntType, mutable = false, rhs)

  /** Tree shape for getting the `hi` word of a 32x32->64 signed multiplication.
   *
   *  We're reusing `IntIsUnsignedIntegral.genMulSignedHi` here, which is
   *  technically code under test. It's OK because for the `Int` case, that
   *  function is a straightforward factory.
   */
  private def genIntMulSignedHi(x: Int, y: VarRef): Tree =
    IntIsUnsignedIntegral.genMulSignedHi(x, y, useRuntimeLong = false)

  /** Tree shape for getting the `hi` word of a 32x32->64 unsigned multiplication.
   *
   *  See the comment in `genIntMulSignedHi`.
   */
  private def genIntMulUnsignedHi(x: Int, y: VarRef): Tree =
    IntIsUnsignedIntegral.genMulUnsignedHi(x, y)

  // The tests

  @Test def testSpecificShape_Int_/(): Unit = {
    def test(divisor: Int)(expectedTree: Tree): Unit =
      testSpecificShape(BinaryOp.Int_/, divisor)(expectedTree)

    test(1) {
      inum
    }

    test(-1) {
      -inum
    }

    test(2) {
      (inum + ((inum >> 0) >>> 31)) >> 1
    }

    test(-2) {
      -((inum + ((inum >> 0) >>> 31)) >> 1)
    }

    test(8) {
      (inum + ((inum >> 2) >>> 29)) >> 3
    }

    test(-8) {
      -((inum + ((inum >> 2) >>> 29)) >> 3)
    }

    test(Int.MinValue) {
      -((inum + ((inum >> 30) >>> 1)) >> 31)
    }

    test(3) {
      (genIntMulSignedHi(0x55555556, inum) >> 0) + (inum >>> 31)
    }

    test(-3) {
      val t = tVarDef((genIntMulSignedHi(0x55555555, inum) - inum) >> 1)
      Block(
        t,
        t.ref + (t.ref >>> 31)
      )
    }

    test(5) {
      (genIntMulSignedHi(0x66666667, inum) >> 1) + (inum >>> 31)
    }

    test(-5) {
      val t = tVarDef(genIntMulSignedHi(0x99999999, inum) >> 1)
      Block(
        t,
        t.ref + (t.ref >>> 31)
      )
    }

    test(7) {
      ((genIntMulSignedHi(0x92492493, inum) + inum) >> 2) + (inum >>> 31)
    }

    test(-7) {
      val t = tVarDef((genIntMulSignedHi(0x6db6db6d, inum) - inum) >> 2)
      Block(
        t,
        t.ref + (t.ref >>> 31)
      )
    }
  }

  @Test def testSpecificShape_Int_%(): Unit = {
    def test(divisor: Int)(expectedTree: Tree): Unit =
      testSpecificShape(BinaryOp.Int_%, divisor)(expectedTree)

    test(1) {
      int(0)
    }

    test(-1) {
      int(0)
    }

    test(2) {
      val t = tVarDef((inum >> 0) >>> 31)
      Block(
        t,
        ((inum + t.ref) & 1) - t.ref
      )
    }

    test(-2) {
      val t = tVarDef((inum >> 0) >>> 31)
      Block(
        t,
        ((inum + t.ref) & 1) - t.ref
      )
    }

    test(8) {
      val t = tVarDef((inum >> 2) >>> 29)
      Block(
        t,
        ((inum + t.ref) & 7) - t.ref
      )
    }

    test(-8) {
      val t = tVarDef((inum >> 2) >>> 29)
      Block(
        t,
        ((inum + t.ref) & 7) - t.ref
      )
    }

    test(Int.MinValue) {
      val t = tVarDef((inum >> 30) >>> 1)
      Block(
        t,
        ((inum + t.ref) & Int.MaxValue) - t.ref
      )
    }

    test(3) {
      inum - int(3) * {
        (genIntMulSignedHi(0x55555556, inum) >> 0) + (inum >>> 31)
      }
    }

    test(-3) {
      inum - int(-3) * {
        val t = tVarDef((genIntMulSignedHi(0x55555555, inum) - inum) >> 1)
        Block(
          t,
          t.ref + (t.ref >>> 31)
        )
      }
    }

    test(5) {
      inum - int(5) * {
        (genIntMulSignedHi(0x66666667, inum) >> 1) + (inum >>> 31)
      }
    }

    test(-5) {
      inum - int(-5) * {
        val t = tVarDef(genIntMulSignedHi(0x99999999, inum) >> 1)
        Block(
          t,
          t.ref + (t.ref >>> 31)
        )
      }
    }

    test(7) {
      inum - int(7) * {
        ((genIntMulSignedHi(0x92492493, inum) + inum) >> 2) + (inum >>> 31)
      }
    }

    test(-7) {
      inum - int(-7) * {
        val t = tVarDef((genIntMulSignedHi(0x6db6db6d, inum) - inum) >> 2)
        Block(
          t,
          t.ref + (t.ref >>> 31)
        )
      }
    }
  }

  @Test def testSpecificShape_Int_unsigned_/(): Unit = {
    def test(divisor: Int)(expectedTree: Tree): Unit =
      testSpecificShape(BinaryOp.Int_unsigned_/, divisor)(expectedTree)

    test(1) {
      inum
    }

    test(2) {
      inum >>> 1
    }

    test(8) {
      inum >>> 3
    }

    test(Int.MinValue) {
      inum >>> 31
    }

    test(3) {
      genIntMulUnsignedHi(0xaaaaaaab, inum) >>> 1
    }

    test(5) {
      genIntMulUnsignedHi(0xcccccccd, inum) >>> 2
    }

    test(-7) {
      genIntMulUnsignedHi(0x20000001, inum) >>> 29
    }

    test(-5) {
      genIntMulUnsignedHi(0x80000003, inum) >>> 31
    }

    test(-1) {
      genIntMulUnsignedHi(0x80000001, inum) >>> 31
    }

    test(7) {
      val hi = hiVarDef(genIntMulUnsignedHi(0x24924925, inum))
      Block(
        hi,
        (((inum - hi.ref) >>> 1) + hi.ref) >>> 2
      )
    }

    test(-8) {
      val hi = hiVarDef(genIntMulUnsignedHi(0x00000009, inum))
      Block(
        hi,
        (((inum - hi.ref) >>> 1) + hi.ref) >>> 31
      )
    }
  }

  @Test def testSpecificShape_Int_unsigned_%(): Unit = {
    def test(divisor: Int)(expectedTree: Tree): Unit =
      testSpecificShape(BinaryOp.Int_unsigned_%, divisor)(expectedTree)

    test(1) {
      int(0)
    }

    test(2) {
      inum & 1
    }

    test(8) {
      inum & 7
    }

    test(Int.MinValue) {
      inum & Int.MaxValue
    }

    test(3) {
      inum - int(3) * (genIntMulUnsignedHi(0xaaaaaaab, inum) >>> 1)
    }

    test(5) {
      inum - int(5) * (genIntMulUnsignedHi(0xcccccccd, inum) >>> 2)
    }

    test(-7) {
      inum - int(-7) * (genIntMulUnsignedHi(0x20000001, inum) >>> 29)
    }

    test(-5) {
      inum - int(-5) * (genIntMulUnsignedHi(0x80000003, inum) >>> 31)
    }

    test(-1) {
      inum - int(-1) * (genIntMulUnsignedHi(0x80000001, inum) >>> 31)
    }

    test(7) {
      inum - int(7) * {
        val hi = hiVarDef(genIntMulUnsignedHi(0x24924925, inum))
        Block(
          hi,
          (((inum - hi.ref) >>> 1) + hi.ref) >>> 2
        )
      }
    }

    test(-8) {
      inum - int(-8) * {
        val hi = hiVarDef(genIntMulUnsignedHi(0x00000009, inum))
        Block(
          hi,
          (((inum - hi.ref) >>> 1) + hi.ref) >>> 31
        )
      }
    }
  }
}
