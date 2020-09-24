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

/** Tests for potential name clashes between user-written identifiers and
 *  compiler-generated ones.
 */
class InternalNameClashesTestEx {
  import InternalNameClashesTestEx._

  @Test def testLocalVariableClashWithCodegenVar(): Unit = {
    /* This tests that user-defined local variables cannot clash with
     * compiler-generated "codegenVars".
     */
    @noinline def someValue(): Int = 42

    val $c_Lorg_scalajs_testsuite_compiler_InternalNameClashesTestEx$LocalVariableClashWithCodegenVar = someValue()
    val foo = new LocalVariableClashWithCodegenVar(5)
    assertEquals(42, $c_Lorg_scalajs_testsuite_compiler_InternalNameClashesTestEx$LocalVariableClashWithCodegenVar)
    assertEquals(5, foo.x)
  }

  @Test def testLocalVariableClashWithExplicitThisParam(): Unit = {
    /* Default methods in interfaces receive an explicit this parameter, which
     * is encoded as `$thiz` by the emitter. Here we make sure that we can
     * write a user-defined `$thiz` local variable without clashing with the
     * compiler-generated one.
     */
    val foo = new LocalVariableClashWithExplicitThisParamTrait {
      val x: Int = 5
    }

    assertEquals(1021, foo.test(6))
  }

  @Test def testLocalClashWithTempVar_issue2971(): Unit = {
    @noinline def initValue: Int = 5

    @noinline def sum(x: Int, y: Int): Int = x + y

    val $x1 = initValue
    val t = sum({
      val y = sum($x1, 7)
      sum(y, 3) // this will be assigned to a temporary var called `$x1`
    }, {
      val z = sum($x1, 12)
      sum(z, 4)
    })
    assertEquals(36, t)
    assertEquals(5, $x1)
  }

  @Test def testLocalVariableClashWithRecordField(): Unit = {
    @noinline def test(): Boolean = true

    // The two record fields will be called `babar__foo` and `babar__bar`
    val babar =
      if (test()) new LocalVariableClashWithRecordField(5, "one")
      else new LocalVariableClashWithRecordField(6, "two")

    val babar__foo = test()

    assertEquals(5, babar.foo)
    assertEquals("one", babar.bar)
    assertEquals(true, babar__foo)
  }

}

object InternalNameClashesTestEx {
  @noinline
  class LocalVariableClashWithCodegenVar(val x: Int)

  trait LocalVariableClashWithExplicitThisParamTrait {
    val x: Int

    @noinline
    def test(y: Int): Int = {
      /* We test both $thiz and $$thiz because GlobalScopeTestEx references the
       * global variable `$thiz`, causing codegen vars for `$thiz` to be
       * renamed to `$$thiz`.
       */
      val $thiz = x + y
      val $$thiz = x * y
      ($thiz * $thiz) + ($$thiz * $$thiz)
    }
  }

  @inline
  class LocalVariableClashWithRecordField(val foo: Int, val bar: String)
}
