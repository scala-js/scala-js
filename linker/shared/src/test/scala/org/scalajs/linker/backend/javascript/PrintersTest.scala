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

package org.scalajs.linker.backend.javascript

import scala.language.implicitConversions

import java.nio.charset.StandardCharsets.UTF_8

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir

import Trees._

class PrintersTest {

  private implicit val pos: ir.Position = ir.Position.NoPosition

  private implicit def str2ident(name: String): Ident =
    Ident(name, ir.OriginalName.NoOriginalName)

  private def printTree(tree: Tree): String = {
    val out = new ByteArrayWriter
    val printer = new Printers.JSTreePrinter(out)
    printer.printStat(tree)
    new String(out.toByteArray(), UTF_8)
  }

  private def assertPrintEquals(expected: String, tree: Tree): Unit = {
    val printResult = printTree(tree)
    assertEquals(expected.stripMargin.trim + "\n", printResult)
  }

  @Test def printFunctionDef(): Unit = {
    assertPrintEquals(
        """
          |function test() {
          |  const x = 2;
          |  return x;
          |}
        """,
        FunctionDef("test", Nil, None, Block(
          Let("x", mutable = false, Some(IntLiteral(2))),
          Return(VarRef("x"))))
    )

    assertPrintEquals(
        """
          |function test() {
          |}
        """,
        FunctionDef("test", Nil, None, Skip())
    )
  }

  @Test def printClassDef(): Unit = {
    assertPrintEquals(
        """
          |class MyClass extends foo.Other {
          |}
        """,
        ClassDef(Some("MyClass"), Some(DotSelect(VarRef("foo"), "Other")), Nil)
    )

    assertPrintEquals(
        """
          |class MyClass {
          |  foo() {
          |  }
          |  get a() {
          |    return 1;
          |  }
          |  set a(x) {
          |  }
          |}
        """,
        ClassDef(Some("MyClass"), None, List(
          MethodDef(false, "foo", Nil, None, Skip()),
          GetterDef(false, "a", Return(IntLiteral(1))),
          SetterDef(false, "a", ParamDef("x"), Skip())
        ))
    )
  }

  @Test def printJSDocConstructor(): Unit = {
    assertPrintEquals(
      """
        |/** @constructor */
        |ctor = (function() {
        |});
      """,
      JSDocConstructor(Assign(VarRef("ctor"), Function(false, Nil, None, Skip())))
    )
  }

  @Test def printFor(): Unit = {
    assertPrintEquals(
      """
        |for (let x = 1; (x < 15); x = (x + 1)) {
        |}
      """,
      For(Let("x", true, Some(IntLiteral(1))),
          BinaryOp(ir.Trees.JSBinaryOp.<, VarRef("x"), IntLiteral(15)),
          Assign(VarRef("x"), BinaryOp(ir.Trees.JSBinaryOp.+, VarRef("x"), IntLiteral(1))),
          Skip())
    )
  }

  @Test def printForIn(): Unit = {
    assertPrintEquals(
      """
        |for (var x in foo) {
        |}
      """,
      ForIn(VarDef("x", None), VarRef("foo"), Skip())
    )
  }

  @Test def printIf(): Unit = {
    assertPrintEquals(
        """
          |if (false) {
          |  1;
          |}
        """,
        If(BooleanLiteral(false), IntLiteral(1), Skip())
    )

    assertPrintEquals(
        """
          |if (false) {
          |  1;
          |} else {
          |  2;
          |}
        """,
        If(BooleanLiteral(false), IntLiteral(1), IntLiteral(2))
    )

    assertPrintEquals(
        """
          |if (false) {
          |  1;
          |} else if (true) {
          |  2;
          |} else {
          |  3;
          |}
        """,
        If(BooleanLiteral(false), IntLiteral(1),
            If(BooleanLiteral(true), IntLiteral(2), IntLiteral(3)))
    )
  }

  @Test def delayedIdentPrintVersusShow(): Unit = {
    locally {
      object resolver extends DelayedIdent.Resolver {
        def resolve(): String = "foo"
        def debugString: String = "bar"
      }

      val tree = DotSelect(VarRef("x"), DelayedIdent(resolver))

      assertPrintEquals("x.foo;", tree)
      assertEquals("x.<delayed:bar>;", tree.show)
    }

    // Even when `resolve()` throws, `show` still succeeds based on `debugString`.
    locally {
      object resolver extends DelayedIdent.Resolver {
        def resolve(): String = throw new IllegalStateException("not ready")
        def debugString: String = "bar"
      }

      val tree = DotSelect(VarRef("x"), DelayedIdent(resolver))

      assertThrows(classOf[IllegalStateException], () => printTree(tree))
      assertEquals("x.<delayed:bar>;", tree.show)
    }
  }

  @Test def showPrintedTree(): Unit = {
    val tree = PrintedTree("test".getBytes(UTF_8), SourceMapWriter.Fragment.Empty)

    assertEquals("test", tree.show)
  }

  @Test def showNestedPrintedTree(): Unit = {
    val tree = PrintedTree("  test\n".getBytes(UTF_8), SourceMapWriter.Fragment.Empty)

    val str = While(BooleanLiteral(false), tree).show
    assertEquals(
      """
        |while (false) {
        |  test
        |}
      """.stripMargin.trim,
      str
    )
  }
}
