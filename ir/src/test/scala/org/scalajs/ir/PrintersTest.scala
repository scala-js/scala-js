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

import Definitions._
import Printers._
import Trees._
import Types._

import OptimizerHints.{empty => NoOptHints}

class PrintersTest {
  private implicit val dummyPos = Position.NoPosition

  private def assertPrintEquals(expected: String, node: IRNode): Unit =
    assertPrintEqualsImpl(expected, _.printAnyNode(node))

  private def assertPrintEquals(expected: String, tpe: Type): Unit =
    assertPrintEqualsImpl(expected, _.print(tpe))

  private def assertPrintEquals(expected: String, typeRef: TypeRef): Unit =
    assertPrintEqualsImpl(expected, _.print(typeRef))

  private def assertPrintEqualsImpl(expected: String,
      print: IRTreePrinter => Unit): Unit = {
    val sw = new java.io.StringWriter
    val printer = new IRTreePrinter(sw)
    print(printer)
    assertEquals(expected.stripMargin.trim, sw.toString())
  }

  private implicit def string2ident(name: String): Ident = Ident(name)
  private implicit def string2classType(cls: String): ClassType = ClassType(cls)
  private implicit def string2classRef(cls: String): ClassRef = ClassRef(cls)

  private def b(value: Boolean): BooleanLiteral = BooleanLiteral(value)
  private def i(value: Int): IntLiteral = IntLiteral(value)
  private def l(value: Long): LongLiteral = LongLiteral(value)
  private def f(value: Float): FloatLiteral = FloatLiteral(value)
  private def d(value: Double): DoubleLiteral = DoubleLiteral(value)

  private def ref(ident: Ident, tpe: Type): VarRef = VarRef(ident)(tpe)

  private def arrayType(baseClassName: String, dimensions: Int): ArrayType =
    ArrayType(ArrayTypeRef(baseClassName, dimensions))

  @Test def printType(): Unit = {
    assertPrintEquals("any", AnyType)
    assertPrintEquals("nothing", NothingType)
    assertPrintEquals("void", UndefType)
    assertPrintEquals("boolean", BooleanType)
    assertPrintEquals("char", CharType)
    assertPrintEquals("byte", ByteType)
    assertPrintEquals("short", ShortType)
    assertPrintEquals("int", IntType)
    assertPrintEquals("long", LongType)
    assertPrintEquals("float", FloatType)
    assertPrintEquals("double", DoubleType)
    assertPrintEquals("string", StringType)
    assertPrintEquals("null", NullType)
    assertPrintEquals("<notype>", NoType)

    assertPrintEquals("O", ClassType(ObjectClass))

    assertPrintEquals("O[]", arrayType(ObjectClass, 1))
    assertPrintEquals("I[][]", arrayType("I", 2))

    assertPrintEquals("(x: int, var y: any)",
        RecordType(List(
            RecordType.Field("x", None, IntType, mutable = false),
            RecordType.Field("y", None, AnyType, mutable = true))))
  }

  @Test def printTypeRef(): Unit = {
    assertPrintEquals("O", ClassRef(ObjectClass))

    assertPrintEquals("O[]", ArrayTypeRef(ObjectClass, 1))
    assertPrintEquals("I[][]", ArrayTypeRef("I", 2))
  }

  @Test def printVarDef(): Unit = {
    assertPrintEquals("val x: int = 5",
        VarDef("x", IntType, mutable = false, i(5)))
    assertPrintEquals("var x: int = 5",
        VarDef("x", IntType, mutable = true, i(5)))
  }

  @Test def printParamDef(): Unit = {
    assertPrintEquals("x: int",
        ParamDef("x", IntType, mutable = false, rest = false))
    assertPrintEquals("var x: int",
        ParamDef("x", IntType, mutable = true, rest = false))
    assertPrintEquals("...x: any",
        ParamDef("x", AnyType, mutable = false, rest = true))
    assertPrintEquals("var ...x: any",
        ParamDef("x", AnyType, mutable = true, rest = true))
  }

  @Test def printSkip(): Unit = {
    assertPrintEquals("/*<skip>*/", Skip())
  }

  @Test def printBlock(): Unit = {
    assertPrintEquals(
        """
          |{
          |  5;
          |  6
          |}
        """,
        Block(i(5), i(6)))
  }

  @Test def printLabeled(): Unit = {
    assertPrintEquals(
        """
          |lab: {
          |  6
          |}
        """,
        Labeled("lab", NoType, i(6)))

    assertPrintEquals(
        """
          |lab[int]: {
          |  6
          |}
        """,
        Labeled("lab", IntType, i(6)))

    assertPrintEquals(
        """
          |lab: {
          |  5;
          |  6
          |}
        """,
        Labeled("lab", NoType, Block(i(5), i(6))))
  }

  @Test def printAssign(): Unit = {
    assertPrintEquals("x = 5",
        Assign(VarRef("x")(IntType), i(5)))
  }

  @Test def printReturn(): Unit = {
    assertPrintEquals("return@lab 5", Return(i(5), "lab"))
  }

  @Test def printIf(): Unit = {
    assertPrintEquals(
        """
          |if (true) {
          |  5
          |} else {
          |  6
          |}
        """,
        If(b(true), i(5), i(6))(IntType))

    assertPrintEquals(
        """
          |if (true) {
          |  5
          |}
        """,
        If(b(true), i(5), Skip())(NoType))

    assertPrintEquals(
        """
          |if (true) {
          |  5
          |} else if (false) {
          |  6
          |} else {
          |  7
          |}
        """,
        If(b(true), i(5), If(b(false), i(6), i(7))(IntType))(IntType))

    assertPrintEquals("x || y",
        If(ref("x", BooleanType), b(true), ref("y", BooleanType))(BooleanType))

    assertPrintEquals("x && y",
        If(ref("x", BooleanType), ref("y", BooleanType), b(false))(BooleanType))
  }

  @Test def printWhile(): Unit = {
    assertPrintEquals(
        """
          |while (true) {
          |  5
          |}
        """,
        While(b(true), i(5)))
  }

  @Test def printDoWhile(): Unit = {
    assertPrintEquals(
        """
          |do {
          |  5
          |} while (true)
        """,
        DoWhile(i(5), b(true)))
  }

  @Test def printForIn(): Unit = {
    assertPrintEquals(
        """
          |for (val x in o) {
          |  5
          |}
        """,
        ForIn(ref("o", AnyType), "x", i(5)))
  }

  @Test def printTry(): Unit = {
    assertPrintEquals(
        """
          |try {
          |  5
          |} catch (e) {
          |  6
          |}
        """,
        TryCatch(i(5), "e", i(6))(IntType))

    assertPrintEquals(
        """
          |try {
          |  5
          |} finally {
          |  6
          |}
        """,
        TryFinally(i(5), i(6)))

    assertPrintEquals(
        """
          |try {
          |  5
          |} catch (e) {
          |  6
          |} finally {
          |  7
          |}
        """,
        TryFinally(TryCatch(i(5), "e", i(6))(IntType), i(7)))
  }

  @Test def printThrow(): Unit = {
    assertPrintEquals("throw null", Throw(Null()))
  }

  @Test def printMatch(): Unit = {
    assertPrintEquals(
        """
          |match (x) {
          |  case 5:
          |    6;
          |  case 7 | 8:
          |    {
          |      9;
          |      10
          |    };
          |  default:
          |    11;
          |}
        """,
        Match(ref("x", IntType), List(
            List(i(5)) -> i(6),
            List(i(7), i(8)) -> Block(i(9), i(10))),
            i(11))(IntType))
  }

  @Test def printDebugger(): Unit = {
    assertPrintEquals("debugger", Debugger())
  }

  @Test def printNew(): Unit = {
    assertPrintEquals("new O().init___()",
        New(ObjectClass, "init___", Nil))
    assertPrintEquals("new T2().init___O__O(5, 6)",
        New("T2", "init___O__O", List(i(5), i(6))))
  }

  @Test def printLoadModule(): Unit = {
    assertPrintEquals("mod:s_Predef$", LoadModule("s_Predef$"))
  }

  @Test def printStoreModule(): Unit = {
    assertPrintEquals("mod:s_Predef$<-this",
        StoreModule("s_Predef$", This()("s_Predef$")))
  }

  @Test def printSelect(): Unit = {
    assertPrintEquals("x.f$1", Select(ref("x", "Ltest_Test"), "f$1")(IntType))
  }

  @Test def printSelectStatic(): Unit = {
    assertPrintEquals("Ltest_Test.f$1",
        SelectStatic("Ltest_Test", "f$1")(IntType))
  }

  @Test def printApply(): Unit = {
    assertPrintEquals("x.m__V()",
        Apply(ref("x", "Ltest_Test"), "m__V", Nil)(NoType))
    assertPrintEquals("x.m__I__I(5)",
        Apply(ref("x", "Ltest_Test"), "m__I__I", List(i(5)))(IntType))
    assertPrintEquals("x.m__I__I__I(5, 6)",
        Apply(ref("x", "Ltest_Test"), "m__I__I__I", List(i(5), i(6)))(IntType))
  }

  @Test def printApplyStatically(): Unit = {
    assertPrintEquals("x.Ltest_Test::m__V()",
        ApplyStatically(ref("x", "Ltest_Test"), "Ltest_Test", "m__V",
            Nil)(NoType))
    assertPrintEquals("x.Ltest_Test::m__I__I(5)",
        ApplyStatically(ref("x", "Ltest_Test"), "Ltest_Test", "m__I__I",
            List(i(5)))(IntType))
    assertPrintEquals("x.Ltest_Test::m__I__I__I(5, 6)",
        ApplyStatically(ref("x", "Ltest_Test"), "Ltest_Test", "m__I__I__I",
            List(i(5), i(6)))(IntType))
  }

  @Test def printApplyStatic(): Unit = {
    assertPrintEquals("Ltest_Test::m__V()",
        ApplyStatic("Ltest_Test", "m__V", Nil)(NoType))
    assertPrintEquals("Ltest_Test::m__I__I(5)",
        ApplyStatic("Ltest_Test", "m__I__I", List(i(5)))(IntType))
    assertPrintEquals("Ltest_Test::m__I__I__I(5, 6)",
        ApplyStatic("Ltest_Test", "m__I__I__I", List(i(5), i(6)))(IntType))
  }

  @Test def printUnaryOp(): Unit = {
    import UnaryOp._

    assertPrintEquals("(!x)", UnaryOp(Boolean_!, ref("x", BooleanType)))

    assertPrintEquals("((int)x)", UnaryOp(CharToInt, ref("x", CharType)))
    assertPrintEquals("((int)x)", UnaryOp(ByteToInt, ref("x", ByteType)))
    assertPrintEquals("((int)x)", UnaryOp(ShortToInt, ref("x", ShortType)))
    assertPrintEquals("((long)x)", UnaryOp(IntToLong, ref("x", IntType)))
    assertPrintEquals("((double)x)", UnaryOp(IntToDouble, ref("x", IntType)))
    assertPrintEquals("((double)x)", UnaryOp(FloatToDouble, ref("x", FloatType)))

    assertPrintEquals("((char)x)", UnaryOp(IntToChar, ref("x", IntType)))
    assertPrintEquals("((byte)x)", UnaryOp(IntToByte, ref("x", IntType)))
    assertPrintEquals("((short)x)", UnaryOp(IntToShort, ref("x", IntType)))
    assertPrintEquals("((int)x)", UnaryOp(LongToInt, ref("x", LongType)))
    assertPrintEquals("((int)x)", UnaryOp(DoubleToInt, ref("x", DoubleType)))
    assertPrintEquals("((float)x)", UnaryOp(DoubleToFloat, ref("x", DoubleType)))

    assertPrintEquals("((double)x)", UnaryOp(LongToDouble, ref("x", LongType)))
    assertPrintEquals("((long)x)", UnaryOp(DoubleToLong, ref("x", DoubleType)))
  }

  @Test def printPseudoUnaryOp(): Unit = {
    import BinaryOp._

    assertPrintEquals("(-x)", BinaryOp(Int_-, i(0), ref("x", IntType)))
    assertPrintEquals("(-x)", BinaryOp(Long_-, l(0), ref("x", LongType)))
    assertPrintEquals("(-x)", BinaryOp(Float_-, f(0), ref("x", FloatType)))
    assertPrintEquals("(-x)", BinaryOp(Double_-, d(0), ref("x", DoubleType)))

    assertPrintEquals("(~x)", BinaryOp(Int_^, i(-1), ref("x", IntType)))
    assertPrintEquals("(~x)", BinaryOp(Long_^, l(-1), ref("x", LongType)))
  }

  @Test def printBinaryOp(): Unit = {
    import BinaryOp._

    assertPrintEquals("(x === y)",
        BinaryOp(===, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x !== y)",
        BinaryOp(!==, ref("x", AnyType), ref("y", AnyType)))

    assertPrintEquals("(x +[string] y)",
        BinaryOp(String_+, ref("x", AnyType), ref("y", AnyType)))

    assertPrintEquals("(x ==[bool] y)",
        BinaryOp(Boolean_==, ref("x", BooleanType), ref("y", BooleanType)))
    assertPrintEquals("(x !=[bool] y)",
        BinaryOp(Boolean_!=, ref("x", BooleanType), ref("y", BooleanType)))
    assertPrintEquals("(x |[bool] y)",
        BinaryOp(Boolean_|, ref("x", BooleanType), ref("y", BooleanType)))
    assertPrintEquals("(x &[bool] y)",
        BinaryOp(Boolean_&, ref("x", BooleanType), ref("y", BooleanType)))

    assertPrintEquals("(x +[int] y)",
        BinaryOp(Int_+, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x -[int] y)",
        BinaryOp(Int_-, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x *[int] y)",
        BinaryOp(Int_*, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x /[int] y)",
        BinaryOp(Int_/, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x %[int] y)",
        BinaryOp(Int_%, ref("x", IntType), ref("y", IntType)))

    assertPrintEquals("(x |[int] y)",
        BinaryOp(Int_|, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x &[int] y)",
        BinaryOp(Int_&, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x ^[int] y)",
        BinaryOp(Int_^, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x <<[int] y)",
        BinaryOp(Int_<<, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x >>>[int] y)",
        BinaryOp(Int_>>>, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x >>[int] y)",
        BinaryOp(Int_>>, ref("x", IntType), ref("y", IntType)))

    assertPrintEquals("(x ==[int] y)",
        BinaryOp(Int_==, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x !=[int] y)",
        BinaryOp(Int_!=, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x <[int] y)",
        BinaryOp(Int_<, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x <=[int] y)",
        BinaryOp(Int_<=, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x >[int] y)",
        BinaryOp(Int_>, ref("x", IntType), ref("y", IntType)))
    assertPrintEquals("(x >=[int] y)",
        BinaryOp(Int_>=, ref("x", IntType), ref("y", IntType)))

    assertPrintEquals("(x +[long] y)",
        BinaryOp(Long_+, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x -[long] y)",
        BinaryOp(Long_-, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x *[long] y)",
        BinaryOp(Long_*, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x /[long] y)",
        BinaryOp(Long_/, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x %[long] y)",
        BinaryOp(Long_%, ref("x", LongType), ref("y", LongType)))

    assertPrintEquals("(x |[long] y)",
        BinaryOp(Long_|, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x &[long] y)",
        BinaryOp(Long_&, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x ^[long] y)",
        BinaryOp(Long_^, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x <<[long] y)",
        BinaryOp(Long_<<, ref("x", LongType), ref("y", IntType)))
    assertPrintEquals("(x >>>[long] y)",
        BinaryOp(Long_>>>, ref("x", LongType), ref("y", IntType)))
    assertPrintEquals("(x >>[long] y)",
        BinaryOp(Long_>>, ref("x", LongType), ref("y", IntType)))

    assertPrintEquals("(x ==[long] y)",
        BinaryOp(Long_==, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x !=[long] y)",
        BinaryOp(Long_!=, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x <[long] y)",
        BinaryOp(Long_<, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x <=[long] y)",
        BinaryOp(Long_<=, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x >[long] y)",
        BinaryOp(Long_>, ref("x", LongType), ref("y", LongType)))
    assertPrintEquals("(x >=[long] y)",
        BinaryOp(Long_>=, ref("x", LongType), ref("y", LongType)))

    assertPrintEquals("(x +[float] y)",
        BinaryOp(Float_+, ref("x", FloatType), ref("y", FloatType)))
    assertPrintEquals("(x -[float] y)",
        BinaryOp(Float_-, ref("x", FloatType), ref("y", FloatType)))
    assertPrintEquals("(x *[float] y)",
        BinaryOp(Float_*, ref("x", FloatType), ref("y", FloatType)))
    assertPrintEquals("(x /[float] y)",
        BinaryOp(Float_/, ref("x", FloatType), ref("y", FloatType)))
    assertPrintEquals("(x %[float] y)",
        BinaryOp(Float_%, ref("x", FloatType), ref("y", FloatType)))

    assertPrintEquals("(x +[double] y)",
        BinaryOp(Double_+, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x -[double] y)",
        BinaryOp(Double_-, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x *[double] y)",
        BinaryOp(Double_*, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x /[double] y)",
        BinaryOp(Double_/, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x %[double] y)",
        BinaryOp(Double_%, ref("x", DoubleType), ref("y", DoubleType)))

    assertPrintEquals("(x ==[double] y)",
        BinaryOp(Double_==, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x !=[double] y)",
        BinaryOp(Double_!=, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x <[double] y)",
        BinaryOp(Double_<, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x <=[double] y)",
        BinaryOp(Double_<=, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x >[double] y)",
        BinaryOp(Double_>, ref("x", DoubleType), ref("y", DoubleType)))
    assertPrintEquals("(x >=[double] y)",
        BinaryOp(Double_>=, ref("x", DoubleType), ref("y", DoubleType)))
  }

  @Test def printNewArray(): Unit = {
    assertPrintEquals("new I[3]", NewArray(ArrayTypeRef("I", 1), List(i(3))))
    assertPrintEquals("new I[3][]", NewArray(ArrayTypeRef("I", 2), List(i(3))))
    assertPrintEquals("new O[3][4][][]",
        NewArray(ArrayTypeRef("O", 4), List(i(3), i(4))))
  }

  @Test def printArrayValue(): Unit = {
    assertPrintEquals("I[]()",
        ArrayValue(ArrayTypeRef("I", 1), List()))
    assertPrintEquals("I[](5, 6)",
        ArrayValue(ArrayTypeRef("I", 1), List(i(5), i(6))))

    assertPrintEquals("I[][](null)",
        ArrayValue(ArrayTypeRef("I", 2), List(Null())))
  }

  @Test def printArrayLength(): Unit = {
    assertPrintEquals("x.length", ArrayLength(ref("x", arrayType("I", 1))))
  }

  @Test def printArraySelect(): Unit = {
    assertPrintEquals("x[3]",
        ArraySelect(ref("x", arrayType("I", 1)), i(3))(IntType))
  }

  @Test def printRecordValue(): Unit = {
    assertPrintEquals("(x = 3, y = 4)",
        RecordValue(
            RecordType(List(
                RecordType.Field("x", None, IntType, mutable = false),
                RecordType.Field("y", None, IntType, mutable = true))),
            List(i(3), i(4))))
  }

  @Test def printIsInstanceOf(): Unit = {
    assertPrintEquals("x.isInstanceOf[T]",
        IsInstanceOf(ref("x", AnyType), BoxedStringClass))
  }

  @Test def printAsInstanceOf(): Unit = {
    assertPrintEquals("x.asInstanceOf[T]",
        AsInstanceOf(ref("x", AnyType), BoxedStringClass))
  }

  @Test def printUnbox(): Unit = {
    assertPrintEquals("x.asInstanceOf[I]", Unbox(ref("x", AnyType), 'I'))
  }

  @Test def printGetClass(): Unit = {
    assertPrintEquals("x.getClass()", GetClass(ref("x", AnyType)))
  }

  @Test def printJSNew(): Unit = {
    assertPrintEquals("new C()", JSNew(ref("C", AnyType), Nil))
    assertPrintEquals("new C(4, 5)", JSNew(ref("C", AnyType), List(i(4), i(5))))
    assertPrintEquals("new x.C(4, 5)",
        JSNew(JSDotSelect(ref("x", AnyType), "C"), List(i(4), i(5))))
    assertPrintEquals("""new x["C"]()""",
        JSNew(JSBracketSelect(ref("x", AnyType), StringLiteral("C")), Nil))

    val fApplied = JSFunctionApply(ref("f", AnyType), Nil)
    assertPrintEquals("new (f())()", JSNew(fApplied, Nil))
    assertPrintEquals("new (f().C)(4, 5)",
        JSNew(JSDotSelect(fApplied, "C"), List(i(4), i(5))))
    assertPrintEquals("""new (f()["C"])()""",
        JSNew(JSBracketSelect(fApplied, StringLiteral("C")), Nil))
  }

  @Test def printJSDotSelect(): Unit = {
    assertPrintEquals("x.f", JSDotSelect(ref("x", AnyType), "f"))
  }

  @Test def printJSBracketSelect(): Unit = {
    assertPrintEquals("""x["f"]""",
        JSBracketSelect(ref("x", AnyType), StringLiteral("f")))
  }

  @Test def printJSFunctionApply(): Unit = {
    assertPrintEquals("f()", JSFunctionApply(ref("f", AnyType), Nil))
    assertPrintEquals("f(3, 4)",
        JSFunctionApply(ref("f", AnyType), List(i(3), i(4))))

    assertPrintEquals("(0, x.f)()",
        JSFunctionApply(JSDotSelect(ref("x", AnyType), "f"), Nil))
    assertPrintEquals("""(0, x["f"])()""",
        JSFunctionApply(JSBracketSelect(ref("x", AnyType), StringLiteral("f")),
            Nil))
    assertPrintEquals("(0, x.f$1)()",
        JSFunctionApply(Select(ref("x", "Ltest_Test"), "f$1")(AnyType), Nil))
  }

  @Test def printJSDotMethodApply(): Unit = {
    assertPrintEquals("x.m()", JSDotMethodApply(ref("x", AnyType), "m", Nil))
    assertPrintEquals("x.m(4, 5)",
        JSDotMethodApply(ref("x", AnyType), "m", List(i(4), i(5))))
  }

  @Test def printJSBracketMethodApply(): Unit = {
    assertPrintEquals("""x["m"]()""",
        JSBracketMethodApply(ref("x", AnyType), StringLiteral("m"), Nil))
    assertPrintEquals("""x["m"](4, 5)""",
        JSBracketMethodApply(ref("x", AnyType), StringLiteral("m"),
            List(i(4), i(5))))
  }

  @Test def printJSSuperBracketSelect(): Unit = {
    assertPrintEquals("""super(sc)::x["f"]""",
        JSSuperBracketSelect(ref("sc", AnyType), ref("x", AnyType), StringLiteral("f")))
  }

  @Test def printJSSuperBracketCall(): Unit = {
    assertPrintEquals("""super(sc)::x["f"]()""",
        JSSuperBracketCall(ref("sc", AnyType), ref("x", AnyType), StringLiteral("f"), Nil))
  }

  @Test def printJSSuperConstructorCall(): Unit = {
    assertPrintEquals("super()", JSSuperConstructorCall(Nil))
    assertPrintEquals("super(4, 5)", JSSuperConstructorCall(List(i(4), i(5))))
  }

  @Test def printLoadJSConstructor(): Unit = {
    assertPrintEquals("constructorOf[LTest]", LoadJSConstructor("LTest"))
  }

  @Test def printLoadJSModule(): Unit = {
    assertPrintEquals("mod:LTest$", LoadJSModule("LTest$"))
  }

  @Test def printJSSpread(): Unit = {
    assertPrintEquals("...x", JSSpread(ref("x", AnyType)))
  }

  @Test def printJSDelete(): Unit = {
    assertPrintEquals("delete x.f",
        JSDelete(JSDotSelect(ref("x", AnyType), "f")))
  }

  @Test def printJSUnaryOp(): Unit = {
    assertPrintEquals("(+x)", JSUnaryOp(JSUnaryOp.+, ref("x", AnyType)))
    assertPrintEquals("(-x)", JSUnaryOp(JSUnaryOp.-, ref("x", AnyType)))
    assertPrintEquals("(~x)", JSUnaryOp(JSUnaryOp.~, ref("x", AnyType)))
    assertPrintEquals("(!x)", JSUnaryOp(JSUnaryOp.!, ref("x", AnyType)))
    assertPrintEquals("(typeof x)",
        JSUnaryOp(JSUnaryOp.typeof, ref("x", AnyType)))
  }

  @Test def printJSBinaryOp(): Unit = {
    assertPrintEquals("(x === y)",
        JSBinaryOp(JSBinaryOp.===, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x !== y)",
        JSBinaryOp(JSBinaryOp.!==, ref("x", AnyType), ref("y", AnyType)))

    assertPrintEquals("(x + y)",
        JSBinaryOp(JSBinaryOp.+, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x - y)",
        JSBinaryOp(JSBinaryOp.-, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x * y)",
        JSBinaryOp(JSBinaryOp.*, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x / y)",
        JSBinaryOp(JSBinaryOp./, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x % y)",
        JSBinaryOp(JSBinaryOp.%, ref("x", AnyType), ref("y", AnyType)))

    assertPrintEquals("(x | y)",
        JSBinaryOp(JSBinaryOp.|, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x & y)",
        JSBinaryOp(JSBinaryOp.&, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x ^ y)",
        JSBinaryOp(JSBinaryOp.^, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x << y)",
        JSBinaryOp(JSBinaryOp.<<, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x >>> y)",
        JSBinaryOp(JSBinaryOp.>>>, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x >> y)",
        JSBinaryOp(JSBinaryOp.>>, ref("x", AnyType), ref("y", AnyType)))

    assertPrintEquals("(x < y)",
        JSBinaryOp(JSBinaryOp.<, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x <= y)",
        JSBinaryOp(JSBinaryOp.<=, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x > y)",
        JSBinaryOp(JSBinaryOp.>, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x >= y)",
        JSBinaryOp(JSBinaryOp.>=, ref("x", AnyType), ref("y", AnyType)))

    assertPrintEquals("(x && y)",
        JSBinaryOp(JSBinaryOp.&&, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x || y)",
        JSBinaryOp(JSBinaryOp.||, ref("x", AnyType), ref("y", AnyType)))

    assertPrintEquals("(x in y)",
        JSBinaryOp(JSBinaryOp.in, ref("x", AnyType), ref("y", AnyType)))
    assertPrintEquals("(x instanceof y)",
        JSBinaryOp(JSBinaryOp.instanceof, ref("x", AnyType), ref("y", AnyType)))
  }

  @Test def printJSArrayConstr(): Unit = {
    assertPrintEquals("[]", JSArrayConstr(Nil))
    assertPrintEquals("[5, 6]", JSArrayConstr(List(i(5), i(6))))
  }

  @Test def printJSObjectConstr(): Unit = {
    assertPrintEquals("{}", JSObjectConstr(Nil))

    assertPrintEquals(
        """
          |{
          |  f: 5,
          |  "g": 6
          |}
        """,
        JSObjectConstr(List(Ident("f") -> i(5), StringLiteral("g") -> i(6))))
  }

  @Test def printGlobalRef(): Unit = {
    assertPrintEquals("global:Foo", JSGlobalRef("Foo"))
  }

  @Test def printJSLinkingInfo(): Unit = {
    assertPrintEquals("<linkinginfo>", JSLinkingInfo())
  }

  @Test def printUndefined(): Unit = {
    assertPrintEquals("(void 0)", Undefined())
  }

  @Test def printNull(): Unit = {
    assertPrintEquals("null", Null())
  }

  @Test def printBoolean(): Unit = {
    assertPrintEquals("true", BooleanLiteral(true))
    assertPrintEquals("false", BooleanLiteral(false))
  }

  @Test def printCharLiteral(): Unit = {
    assertPrintEquals("'A'", CharLiteral('A'))
    assertPrintEquals("'\\u0005'", CharLiteral('\u0005'))
    assertPrintEquals("'\\ufffb'", CharLiteral('\ufffb'))
  }

  @Test def printByteLiteral(): Unit = {
    assertPrintEquals("5_b", ByteLiteral(5))
    assertPrintEquals("(-5_b)", ByteLiteral(-5))
  }

  @Test def printShortLiteral(): Unit = {
    assertPrintEquals("5_s", ShortLiteral(5))
    assertPrintEquals("(-5_s)", ShortLiteral(-5))
  }

  @Test def printIntLiteral(): Unit = {
    assertPrintEquals("5", IntLiteral(5))
    assertPrintEquals("(-5)", IntLiteral(-5))
  }

  @Test def printLongLiteral(): Unit = {
    assertPrintEquals("5L", LongLiteral(5L))
    assertPrintEquals("(-5L)", LongLiteral(-5L))
  }

  @Test def printFloatLiteral(): Unit = {
    assertPrintEquals(0.0f.toString + "f", FloatLiteral(0.0f))
    assertPrintEquals("(-0f)", FloatLiteral(-0.0f))
    assertPrintEquals("Infinityf", FloatLiteral(Float.PositiveInfinity))
    assertPrintEquals("(-Infinityf)", FloatLiteral(Float.NegativeInfinity))
    assertPrintEquals("NaNf", FloatLiteral(Float.NaN))

    assertPrintEquals(1.0f.toString + "f", FloatLiteral(1.0f))
    assertPrintEquals(1.5f.toString + "f", FloatLiteral(1.5f))
    assertPrintEquals("(" + (-1.5f).toString + "f)", FloatLiteral(-1.5f))
  }

  @Test def printDoubleLiteral(): Unit = {
    assertPrintEquals(0.0.toString + "d", DoubleLiteral(0.0))
    assertPrintEquals("(-0d)", DoubleLiteral(-0.0))
    assertPrintEquals("Infinityd", DoubleLiteral(Double.PositiveInfinity))
    assertPrintEquals("(-Infinityd)", DoubleLiteral(Double.NegativeInfinity))
    assertPrintEquals("NaNd", DoubleLiteral(Double.NaN))

    assertPrintEquals(1.0.toString + "d", DoubleLiteral(1.0))
    assertPrintEquals(1.5.toString + "d", DoubleLiteral(1.5))
    assertPrintEquals("(" + (-1.5).toString + "d)", DoubleLiteral(-1.5))
  }

  @Test def printStringLiteral(): Unit = {
    assertPrintEquals(raw"""""""", StringLiteral(""))
    assertPrintEquals(raw""""foo"""", StringLiteral("foo"))
    assertPrintEquals(raw""""fo\no"""", StringLiteral("fo\no"))
    assertPrintEquals("\"a\\u1234b\"", StringLiteral("a\u1234b"))
  }

  @Test def printClassOf(): Unit = {
    assertPrintEquals("classOf[LTest]", ClassOf("LTest"))
  }

  @Test def printVarRef(): Unit = {
    assertPrintEquals("x", VarRef("x")(IntType))
  }

  @Test def printThis(): Unit = {
    assertPrintEquals("this", This()(AnyType))
  }

  @Test def printClosure(): Unit = {
    assertPrintEquals(
        """
          |(lambda<>() = {
          |  5
          |})
        """,
        Closure(false, Nil, Nil, i(5), Nil))

    assertPrintEquals(
        """
          |(arrow-lambda<x: any = a, y: int = 6>(z: any) = {
          |  z
          |})
        """,
        Closure(
            true,
            List(
                ParamDef("x", AnyType, mutable = false, rest = false),
                ParamDef("y", IntType, mutable = false, rest = false)),
            List(ParamDef("z", AnyType, mutable = false, rest = false)),
            ref("z", AnyType),
            List(ref("a", IntType), i(6))))
  }

  @Test def printCreateJSClass(): Unit = {
    assertPrintEquals(
        """
          |createjsclass[LFoo](x, y)
        """,
        CreateJSClass("LFoo", List(ref("x", IntType), ref("y", AnyType))))
  }

  @Test def printTransient(): Unit = {
    class MyTransient(expr: Tree) extends Transient.Value {
      def printIR(out: Printers.IRTreePrinter): Unit = {
        out.print("mytransient(")
        out.print(expr)
        out.print(")")
      }
    }

    assertPrintEquals("mytransient(5)",
        Transient(new MyTransient(i(5)))(AnyType))
  }

  @Test def printClassDefKinds(): Unit = {
    import ClassKind._

    def makeForKind(kind: ClassKind): ClassDef = {
      ClassDef("LTest", kind, None, Some(ObjectClass), Nil, None, None, Nil,
          Nil)(
          NoOptHints)
    }

    assertPrintEquals(
        """
          |class LTest extends O {
          |}
        """,
        makeForKind(Class))

    assertPrintEquals(
        """
          |module class LTest extends O {
          |}
        """,
        makeForKind(ModuleClass))

    assertPrintEquals(
        """
          |interface LTest extends O {
          |}
        """,
        makeForKind(Interface))

    assertPrintEquals(
        """
          |abstract js type LTest extends O {
          |}
        """,
        makeForKind(AbstractJSType))

    assertPrintEquals(
        """
          |hijacked class LTest extends O {
          |}
        """,
        makeForKind(HijackedClass))

    assertPrintEquals(
        """
          |js class LTest extends O {
          |}
        """,
        makeForKind(JSClass))

    assertPrintEquals(
        """
          |js module class LTest extends O {
          |}
        """,
        makeForKind(JSModuleClass))

    assertPrintEquals(
        """
          |native js class LTest extends O {
          |}
        """,
        makeForKind(NativeJSClass))

    assertPrintEquals(
        """
          |native js module class LTest extends O {
          |}
        """,
        makeForKind(NativeJSModuleClass))
  }

  @Test def printClassDefParents(): Unit = {
    def makeForParents(superClass: Option[Ident],
        interfaces: List[Ident]): ClassDef = {
      ClassDef("LTest", ClassKind.Class, None, superClass, interfaces, None,
          None, Nil, Nil)(
          NoOptHints)
    }

    assertPrintEquals(
        """
          |class LTest {
          |}
        """,
        makeForParents(None, Nil))

    assertPrintEquals(
        """
          |class LTest extends O implements LIntf {
          |}
        """,
        makeForParents(Some(ObjectClass), List("LIntf")))

    assertPrintEquals(
        """
          |class LTest extends sr_AbstractFunction0 implements LIntf1, LIntf2 {
          |}
        """,
        makeForParents(Some("sr_AbstractFunction0"), List("LIntf1", "LIntf2")))
  }

  @Test def printClassDefJSNativeLoadSpec(): Unit = {
    assertPrintEquals(
        """
          |native js class LTest extends O loadfrom global:Foo["Bar"] {
          |}
        """,
        ClassDef("LTest", ClassKind.NativeJSClass, None, Some(ObjectClass), Nil,
            None, Some(JSNativeLoadSpec.Global("Foo", List("Bar"))), Nil, Nil)(
            NoOptHints))

    assertPrintEquals(
        """
          |native js class LTest extends O loadfrom import(foo)["Bar"] {
          |}
        """,
        ClassDef("LTest", ClassKind.NativeJSClass, None, Some(ObjectClass), Nil,
            None, Some(JSNativeLoadSpec.Import("foo", List("Bar"))), Nil, Nil)(
            NoOptHints))

    assertPrintEquals(
        """
          |native js class LTest extends O loadfrom import(foo)["Bar"] fallback global:Baz["Foobar"] {
          |}
        """,
        ClassDef("LTest", ClassKind.NativeJSClass, None, Some(ObjectClass), Nil,
            None,
            Some(JSNativeLoadSpec.ImportWithGlobalFallback(
                JSNativeLoadSpec.Import("foo", List("Bar")),
                JSNativeLoadSpec.Global("Baz", List("Foobar")))), Nil, Nil)(
            NoOptHints))
  }

  @Test def printClassDefJSClassCaptures(): Unit = {
    assertPrintEquals(
        """
          |captures: none
          |js class LTest extends O {
          |}
        """,
        ClassDef("LTest", ClassKind.JSClass, Some(Nil), Some(ObjectClass), Nil,
            None, None, Nil, Nil)(
            NoOptHints))

    assertPrintEquals(
        """
          |captures: x: int, y: string
          |js class LTest extends O {
          |}
        """,
        ClassDef("LTest", ClassKind.JSClass,
            Some(List(
                ParamDef("x", IntType, mutable = false, rest = false),
                ParamDef("y", StringType, mutable = false, rest = false)
            )),
            Some(ObjectClass), Nil, None, None, Nil, Nil)(
            NoOptHints))
  }

  @Test def printClassDefJSSuperClass(): Unit = {
    assertPrintEquals(
        """
          |captures: sup: any
          |js class LTest extends LBar (via sup) {
          |}
        """,
        ClassDef("LTest", ClassKind.JSClass,
            Some(List(ParamDef("sup", AnyType, mutable = false, rest = false))),
            Some("LBar"), Nil, Some(ref("sup", AnyType)), None, Nil, Nil)(
            NoOptHints))
  }

  @Test def printClassDefOptimizerHints(): Unit = {
    assertPrintEquals(
        """
          |@hints(1) class LTest extends O {
          |}
        """,
        ClassDef("LTest", ClassKind.Class, None, Some(ObjectClass), Nil, None,
            None, Nil, Nil)(
            NoOptHints.withInline(true)))
  }

  @Test def printClassDefDefs(): Unit = {
    assertPrintEquals(
        """
          |module class LTest extends O {
          |  val x$1: int
          |  var y$1: int
          |  export top module "pkg.Foo"
          |}
        """,
        ClassDef("LTest", ClassKind.ModuleClass, None, Some(ObjectClass), Nil,
            None, None,
            List(
                FieldDef(MemberFlags.empty, "x$1", IntType),
                FieldDef(MemberFlags.empty.withMutable(true), "y$1", IntType)),
            List(
                TopLevelModuleExportDef("pkg.Foo")))(
            NoOptHints))
  }

  @Test def printFieldDef(): Unit = {
    assertPrintEquals("val x$1: int",
        FieldDef(MemberFlags.empty, "x$1", IntType))
    assertPrintEquals("var y$1: any",
        FieldDef(MemberFlags.empty.withMutable(true), "y$1", AnyType))

    assertPrintEquals("""val "x": int""",
        FieldDef(MemberFlags.empty, StringLiteral("x"), IntType))
    assertPrintEquals("""var "y": any""",
        FieldDef(MemberFlags.empty.withMutable(true), StringLiteral("y"), AnyType))

    assertPrintEquals("""static val "x": int""",
        FieldDef(MemberFlags.empty.withStatic(true), StringLiteral("x"), IntType))
    assertPrintEquals("""static var "y": any""",
        FieldDef(MemberFlags.empty.withStatic(true).withMutable(true), StringLiteral("y"), AnyType))
  }

  @Test def printMethodDef(): Unit = {
    assertPrintEquals(
        """
          |def m__I__I(x: int): int = <abstract>
        """,
        MethodDef(MemberFlags.empty, "m__I__I",
            List(ParamDef("x", IntType, mutable = false, rest = false)),
            IntType, None)(NoOptHints, None))

    assertPrintEquals(
        """
          |def m__I__I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, "m__I__I",
            List(ParamDef("x", IntType, mutable = false, rest = false)),
            IntType, Some(i(5)))(NoOptHints, None))

    assertPrintEquals(
        """
          |@hints(1) def m__I__I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, "m__I__I",
            List(ParamDef("x", IntType, mutable = false, rest = false)),
            IntType, Some(i(5)))(NoOptHints.withInline(true), None))

    assertPrintEquals(
        """
          |def m__I__V(x: int) {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, "m__I__V",
            List(ParamDef("x", IntType, mutable = false, rest = false)),
            NoType, Some(i(5)))(NoOptHints, None))

    assertPrintEquals(
        """
          |def "m"(x: any): any = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, StringLiteral("m"),
            List(ParamDef("x", AnyType, mutable = false, rest = false)),
            AnyType, Some(i(5)))(NoOptHints, None))

    assertPrintEquals(
        """
          |def "m"(...x: any): any = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, StringLiteral("m"),
            List(ParamDef("x", AnyType, mutable = false, rest = true)),
            AnyType, Some(i(5)))(NoOptHints, None))

    assertPrintEquals(
        """
          |static def m__I__I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty.withStatic(true), "m__I__I",
            List(ParamDef("x", IntType, mutable = false, rest = false)),
            IntType, Some(i(5)))(NoOptHints, None))
  }

  @Test def printPropertyDef(): Unit = {
    for (static <- Seq(false, true)) {
      val staticStr =
        if (static) "static "
        else ""
      val flags = MemberFlags.empty.withStatic(static)

      assertPrintEquals(
          s"""
            |${staticStr}get "prop"(): any = {
            |  5
            |}
          """,
          PropertyDef(flags, StringLiteral("prop"), Some(i(5)), None))

      assertPrintEquals(
          s"""
            |${staticStr}set "prop"(x: any) {
            |  7
            |}
          """,
          PropertyDef(flags, StringLiteral("prop"),
              None,
              Some((ParamDef("x", AnyType, mutable = false, rest = false), i(7)))))

      assertPrintEquals(
          s"""
            |${staticStr}get "prop"(): any = {
            |  5
            |}
            |${staticStr}set "prop"(x: any) {
            |  7
            |}
          """,
          PropertyDef(flags, StringLiteral("prop"),
              Some(i(5)),
              Some((ParamDef("x", AnyType, mutable = false, rest = false),
                  i(7)))))
    }
  }

  @Test def printJSClassExportDef(): Unit = {
    assertPrintEquals(
        """export top class "pkg.Foo"""",
        TopLevelJSClassExportDef("pkg.Foo"))
  }

  @Test def printTopLevelModuleExportDef(): Unit = {
    assertPrintEquals(
        """export top module "pkg.Foo"""",
        TopLevelModuleExportDef("pkg.Foo"))
  }

  @Test def printTopLevelMethodExportDef(): Unit = {
    assertPrintEquals(
        """
          |export top static def "pkg.foo"(x: any): any = {
          |  5
          |}""",
        TopLevelMethodExportDef(MethodDef(MemberFlags.empty.withStatic(true),
            StringLiteral("pkg.foo"),
            List(ParamDef("x", AnyType, mutable = false, rest = false)),
            AnyType, Some(i(5)))(NoOptHints, None)))
  }

  @Test def printTopLevelFieldExportDef(): Unit = {
    assertPrintEquals(
        """
          |export top static field x$1 as "x"
        """,
        TopLevelFieldExportDef("x", "x$1"))
  }
}
