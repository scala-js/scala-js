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

import Names._
import OriginalName.NoOriginalName
import Printers._
import Trees._
import Types._
import WellKnownNames._

import TestIRBuilder._

class PrintersTest {
  import MemberNamespace.{Constructor, Private, PublicStatic => Static, PrivateStatic}

  /** An original name. */
  private val TestON = OriginalName("orig name")

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

  @Test def printType(): Unit = {
    assertPrintEquals("any", AnyType)
    assertPrintEquals("any!", AnyNotNullType)
    assertPrintEquals("nothing", NothingType)
    assertPrintEquals("undef", UndefType)
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
    assertPrintEquals("void", VoidType)

    assertPrintEquals("java.lang.Object", ClassType(ObjectClass, nullable = true))
    assertPrintEquals("java.lang.String!",
        ClassType(BoxedStringClass, nullable = false))

    assertPrintEquals("java.lang.Object[]", arrayType(ObjectClass, 1))
    assertPrintEquals("int[][]", arrayType(IntRef, 2))
    assertPrintEquals("java.lang.String[]!",
        ArrayType(ArrayTypeRef(BoxedStringClass, 1), nullable = false))

    assertPrintEquals("(() => int)", ClosureType(Nil, IntType, nullable = true))
    assertPrintEquals("((any, java.lang.String!) => boolean)!",
        ClosureType(List(AnyType, ClassType(BoxedStringClass, nullable = false)), BooleanType, nullable = false))

    assertPrintEquals("(x: int, var y: any)",
        RecordType(List(
            RecordType.Field("x", NON, IntType, mutable = false),
            RecordType.Field("y", NON, AnyType, mutable = true))))
  }

  @Test def printTypeRef(): Unit = {
    assertPrintEquals("java.lang.Object", ClassRef(ObjectClass))

    assertPrintEquals("java.lang.Object[]", ArrayTypeRef(ObjectClass, 1))
    assertPrintEquals("int[][]", ArrayTypeRef(IntRef, 2))

    assertPrintEquals("foo", TransientTypeRef(LabelName("foo"))(IntType))
  }

  @Test def printVarDef(): Unit = {
    assertPrintEquals("val x: int = 5",
        VarDef("x", NON, IntType, mutable = false, i(5)))
    assertPrintEquals("var x: int = 5",
        VarDef("x", NON, IntType, mutable = true, i(5)))
    assertPrintEquals("val x{orig name}: int = 5",
        VarDef("x", TestON, IntType, mutable = false, i(5)))
  }

  @Test def printParamDef(): Unit = {
    assertPrintEquals("x: int",
        ParamDef("x", NON, IntType, mutable = false))
    assertPrintEquals("var x: int",
        ParamDef("x", NON, IntType, mutable = true))
    assertPrintEquals("x{orig name}: int",
        ParamDef("x", TestON, IntType, mutable = false))
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
        Labeled("lab", VoidType, i(6)))

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
        Labeled("lab", VoidType, Block(i(5), i(6))))
  }

  @Test def printAssign(): Unit = {
    assertPrintEquals("x = 5",
        Assign(VarRef("x")(IntType), i(5)))
  }

  @Test def printReturn(): Unit = {
    assertPrintEquals("return@lab 5", Return(i(5), "lab"))
    assertPrintEquals("return@lab", Return(Skip(), "lab"))
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
        If(b(true), i(5), Skip())(VoidType))

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

  @Test def printLinkTimeIf(): Unit = {
    assertPrintEquals(
        """
          |link-time-if (true) {
          |  5
          |} else {
          |  6
          |}
        """,
        LinkTimeIf(b(true), i(5), i(6))(IntType))

    assertPrintEquals(
        """
          |link-time-if (true) {
          |  5
          |} else {
          |}
        """,
        LinkTimeIf(b(true), i(5), Skip())(VoidType))

    assertPrintEquals(
        """
          |link-time-if (true) {
          |  5
          |} else {
          |  link-time-if (false) {
          |    6
          |  } else {
          |    7
          |  }
          |}
        """,
        LinkTimeIf(b(true), i(5), LinkTimeIf(b(false), i(6), i(7))(IntType))(IntType))

    assertPrintEquals(
        """
          |link-time-if (x) {
          |  true
          |} else {
          |  y
          |}
        """,
        LinkTimeIf(ref("x", BooleanType), b(true), ref("y", BooleanType))(BooleanType))

    assertPrintEquals(
        """
          |link-time-if (x) {
          |  y
          |} else {
          |  false
          |}
        """,
        LinkTimeIf(ref("x", BooleanType), ref("y", BooleanType), b(false))(BooleanType))
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

  @Test def printForIn(): Unit = {
    assertPrintEquals(
        """
          |for (val x in o) {
          |  5
          |}
        """,
        ForIn(ref("o", AnyType), "x", NON, i(5)))

    assertPrintEquals(
        """
          |for (val x{orig name} in o) {
          |  5
          |}
        """,
        ForIn(ref("o", AnyType), "x", TestON, i(5)))
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
        TryCatch(i(5), "e", NON, i(6))(IntType))

    assertPrintEquals(
        """
          |try {
          |  5
          |} catch (e{orig name}) {
          |  6
          |}
        """,
        TryCatch(i(5), "e", TestON, i(6))(IntType))

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
        TryFinally(TryCatch(i(5), "e", NON, i(6))(IntType), i(7)))
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

  @Test def printJSAwait(): Unit = {
    assertPrintEquals("await(p)", JSAwait(ref("p", AnyType)))
  }

  @Test def printDebugger(): Unit = {
    assertPrintEquals("debugger", Debugger())
  }

  @Test def printNew(): Unit = {
    assertPrintEquals("new java.lang.Object().<init>;V()",
        New(ObjectClass, NoArgConstructorName, Nil))
    assertPrintEquals("new scala.Tuple2().<init>;Ljava.lang.Object;Ljava.lang.Object;V(5, 6)",
        New("scala.Tuple2", MethodName.constructor(List(O, O)), List(i(5), i(6))))
  }

  @Test def printLoadModule(): Unit = {
    assertPrintEquals("mod:scala.Predef$", LoadModule("scala.Predef$"))
  }

  @Test def printStoreModule(): Unit = {
    assertPrintEquals("<storeModule>", StoreModule())
  }

  @Test def printSelect(): Unit = {
    assertPrintEquals("x.test.Test::f",
        Select(ref("x", "test.Test"), FieldName("test.Test", "f"))(IntType))
  }

  @Test def printSelectStatic(): Unit = {
    assertPrintEquals("test.Test::f",
        SelectStatic(FieldName("test.Test", "f"))(IntType))
  }

  @Test def printApply(): Unit = {
    assertPrintEquals("x.m;V()",
        Apply(EAF, ref("x", "test.Test"), MethodName("m", Nil, V), Nil)(VoidType))
    assertPrintEquals("x.m;I;I(5)",
        Apply(EAF, ref("x", "test.Test"), MethodName("m", List(I), I),
            List(i(5)))(IntType))
    assertPrintEquals("x.m;I;I;I(5, 6)",
        Apply(EAF, ref("x", "test.Test"), MethodName("m", List(I, I), I),
            List(i(5), i(6)))(IntType))
  }

  @Test def printApplyStatically(): Unit = {
    assertPrintEquals("x.test.Test::m;V()",
        ApplyStatically(EAF, ref("x", "test.Test"), "test.Test",
            MethodName("m", Nil, V), Nil)(VoidType))
    assertPrintEquals("x.test.Test::m;I;I(5)",
        ApplyStatically(EAF, ref("x", "test.Test"), "test.Test",
            MethodName("m", List(I), I), List(i(5)))(IntType))
    assertPrintEquals("x.test.Test::m;I;I;I(5, 6)",
        ApplyStatically(EAF, ref("x", "test.Test"), "test.Test",
            MethodName("m", List(I, I), I), List(i(5), i(6)))(IntType))

    assertPrintEquals("x.test.Test::private::m;V()",
        ApplyStatically(EAF.withPrivate(true), ref("x", "test.Test"),
            "test.Test", MethodName("m", Nil, V), Nil)(VoidType))
  }

  @Test def printApplyStatic(): Unit = {
    assertPrintEquals("test.Test::m;V()",
        ApplyStatic(EAF, "test.Test", MethodName("m", Nil, V), Nil)(VoidType))
    assertPrintEquals("test.Test::m;I;I(5)",
        ApplyStatic(EAF, "test.Test", MethodName("m", List(I), I),
            List(i(5)))(IntType))
    assertPrintEquals("test.Test::m;I;I;I(5, 6)",
        ApplyStatic(EAF, "test.Test", MethodName("m", List(I, I), I),
            List(i(5), i(6)))(IntType))

    assertPrintEquals("test.Test::private::m;V()",
        ApplyStatic(EAF.withPrivate(true), "test.Test", MethodName("m", Nil, V),
            Nil)(VoidType))
  }

  @Test def printApplyDynamicImportStatic(): Unit = {
    assertPrintEquals("dynamicImport test.Test::m;Ljava.lang.Object()",
        ApplyDynamicImport(EAF, "test.Test", MethodName("m", Nil, O), Nil))
  }

  @Test def printApplyTypedClosure(): Unit = {
    assertPrintEquals("f()",
        ApplyTypedClosure(EAF, ref("f", NothingType), Nil))
    assertPrintEquals("f(1)",
        ApplyTypedClosure(EAF, ref("f", NothingType), List(i(1))))
    assertPrintEquals("f(1, 2)",
        ApplyTypedClosure(EAF, ref("f", NothingType), List(i(1), i(2))))
  }

  @Test def printNewLambda(): Unit = {
    assertPrintEquals(
        s"""
        |<newLambda>(
        |  extends java.lang.Object implements java.lang.Comparable,
        |  def compareTo;Ljava.lang.Object;Z(any): boolean,
        |  (typed-lambda<>(that: any): boolean = {
        |    true
        |  })
        |)
        """,
        NewLambda(
          NewLambda.Descriptor(
            ObjectClass,
            List("java.lang.Comparable"),
            MethodName(SimpleMethodName("compareTo"), List(ClassRef(ObjectClass)), BooleanRef),
            List(AnyType),
            BooleanType
          ),
          Closure(
            ClosureFlags.typed,
            Nil,
            List(ParamDef("that", NON, AnyType, mutable = false)),
            None,
            BooleanType,
            BooleanLiteral(true),
            Nil
          )
        )(ClassType("java.lang.Comparable", nullable = false))
    )
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

    assertPrintEquals("((float)x)", UnaryOp(LongToFloat, ref("x", LongType)))

    assertPrintEquals("x.length", UnaryOp(String_length, ref("x", StringType)))

    assertPrintEquals("x.notNull", UnaryOp(CheckNotNull, ref("x", AnyType)))

    val classVarRef = ref("x", ClassType(ClassClass, nullable = false))
    assertPrintEquals("x.name", UnaryOp(Class_name, classVarRef))
    assertPrintEquals("x.isPrimitive", UnaryOp(Class_isPrimitive, classVarRef))
    assertPrintEquals("x.isInterface", UnaryOp(Class_isInterface, classVarRef))
    assertPrintEquals("x.isArray", UnaryOp(Class_isArray, classVarRef))
    assertPrintEquals("x.componentType", UnaryOp(Class_componentType, classVarRef))
    assertPrintEquals("x.superClass", UnaryOp(Class_superClass, classVarRef))

    assertPrintEquals("x.length", UnaryOp(Array_length, ref("x", arrayType(IntRef, 1))))
    assertPrintEquals("x.getClass()", UnaryOp(GetClass, ref("x", AnyType)))
    assertPrintEquals("<clone>(x)", UnaryOp(Clone, ref("x", arrayType(ObjectClass, 1))))
    assertPrintEquals("<identityHashCode>(x)", UnaryOp(IdentityHashCode, ref("x", AnyType)))
    assertPrintEquals("<wrapAsThrowable>(e)", UnaryOp(WrapAsThrowable, ref("e", AnyType)))
    assertPrintEquals("<unwrapFromThrowable>(e)",
        UnaryOp(UnwrapFromThrowable, ref("e", ClassType(ThrowableClass, nullable = true))))
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

    assertPrintEquals("x[y]",
        BinaryOp(String_charAt, ref("x", StringType), ref("y", IntType)))

    val classVarRef = ref("x", ClassType(ClassClass, nullable = false))
    assertPrintEquals("isInstance(x, y)", BinaryOp(Class_isInstance, classVarRef, ref("y", AnyType)))
    assertPrintEquals("isAssignableFrom(x, y)",
        BinaryOp(Class_isAssignableFrom, classVarRef, ref("y", ClassType(ClassClass, nullable = false))))
    assertPrintEquals("cast(x, y)", BinaryOp(Class_cast, classVarRef, ref("y", AnyType)))
    assertPrintEquals("newArray(x, y)", BinaryOp(Class_newArray, classVarRef, ref("y", IntType)))
  }

  @Test def printNewArray(): Unit = {
    assertPrintEquals("new int[3]", NewArray(ArrayTypeRef(IntRef, 1), i(3)))
    assertPrintEquals("new int[3][]", NewArray(ArrayTypeRef(IntRef, 2), i(3)))
    assertPrintEquals("new java.lang.Object[3][][][]",
        NewArray(ArrayTypeRef(ObjectClass, 4), i(3)))
  }

  @Test def printArrayValue(): Unit = {
    assertPrintEquals("int[]()",
        ArrayValue(ArrayTypeRef(IntRef, 1), List()))
    assertPrintEquals("int[](5, 6)",
        ArrayValue(ArrayTypeRef(IntRef, 1), List(i(5), i(6))))

    assertPrintEquals("int[][](null)",
        ArrayValue(ArrayTypeRef(IntRef, 2), List(Null())))
  }

  @Test def printArraySelect(): Unit = {
    assertPrintEquals("x[3]",
        ArraySelect(ref("x", arrayType(IntRef, 1)), i(3))(IntType))
  }

  @Test def printRecordValue(): Unit = {
    assertPrintEquals("(x = 3, y = 4)",
        RecordValue(
            RecordType(List(
                RecordType.Field("x", NON, IntType, mutable = false),
                RecordType.Field("y", NON, IntType, mutable = true))),
            List(i(3), i(4))))
  }

  @Test def printIsInstanceOf(): Unit = {
    assertPrintEquals("x.isInstanceOf[java.lang.String!]",
        IsInstanceOf(ref("x", AnyType), ClassType(BoxedStringClass, nullable = false)))
    assertPrintEquals("x.isInstanceOf[int]",
        IsInstanceOf(ref("x", AnyType), IntType))
  }

  @Test def printAsInstanceOf(): Unit = {
    assertPrintEquals("x.asInstanceOf[java.lang.String]",
        AsInstanceOf(ref("x", AnyType), ClassType(BoxedStringClass, nullable = true)))
    assertPrintEquals("x.asInstanceOf[int]",
        AsInstanceOf(ref("x", AnyType), IntType))
  }

  @Test def printJSNew(): Unit = {
    assertPrintEquals("new C()", JSNew(ref("C", AnyType), Nil))
    assertPrintEquals("new C(4, 5)", JSNew(ref("C", AnyType), List(i(4), i(5))))
    assertPrintEquals("new x.test.Test::C(4, 5)",
        JSNew(JSPrivateSelect(ref("x", AnyType), FieldName("test.Test", "C")), List(i(4), i(5))))
    assertPrintEquals("""new x["C"]()""",
        JSNew(JSSelect(ref("x", AnyType), StringLiteral("C")), Nil))

    val fApplied = JSFunctionApply(ref("f", AnyType), Nil)
    assertPrintEquals("new (f())()", JSNew(fApplied, Nil))
    assertPrintEquals("new (f().test.Test::C)(4, 5)",
        JSNew(JSPrivateSelect(fApplied, FieldName("test.Test", "C")), List(i(4), i(5))))
    assertPrintEquals("""new (f()["C"])()""",
        JSNew(JSSelect(fApplied, StringLiteral("C")), Nil))
  }

  @Test def printJSPrivateSelect(): Unit = {
    assertPrintEquals("x.test.Test::f",
        JSPrivateSelect(ref("x", AnyType), FieldName("test.Test", "f")))
  }

  @Test def printJSSelect(): Unit = {
    assertPrintEquals("""x["f"]""",
        JSSelect(ref("x", AnyType), StringLiteral("f")))
  }

  @Test def printJSFunctionApply(): Unit = {
    assertPrintEquals("f()", JSFunctionApply(ref("f", AnyType), Nil))
    assertPrintEquals("f(3, 4)",
        JSFunctionApply(ref("f", AnyType), List(i(3), i(4))))

    assertPrintEquals("(0, x.test.Test::f)()",
        JSFunctionApply(JSPrivateSelect(ref("x", AnyType), FieldName("test.Test", "f")), Nil))
    assertPrintEquals("""(0, x["f"])()""",
        JSFunctionApply(JSSelect(ref("x", AnyType), StringLiteral("f")),
            Nil))
    assertPrintEquals("(0, x.test.Test::f)()",
        JSFunctionApply(Select(ref("x", "test.Test"), FieldName("test.Test", "f"))(AnyType),
            Nil))
  }

  @Test def printJSMethodApply(): Unit = {
    assertPrintEquals("""x["m"]()""",
        JSMethodApply(ref("x", AnyType), StringLiteral("m"), Nil))
    assertPrintEquals("""x["m"](4, 5)""",
        JSMethodApply(ref("x", AnyType), StringLiteral("m"),
            List(i(4), i(5))))
  }

  @Test def printJSSuperSelect(): Unit = {
    assertPrintEquals("""super(sc)::x["f"]""",
        JSSuperSelect(ref("sc", AnyType), ref("x", AnyType), StringLiteral("f")))
  }

  @Test def printJSSuperMethodCall(): Unit = {
    assertPrintEquals("""super(sc)::x["f"]()""",
        JSSuperMethodCall(ref("sc", AnyType), ref("x", AnyType), StringLiteral("f"), Nil))
  }

  @Test def printJSSuperConstructorCall(): Unit = {
    assertPrintEquals("super()", JSSuperConstructorCall(Nil))
    assertPrintEquals("super(4, 5)", JSSuperConstructorCall(List(i(4), i(5))))
  }

  @Test def printJSImportCall(): Unit = {
    assertPrintEquals("""import("foo.js")""", JSImportCall(StringLiteral("foo.js")))
  }

  @Test def printJSNewTarget(): Unit = {
    assertPrintEquals("new.target", JSNewTarget())
  }

  @Test def printJSImportMeta(): Unit = {
    assertPrintEquals("import.meta", JSImportMeta())
  }

  @Test def printLoadJSConstructor(): Unit = {
    assertPrintEquals("constructorOf[Test]", LoadJSConstructor("Test"))
  }

  @Test def printLoadJSModule(): Unit = {
    assertPrintEquals("mod:Test$", LoadJSModule("Test$"))
  }

  @Test def printJSSpread(): Unit = {
    assertPrintEquals("...x", JSSpread(ref("x", AnyType)))
  }

  @Test def printJSDelete(): Unit = {
    assertPrintEquals("""delete x["f"]""",
        JSDelete(ref("x", AnyType), StringLiteral("f")))
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

    assertPrintEquals("(x ** y)",
        JSBinaryOp(JSBinaryOp.**, ref("x", AnyType), ref("y", AnyType)))
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
          |  [x]: 5,
          |  "g": 6
          |}
        """,
        JSObjectConstr(List(ref("x", AnyType) -> i(5), StringLiteral("g") -> i(6))))
  }

  @Test def printGlobalRef(): Unit = {
    assertPrintEquals("global:Foo", JSGlobalRef("Foo"))
  }

  @Test def printJSTypeOfGlobalRef(): Unit = {
    assertPrintEquals("(typeof global:Foo)", JSTypeOfGlobalRef(JSGlobalRef("Foo")))
  }

  @Test def printUndefined(): Unit = {
    assertPrintEquals("undefined", Undefined())
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
    assertPrintEquals("classOf[Test]", ClassOf("Test"))
  }

  @Test def printVarRef(): Unit = {
    assertPrintEquals("x", VarRef("x")(IntType))
    assertPrintEquals("this", This()(AnyType))
  }

  @Test def printClosure(): Unit = {
    assertPrintEquals(
        """
          |(lambda<>(): any = {
          |  5
          |})
        """,
        Closure(ClosureFlags.function, Nil, Nil, None, AnyType, i(5), Nil))

    assertPrintEquals(
        """
          |(arrow-lambda<x: any = a, y{orig name}: int = 6>(z: any): any = {
          |  z
          |})
        """,
        Closure(
            ClosureFlags.arrow,
            List(
                ParamDef("x", NON, AnyType, mutable = false),
                ParamDef("y", TestON, IntType, mutable = false)),
            List(ParamDef("z", NON, AnyType, mutable = false)),
            None,
            AnyType,
            ref("z", AnyType),
            List(ref("a", IntType), i(6))))

    assertPrintEquals(
        """
          |(lambda<>(...z: any): any = {
          |  z
          |})
        """,
        Closure(ClosureFlags.function, Nil, Nil,
            Some(ParamDef("z", NON, AnyType, mutable = false)),
            AnyType, ref("z", AnyType), Nil))

    assertPrintEquals(
        """
          |(async lambda<>(...z: any): any = {
          |  z
          |})
        """,
        Closure(ClosureFlags.function.withAsync(true), Nil, Nil,
            Some(ParamDef("z", NON, AnyType, mutable = false)),
            AnyType, ref("z", AnyType), Nil))

    assertPrintEquals(
        """
          |(async arrow-lambda<>(...z: any): any = {
          |  z
          |})
        """,
        Closure(ClosureFlags.arrow.withAsync(true), Nil, Nil,
            Some(ParamDef("z", NON, AnyType, mutable = false)),
            AnyType, ref("z", AnyType), Nil))

    assertPrintEquals(
        """
          |(typed-lambda<>() {
          |  5
          |})
        """,
        Closure(ClosureFlags.typed, Nil, Nil, None, VoidType, i(5), Nil))

    assertPrintEquals(
        """
          |(typed-lambda<x: any = a, y{orig name}: int = 6>(z: int): int = {
          |  z
          |})
        """,
        Closure(
            ClosureFlags.typed,
            List(
                ParamDef("x", NON, AnyType, mutable = false),
                ParamDef("y", TestON, IntType, mutable = false)),
            List(ParamDef("z", NON, IntType, mutable = false)),
            None,
            IntType,
            ref("z", IntType),
            List(ref("a", IntType), i(6))))
  }

  @Test def printCreateJSClass(): Unit = {
    assertPrintEquals(
        """
          |createjsclass[Foo](x, y)
        """,
        CreateJSClass("Foo", List(ref("x", IntType), ref("y", AnyType))))
  }

  @Test def printLinkTimeProperty(): Unit = {
    assertPrintEquals(
        """
          |<linkTimeProperty>(foo)
        """,
        LinkTimeProperty("foo")(StringType))
  }

  @Test def printTransient(): Unit = {
    class MyTransient(expr: Tree) extends Transient.Value {
      val tpe: Type = AnyType

      def traverse(traverser: Traversers.Traverser): Unit = ???

      def transform(transformer: Transformers.Transformer)(
          implicit pos: Position): Tree = ???

      def printIR(out: Printers.IRTreePrinter): Unit = {
        out.print("mytransient(")
        out.print(expr)
        out.print(")")
      }
    }

    assertPrintEquals("mytransient(5)",
        Transient(new MyTransient(i(5))))
  }

  @Test def printClassDefKinds(): Unit = {
    import ClassKind._

    def makeForKind(kind: ClassKind): ClassDef = {
      ClassDef("Test", NON, kind, None, Some(ObjectClass), Nil, None, None, Nil,
          Nil, None, Nil, Nil, Nil)(
          NoOptHints)
    }

    assertPrintEquals(
        """
          |class Test extends java.lang.Object {
          |}
        """,
        makeForKind(Class))

    assertPrintEquals(
        """
          |module class Test extends java.lang.Object {
          |}
        """,
        makeForKind(ModuleClass))

    assertPrintEquals(
        """
          |interface Test extends java.lang.Object {
          |}
        """,
        makeForKind(Interface))

    assertPrintEquals(
        """
          |abstract js type Test extends java.lang.Object {
          |}
        """,
        makeForKind(AbstractJSType))

    assertPrintEquals(
        """
          |hijacked class Test extends java.lang.Object {
          |}
        """,
        makeForKind(HijackedClass))

    assertPrintEquals(
        """
          |js class Test extends java.lang.Object {
          |}
        """,
        makeForKind(JSClass))

    assertPrintEquals(
        """
          |js module class Test extends java.lang.Object {
          |}
        """,
        makeForKind(JSModuleClass))

    assertPrintEquals(
        """
          |native js class Test extends java.lang.Object {
          |}
        """,
        makeForKind(NativeJSClass))

    assertPrintEquals(
        """
          |native js module class Test extends java.lang.Object {
          |}
        """,
        makeForKind(NativeJSModuleClass))
  }

  @Test def printClassDefParents(): Unit = {
    def makeForParents(superClass: Option[ClassIdent],
        interfaces: List[ClassIdent]): ClassDef = {
      ClassDef("Test", NON, ClassKind.Class, None, superClass, interfaces, None,
          None, Nil, Nil, None, Nil, Nil, Nil)(
          NoOptHints)
    }

    assertPrintEquals(
        """
          |class Test {
          |}
        """,
        makeForParents(None, Nil))

    assertPrintEquals(
        """
          |class Test extends java.lang.Object implements Intf {
          |}
        """,
        makeForParents(Some(ObjectClass), List("Intf")))

    assertPrintEquals(
        """
          |class Test extends sr_AbstractFunction0 implements Intf1, Intf2 {
          |}
        """,
        makeForParents(Some("sr_AbstractFunction0"), List("Intf1", "Intf2")))
  }

  @Test def printClassDefJSNativeLoadSpec(): Unit = {
    assertPrintEquals(
        """
          |native js class Test extends java.lang.Object loadfrom global:Foo["Bar"] {
          |}
        """,
        ClassDef("Test", NON, ClassKind.NativeJSClass, None, Some(ObjectClass), Nil,
            None, Some(JSNativeLoadSpec.Global("Foo", List("Bar"))), Nil, Nil, None,
            Nil, Nil, Nil)(
            NoOptHints))

    assertPrintEquals(
        """
          |native js class Test extends java.lang.Object loadfrom import(foo)["Bar"] {
          |}
        """,
        ClassDef("Test", NON, ClassKind.NativeJSClass, None, Some(ObjectClass), Nil,
            None, Some(JSNativeLoadSpec.Import("foo", List("Bar"))), Nil, Nil, None,
            Nil, Nil, Nil)(
            NoOptHints))

    assertPrintEquals(
        """
          |native js class Test extends java.lang.Object loadfrom import(foo)["Bar"] fallback global:Baz["Foobar"] {
          |}
        """,
        ClassDef("Test", NON, ClassKind.NativeJSClass, None, Some(ObjectClass), Nil,
            None,
            Some(JSNativeLoadSpec.ImportWithGlobalFallback(
                JSNativeLoadSpec.Import("foo", List("Bar")),
                JSNativeLoadSpec.Global("Baz", List("Foobar")))), Nil, Nil, None,
            Nil, Nil, Nil)(
            NoOptHints))
  }

  @Test def printClassDefJSClassCaptures(): Unit = {
    assertPrintEquals(
        """
          |captures: none
          |js class Test extends java.lang.Object {
          |}
        """,
        ClassDef("Test", NON, ClassKind.JSClass, Some(Nil), Some(ObjectClass), Nil,
            None, None, Nil, Nil, None, Nil, Nil, Nil)(
            NoOptHints))

    assertPrintEquals(
        """
          |captures: x: int, y{orig name}: string
          |js class Test extends java.lang.Object {
          |}
        """,
        ClassDef("Test", NON, ClassKind.JSClass,
            Some(List(
                ParamDef("x", NON, IntType, mutable = false),
                ParamDef("y", TestON, StringType, mutable = false)
            )),
            Some(ObjectClass), Nil, None, None, Nil, Nil,  None, Nil, Nil, Nil)(
            NoOptHints))
  }

  @Test def printClassDefJSSuperClass(): Unit = {
    assertPrintEquals(
        """
          |captures: sup: any
          |js class Test extends Bar (via sup) {
          |}
        """,
        ClassDef("Test", NON, ClassKind.JSClass,
            Some(List(ParamDef("sup", NON, AnyType, mutable = false))),
            Some("Bar"), Nil, Some(ref("sup", AnyType)), None, Nil, Nil, None,
            Nil, Nil, Nil)(
            NoOptHints))
  }

  @Test def printClassDefOptimizerHints(): Unit = {
    assertPrintEquals(
        """
          |@hints(1) class Test extends java.lang.Object {
          |}
        """,
        ClassDef("Test", NON, ClassKind.Class, None, Some(ObjectClass), Nil,
            None, None, Nil, Nil, None, Nil, Nil, Nil)(
            NoOptHints.withInline(true)))
  }

  @Test def printClassDefOriginalName(): Unit = {
    assertPrintEquals(
        """
          |module class Test{orig name} extends java.lang.Object {
          |}
        """,
        ClassDef("Test", TestON, ClassKind.ModuleClass, None, Some(ObjectClass),
            Nil, None, None, Nil, Nil, None, Nil, Nil, Nil)(
            NoOptHints))
  }

  @Test def printClassDefDefs(): Unit = {
    assertPrintEquals(
        """
          |module class Test extends java.lang.Object {
          |  val Test::x: int
          |  def m;I(): int = <abstract>
          |  constructor def constructor(): any = {
          |    super()
          |  }
          |  def "o"(): any = {
          |    5
          |  }
          |  static native p;Ljava.lang.Object loadfrom global:foo
          |  export top[moduleID="main"] module "Foo"
          |}
        """,
        ClassDef("Test", NON, ClassKind.ModuleClass, None, Some(ObjectClass),
            Nil, None, None,
            List(FieldDef(MemberFlags.empty, FieldName("Test", "x"), NON, IntType)),
            List(MethodDef(MemberFlags.empty, MethodName("m", Nil, I), NON, Nil, IntType, None)(NoOptHints, UNV)),
            Some(JSConstructorDef(MemberFlags.empty.withNamespace(Constructor), Nil, None,
                JSConstructorBody(Nil, JSSuperConstructorCall(Nil), Nil))(NoOptHints, UNV)),
            List(JSMethodDef(MemberFlags.empty, StringLiteral("o"), Nil, None, i(5))(NoOptHints, UNV)),
            List(JSNativeMemberDef(MemberFlags.empty.withNamespace(Static), MethodName("p", Nil, O),
                JSNativeLoadSpec.Global("foo", Nil))),
            List(TopLevelModuleExportDef("main", "Foo")))(
            NoOptHints))
  }

  @Test def printFieldDef(): Unit = {
    assertPrintEquals("val Test::x: int",
        FieldDef(MemberFlags.empty, FieldName("Test", "x"), NON, IntType))
    assertPrintEquals("var Test::y: any",
        FieldDef(MemberFlags.empty.withMutable(true), FieldName("Test", "y"), NON, AnyType))
    assertPrintEquals("val Test::x{orig name}: int",
        FieldDef(MemberFlags.empty, FieldName("Test", "x"), TestON, IntType))
  }

  @Test def printJSFieldDef(): Unit = {
    assertPrintEquals("""val "x": int""",
        JSFieldDef(MemberFlags.empty, StringLiteral("x"), IntType))
    assertPrintEquals("""var "y": any""",
        JSFieldDef(MemberFlags.empty.withMutable(true), StringLiteral("y"), AnyType))

    assertPrintEquals("""static val "x": int""",
        JSFieldDef(MemberFlags.empty.withNamespace(Static), StringLiteral("x"), IntType))
    assertPrintEquals("""static var "y": any""",
        JSFieldDef(MemberFlags.empty.withNamespace(Static).withMutable(true), StringLiteral("y"), AnyType))
  }

  @Test def printMethodDef(): Unit = {
    val mIIMethodName = MethodName("m", List(I), I)
    val mIVMethodName = MethodName("m", List(I), V)

    assertPrintEquals(
        """
          |def m;I;I(x: int): int = <abstract>
        """,
        MethodDef(MemberFlags.empty, mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, None)(NoOptHints, UNV))

    assertPrintEquals(
        """
          |def m;I;I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, Some(i(5)))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |@hints(1) def m;I;I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, Some(i(5)))(NoOptHints.withInline(true), UNV))

    assertPrintEquals(
        """
          |def m;I;V(x: int) {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty, mIVMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            VoidType, Some(i(5)))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |static def m;I;I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty.withNamespace(Static), mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, Some(i(5)))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |private def m;I;I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty.withNamespace(Private), mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, Some(i(5)))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |private static def m;I;I(x: int): int = {
          |  5
          |}
        """,
        MethodDef(MemberFlags.empty.withNamespace(PrivateStatic), mIIMethodName, NON,
            List(ParamDef("x", NON, IntType, mutable = false)),
            IntType, Some(i(5)))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |def m;I;I{orig name}(x{orig name}: int): int = <abstract>
        """,
        MethodDef(MemberFlags.empty, mIIMethodName, TestON,
            List(ParamDef("x", TestON, IntType, mutable = false)),
            IntType, None)(NoOptHints, UNV))
  }

  @Test def printJSConstructorDef(): Unit = {
    assertPrintEquals(
        """
          |constructor def constructor(x: any): any = {
          |  5;
          |  super(6);
          |  undefined
          |}
        """,
        JSConstructorDef(MemberFlags.empty.withNamespace(Constructor),
            List(ParamDef("x", NON, AnyType, mutable = false)), None,
            JSConstructorBody(List(i(5)), JSSuperConstructorCall(List(i(6))), List(Undefined())))(
            NoOptHints, UNV))

    assertPrintEquals(
        """
          |constructor def constructor(x: any, ...y: any): any = {
          |  super(6);
          |  7
          |}
        """,
        JSConstructorDef(MemberFlags.empty.withNamespace(Constructor),
            List(ParamDef("x", NON, AnyType, mutable = false)),
            Some(ParamDef("y", NON, AnyType, mutable = false)),
            JSConstructorBody(Nil, JSSuperConstructorCall(List(i(6))), List(i(7))))(
            NoOptHints, UNV))

    // This example is an invalid constructor, but it should be printed anyway
    assertPrintEquals(
        """
          |def constructor(x{orig name}: any): any = {
          |  5;
          |  super(6)
          |}
        """,
        JSConstructorDef(MemberFlags.empty,
            List(ParamDef("x", TestON, AnyType, mutable = false)), None,
            JSConstructorBody(List(i(5)), JSSuperConstructorCall(List(i(6))), Nil))(
            NoOptHints, UNV))
  }

  @Test def printJSMethodDef(): Unit = {
    assertPrintEquals(
        """
          |def "m"(x: any): any = {
          |  5
          |}
        """,
        JSMethodDef(MemberFlags.empty, StringLiteral("m"),
            List(ParamDef("x", NON, AnyType, mutable = false)), None,
            i(5))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |def "m"(x: any, ...y: any): any = {
          |  5
          |}
        """,
        JSMethodDef(MemberFlags.empty, StringLiteral("m"),
            List(ParamDef("x", NON, AnyType, mutable = false)),
            Some(ParamDef("y", NON, AnyType, mutable = false)),
            i(5))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |static def "m"(x: any): any = {
          |  5
          |}
        """,
        JSMethodDef(MemberFlags.empty.withNamespace(Static), StringLiteral("m"),
            List(ParamDef("x", NON, AnyType, mutable = false)), None,
            i(5))(NoOptHints, UNV))

    assertPrintEquals(
        """
          |def "m"(x{orig name}: any): any = {
          |  5
          |}
        """,
        JSMethodDef(MemberFlags.empty, StringLiteral("m"),
            List(ParamDef("x", TestON, AnyType, mutable = false)), None,
            i(5))(NoOptHints, UNV))
  }

  @Test def printJSPropertyDef(): Unit = {
    for (static <- Seq(false, true)) {
      val staticStr =
        if (static) "static "
        else ""
      val flags =
        if (static) MemberFlags.empty.withNamespace(Static)
        else MemberFlags.empty

      assertPrintEquals(
          s"""
            |${staticStr}get "prop"(): any = {
            |  5
            |}
          """,
          JSPropertyDef(flags, StringLiteral("prop"), Some(i(5)), None)(UNV))

      assertPrintEquals(
          s"""
            |${staticStr}set "prop"(x: any) {
            |  7
            |}
          """,
          JSPropertyDef(flags, StringLiteral("prop"),
              None,
              Some((ParamDef("x", NON, AnyType, mutable = false), i(7))))(UNV))

      assertPrintEquals(
          s"""
            |${staticStr}set "prop"(x{orig name}: any) {
            |  7
            |}
          """,
          JSPropertyDef(flags, StringLiteral("prop"),
              None,
              Some((ParamDef("x", TestON, AnyType, mutable = false), i(7))))(UNV))

      assertPrintEquals(
          s"""
            |${staticStr}get "prop"(): any = {
            |  5
            |}
            |${staticStr}set "prop"(x: any) {
            |  7
            |}
          """,
          JSPropertyDef(flags, StringLiteral("prop"),
              Some(i(5)),
              Some((ParamDef("x", NON, AnyType, mutable = false),
                  i(7))))(UNV))
    }
  }

  @Test def printJSClassExportDef(): Unit = {
    assertPrintEquals(
        """export top[moduleID="my-mod"] class "Foo"""",
        TopLevelJSClassExportDef("my-mod", "Foo"))
  }

  @Test def printTopLevelModuleExportDef(): Unit = {
    assertPrintEquals(
        """export top[moduleID="bar"] module "Foo"""",
        TopLevelModuleExportDef("bar", "Foo"))
  }

  @Test def printTopLevelMethodExportDef(): Unit = {
    assertPrintEquals(
        """
          |export top[moduleID="main"] static def "foo"(x: any): any = {
          |  5
          |}""",
        TopLevelMethodExportDef("main", JSMethodDef(
            MemberFlags.empty.withNamespace(Static), StringLiteral("foo"),
            List(ParamDef("x", NON, AnyType, mutable = false)), None,
            i(5))(NoOptHints, UNV)))
  }

  @Test def printTopLevelFieldExportDef(): Unit = {
    assertPrintEquals(
        """
          |export top[moduleID="main"] static field Test::x$1 as "x"
        """,
        TopLevelFieldExportDef("main", "x", FieldName("Test", "x$1")))
  }
}
