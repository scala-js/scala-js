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

package org.scalajs.linker

import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.EntryPointsInfo
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.logging.NullLogger
import org.scalajs.linker._
import org.scalajs.linker.analyzer._
import org.scalajs.linker.frontend.IRLoader
import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.standard._

import Analysis._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class AnalyzerTest {
  import scala.concurrent.ExecutionContext.Implicits.global
  import AnalyzerTest._

  @Test
  def trivialOK(): AsyncResult = await {
    val analysis = computeAnalysis(Nil)
    assertNoError(analysis)
  }

  @Test
  def missingJavaLangObject(): AsyncResult = await {
    val analysis = computeAnalysis(Nil, stdlib = TestIRRepo.empty)
    assertExactErrors(analysis, MissingJavaLangObjectClass(fromAnalyzer))
  }

  @Test
  def invalidJavaLangObject(): AsyncResult = await {
    val invalidJLObjectDefs = Seq(
        // j.l.Object cannot have a super class
        classDef(ObjectClass, superClass = Some("Parent")),
        // j.l.Object must have kind == ClassKind.Class
        classDef(ObjectClass, kind = ClassKind.ModuleClass),
        // j.l.Object cannot extend any interface
        classDef(ObjectClass, interfaces = List("Parent"))
    )

    Future.traverse(invalidJLObjectDefs) { jlObjectDef =>
      val analysis = computeAnalysis(Seq(jlObjectDef), stdlib = TestIRRepo.empty)
      assertExactErrors(analysis,
          InvalidJavaLangObjectClass(fromAnalyzer))
    }
  }

  @Test
  def cycleInInheritanceChainThroughParentClasses(): AsyncResult = await {
    val classDefs = Seq(
        classDef("A", superClass = Some("B")),
        classDef("B", superClass = Some("A"))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.classData("A"))

    assertContainsError("CycleInInheritanceChain(A, B)", analysis) {
      case CycleInInheritanceChain(List(AClass, BClass), `fromAnalyzer`) => true
    }
  }

  @Test
  def cycleInInheritanceChainThroughInterfaces(): AsyncResult = await {
    val classDefs = Seq(
        classDef("A", superClass = Some("B")),
        classDef("B", superClass = Some(ObjectClass), interfaces = List("A"))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.classData("A"))

    assertContainsError("CycleInInheritanceChain(A, B)", analysis) {
      case CycleInInheritanceChain(List(AClass, BClass), `fromAnalyzer`) => true
    }
  }

  @Test
  def bigCycleInInheritanceChain(): AsyncResult = await {
    val classDefs = Seq(
        classDef("A", superClass = Some("B")),
        classDef("B", superClass = Some("C")),

        // Start of cycle.
        classDef("C", superClass = Some("D")),
        classDef("D", superClass = Some("E")),
        classDef("E", superClass = Some("C"))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.classData("A"))

    assertContainsError("CycleInInheritanceChain(B, C, D)", analysis) {
      case CycleInInheritanceChain(List(CClass, DClass, EClass), `fromAnalyzer`) => true
    }
  }

  @Test
  def missingClassDirect(): AsyncResult = await {
    val analysis = computeAnalysis(Nil, reqsFactory.classData("A"))

    assertContainsError("MissingClass(A)", analysis) {
      case MissingClass(ClsInfo("A"), `fromUnitTest`) => true
    }
  }

  @Test
  def missingClassParent(): AsyncResult = await {
    val classDefs = Seq(
        classDef("A", superClass = Some("B"))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.classData("A"))

    assertContainsError("MissingClass(B)", analysis) {
      case MissingClass(ClsInfo("B"), FromClass(ClsInfo("A"))) => true
    }
  }

  @Test
  def missingSuperClass(): AsyncResult = await {
    val kinds = Seq(
        ClassKind.Class,
        ClassKind.ModuleClass,
        ClassKind.HijackedClass,
        ClassKind.JSClass,
        ClassKind.JSModuleClass,
        ClassKind.NativeJSClass,
        ClassKind.NativeJSModuleClass
    )

    Future.traverse(kinds) { kind =>
      val classDefs = Seq(
          classDef("A", kind = kind, memberDefs = List(trivialCtor("A")))
      )

      val analysis = computeAnalysis(classDefs,
          reqsFactory.instantiateClass("A", NoArgConstructorName))

      assertContainsError("MissingSuperClass(A)", analysis) {
        case MissingSuperClass(ClsInfo("A"), FromClass(ClsInfo("A"))) => true
      }
    }
  }

  @Test
  def invalidSuperClass(): AsyncResult = await {
    val kindsSub = Seq(
        ClassKind.Class,
        ClassKind.ModuleClass,
        ClassKind.HijackedClass,
        ClassKind.Interface,
        ClassKind.JSClass,
        ClassKind.JSModuleClass,
        ClassKind.NativeJSClass,
        ClassKind.NativeJSModuleClass,
        ClassKind.AbstractJSType
    )

    def kindsBaseFor(kindSub: ClassKind): Seq[ClassKind] = {
      import ClassKind._
      kindSub match {
        case Class | ModuleClass | HijackedClass =>
          Seq(Interface, ModuleClass, JSClass, NativeJSClass)
        case Interface =>
          Seq(Class, Interface)
        case JSClass | JSModuleClass | NativeJSClass | NativeJSModuleClass |
            AbstractJSType =>
          Seq(Class, Interface, AbstractJSType, JSModuleClass)
      }
    }

    Future.traverse(kindsSub) { kindSub =>
      Future.traverse(kindsBaseFor(kindSub)) { kindBase =>

        val classDefs = Seq(
            classDef("A", kind = kindSub, superClass = Some("B")),
            classDef("B", kind = kindBase,
                superClass = validParentForKind(kindBase))
        )

        val analysis = computeAnalysis(classDefs,
            reqsFactory.instantiateClass("A", NoArgConstructorName))

        assertContainsError("InvalidSuperClass(B, A)", analysis) {
          case InvalidSuperClass(ClsInfo("B"), ClsInfo("A"),
              FromClass(ClsInfo("A"))) =>
            true
        }
      }
    }
  }

  @Test
  def invalidImplementedInterface(): AsyncResult = await {
    val kindsCls = Seq(
        ClassKind.Class,
        ClassKind.ModuleClass,
        ClassKind.HijackedClass,
        ClassKind.Interface,
        ClassKind.JSClass,
        ClassKind.JSModuleClass,
        ClassKind.NativeJSClass,
        ClassKind.NativeJSModuleClass,
        ClassKind.AbstractJSType
    )

    def kindsIntfFor(kindCls: ClassKind): Seq[ClassKind] = {
      import ClassKind._
      kindCls match {
        case Class | ModuleClass | HijackedClass | Interface =>
          Seq(Class, ModuleClass, JSClass, NativeJSClass, AbstractJSType)
        case JSClass | JSModuleClass | NativeJSClass | NativeJSModuleClass |
            AbstractJSType =>
          Seq(Class, ModuleClass, HijackedClass, Interface, JSClass,
              JSModuleClass, NativeJSClass, NativeJSModuleClass)
      }
    }

    Future.traverse(kindsCls) { kindCls =>
      Future.traverse(kindsIntfFor(kindCls)) { kindIntf =>
        val classDefs = Seq(
            classDef("A", kind = kindCls,
                superClass = validParentForKind(kindCls),
                interfaces = List("B")),
            classDef("B", kind = kindIntf,
                superClass = validParentForKind(kindIntf))
        )

        val analysis = computeAnalysis(classDefs,
            reqsFactory.instantiateClass("A", NoArgConstructorName))

        assertContainsError("InvalidImplementedInterface(B, A)", analysis) {
          case InvalidImplementedInterface(ClsInfo("B"), ClsInfo("A"),
              FromClass(ClsInfo("A"))) =>
            true
        }
      }
    }
  }

  @Test
  def notAModule(): AsyncResult = await {
    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            memberDefs = List(trivialCtor("A")))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.accessModule("A"))

    assertContainsError("NotAModule(A)", analysis) {
      case NotAModule(ClsInfo("A"), `fromUnitTest`) => true
    }
  }

  @Test
  def missingMethod(): AsyncResult = await {
    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            memberDefs = List(trivialCtor("A")))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("A", NoArgConstructorName) ++
        reqsFactory.callMethod("A", m("foo", Nil, V)))

    assertContainsError("MissingMethod(A.foo;V)", analysis) {
      case MissingMethod(MethInfo("A", "foo;V"), `fromUnitTest`) => true
    }
  }

  @Test
  def missingAbstractMethod(): AsyncResult = await {
    val fooMethodName = m("foo", Nil, IntRef)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            memberDefs = List(trivialCtor("A"))),
        classDef("B", superClass = Some("A"),
            memberDefs = List(
                trivialCtor("B"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, Some(int(5)))(EOH, None)
            ))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("B", NoArgConstructorName) ++
        reqsFactory.callMethod("A", fooMethodName))

    assertContainsError("MissingMethod(A.foo;I)", analysis) {
      case MissingMethod(MethInfo("A", "foo;I"), `fromUnitTest`) => true
    }
  }

  @Test
  def conflictingDefaultMethods(): AsyncResult = await {
    val defaultMethodDef = MethodDef(EMF, m("foo", Nil, V), NON, Nil,
        NoType, Some(Skip()))(EOH, None)
    val classDefs = Seq(
        classDef("I1", kind = ClassKind.Interface,
            memberDefs = List(defaultMethodDef)),
        classDef("I2", kind = ClassKind.Interface,
            memberDefs = List(defaultMethodDef)),
        classDef("A", superClass = Some(ObjectClass),
            interfaces = List("I1", "I2"),
            memberDefs = List(trivialCtor("A")))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("A", NoArgConstructorName) ++
        reqsFactory.callMethod("A", m("foo", Nil, V)))

    assertContainsError("ConflictingDefaultMethods(I1.foo;V, I2.foo;V)", analysis) {
      case ConflictingDefaultMethods(
          List(MethInfo("I1", "foo;V"), MethInfo("I2", "foo;V")),
          `fromAnalyzer`) =>
        true
      case ConflictingDefaultMethods(
          List(MethInfo("I2", "foo;V"), MethInfo("I1", "foo;V")),
          `fromAnalyzer`) =>
        true
    }
  }

  @Test
  def conflictingTopLevelExports(): AsyncResult = await {
    def singleDef(name: String) = {
      classDef(name,
          kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
          memberDefs = List(trivialCtor(name)),
          topLevelExportDefs = List(TopLevelModuleExportDef("foo")))
    }

    val classDefs = Seq(singleDef("A"), singleDef("B"))
    val analysis = computeAnalysis(classDefs)

    assertContainsError("ConflictingTopLevelExport(foo, A, B)", analysis) {
      case ConflictingTopLevelExport("foo", List(ClsName("A"), ClsName("B"))) =>
        true
      case ConflictingTopLevelExport("foo", List(ClsName("B"), ClsName("A"))) =>
        true
    }
  }

  @Test
  def degenerateConflictingTopLevelExports(): AsyncResult = await {
    val classDefs = Seq(classDef("A",
        kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
        memberDefs = List(trivialCtor("A")),
        topLevelExportDefs = List(
            TopLevelModuleExportDef("foo"),
            TopLevelModuleExportDef("foo"))))

    val analysis = computeAnalysis(classDefs)
    assertContainsError("ConflictingTopLevelExport(foo, <degenerate>)", analysis) {
      case ConflictingTopLevelExport("foo", _) => true
    }
  }

  @Test
  def juPropertiesNotReachableWhenUsingGetSetClearProperty(): AsyncResult = await {
    val systemMod = LoadModule("java.lang.System$")
    val emptyStr = StringLiteral("")
    val StringType = ClassType(BoxedStringClass)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass), memberDefs = List(
            trivialCtor("A"),
            MethodDef(EMF, m("test", Nil, V), NON, Nil, NoType, Some(Block(
                Apply(EAF, systemMod, m("getProperty", List(T), T), List(emptyStr))(StringType),
                Apply(EAF, systemMod, m("getProperty", List(T, T), T), List(emptyStr))(StringType),
                Apply(EAF, systemMod, m("setProperty", List(T, T), T), List(emptyStr))(StringType),
                Apply(EAF, systemMod, m("clearProperty", List(T), T), List(emptyStr))(StringType)
            )))(EOH, None)
        ))
    )

    for {
      analysis <- computeAnalysis(classDefs,
          reqsFactory.instantiateClass("A", NoArgConstructorName) ++
          reqsFactory.callMethod("A", m("test", Nil, V)),
          stdlib = TestIRRepo.fulllib)
    } yield {
      assertNoError(analysis)

      val juPropertiesClass = analysis.classInfos("java.util.Properties")
      assertFalse(juPropertiesClass.isAnySubclassInstantiated)
      assertFalse(juPropertiesClass.areInstanceTestsUsed)
      assertFalse(juPropertiesClass.isDataAccessed)
    }
  }

  @Test  // #3571
  def specificReflectiveProxy(): AsyncResult = await {
    val fooAMethodName = m("foo", Nil, ClassRef("A"))
    val fooBMethodName = m("foo", Nil, ClassRef("B"))

    val fooReflProxyName =
      MethodName.reflectiveProxy(SimpleMethodName("foo"), Nil)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass)),
        classDef("B", superClass = Some("A")),
        classDef("X", superClass = Some(ObjectClass),
            memberDefs = List(
                trivialCtor("X"),
                MethodDef(EMF, fooAMethodName, NON, Nil, ClassType("A"),
                    Some(Null()))(EOH, None),
                MethodDef(EMF, fooBMethodName, NON, Nil, ClassType("B"),
                    Some(Null()))(EOH, None)
            )
        )
    )

    for {
      analysis <- computeAnalysis(classDefs,
          reqsFactory.instantiateClass("X", NoArgConstructorName) ++
          reqsFactory.callMethod("X", fooReflProxyName))
    } yield {
      assertNoError(analysis)

      val MethodSyntheticKind.ReflectiveProxy(target) = {
        analysis.classInfos("X")
          .methodInfos(MemberNamespace.Public)(fooReflProxyName)
          .syntheticKind
      }

      assertEquals(fooBMethodName, target)
    }
  }

  @Test
  def isAbstractReachable(): AsyncResult = await {
    val fooMethodName = m("foo", Nil, IntRef)
    val barMethodName = m("bar", Nil, IntRef)

    val classDefs = Seq(
        classDef("I1", kind = ClassKind.Interface,
            memberDefs = List(
                MethodDef(EMF, barMethodName, NON, Nil, IntType, None)(EOH, None)
            )),
        classDef("I2", kind = ClassKind.Interface,
            memberDefs = List(
                MethodDef(EMF, barMethodName, NON, Nil, IntType, None)(EOH, None)
            )),
        classDef("A", superClass = Some(ObjectClass), interfaces = List("I1"),
            memberDefs = List(
                trivialCtor("A"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, None)(EOH, None)
            )),
        classDef("B", superClass = Some("A"), interfaces = List("I2"),
            memberDefs = List(
                trivialCtor("B"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, Some(int(5)))(EOH, None)
            )),
        classDef("C", superClass = Some("B"),
            memberDefs = List(
                trivialCtor("C"),
                MethodDef(EMF, barMethodName, NON, Nil, IntType, Some(int(5)))(EOH, None)
            ))
    )

    val analysisFuture = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("C", NoArgConstructorName) ++
        reqsFactory.callMethod("A", fooMethodName) ++
        reqsFactory.callMethod("B", barMethodName))

    for (analysis <- analysisFuture) yield {
      assertNoError(analysis)

      val BClassInfo = analysis.classInfos("C")
      assertEquals(List[ClassName]("C", "B", "A", ObjectClass, "I1", "I2"),
          BClassInfo.ancestors.map(_.className))

      val AfooMethodInfo = analysis.classInfos("A")
        .methodInfos(MemberNamespace.Public)(fooMethodName)
      assertTrue(AfooMethodInfo.isAbstractReachable)

      val I1barMethodInfo = analysis.classInfos("I1")
        .methodInfos(MemberNamespace.Public)(barMethodName)
      assertTrue(I1barMethodInfo.isAbstractReachable)

      val I2barMethodInfo = analysis.classInfos("I2")
        .methodInfos(MemberNamespace.Public)(barMethodName)
      assertFalse(I2barMethodInfo.isAbstractReachable)
    }
  }
}

object AnalyzerTest {
  private val reqsFactory = SymbolRequirement.factory("unit test")

  private val fromAnalyzer = FromCore("analyzer")
  private val fromUnitTest = FromCore("unit test")

  private val AClass = ClassName("A")
  private val BClass = ClassName("B")
  private val CClass = ClassName("C")
  private val DClass = ClassName("D")
  private val EClass = ClassName("E")

  private def validParentForKind(kind: ClassKind): Option[ClassName] = {
    import ClassKind._
    kind match {
      case Class | ModuleClass | HijackedClass | NativeJSClass |
          NativeJSModuleClass =>
        Some(ObjectClass)
      case JSClass | JSModuleClass =>
        Some(ClassName("scala.scalajs.js.Object"))
      case Interface | AbstractJSType =>
        None
    }
  }

  private def computeAnalysis(classDefs: Seq[ClassDef],
      symbolRequirements: SymbolRequirement = reqsFactory.none(),
      stdlib: Future[Seq[IRFile]] = TestIRRepo.minilib)(
      implicit ec: ExecutionContext): Future[Analysis] = {
    for {
      baseFiles <- stdlib
      irLoader <- new IRLoader().update(classDefs.map(MemClassDefIRFile(_)) ++ baseFiles)
      analysis <- Analyzer.computeReachability(CommonPhaseConfig(),
          symbolRequirements, allowAddingSyntheticMethods = true,
          checkAbstractReachability = true, irLoader)
    } yield {
      analysis
    }
  }

  private def assertNoError(analysis: Future[Analysis])(
      implicit ec: ExecutionContext): Future[Unit] = {
    assertExactErrors(analysis)
  }

  private def assertNoError(analysis: Analysis): Unit =
    assertExactErrors(analysis)

  private def assertExactErrors(analysis: Future[Analysis],
      expectedErrors: Error*)(implicit ec: ExecutionContext): Future[Unit] = {
    analysis.map(assertExactErrors(_, expectedErrors: _*))
  }

  private def assertExactErrors(analysis: Analysis,
      expectedErrors: Error*): Unit = {
    val actualErrors = analysis.errors

    for (expectedError <- expectedErrors) {
      assertTrue(s"Missing expected error: $expectedError",
          actualErrors.contains(expectedError))
    }

    if (actualErrors.size != expectedErrors.size) {
      for (actualError <- actualErrors) {
        assertTrue(s"Unexpected error: $actualError",
            expectedErrors.contains(actualError))
      }
    }
  }

  private def assertContainsError(msg: String, analysis: Future[Analysis])(
      pf: PartialFunction[Error, Boolean])(
      implicit ec: ExecutionContext): Future[Unit] = {
    analysis.map(assertContainsError(msg, _)(pf))
  }

  private def assertContainsError(msg: String, analysis: Analysis)(
      pf: PartialFunction[Error, Boolean]): Unit = {
    val fullMessage = s"Expected $msg, got ${analysis.errors}"
    assertTrue(fullMessage, analysis.errors.exists {
      e => pf.applyOrElse(e, (_: Error) => false)
    })
  }

  object ClsInfo {
    def unapply(classInfo: Analysis.ClassInfo): Some[String] =
      Some(classInfo.className.nameString)
  }

  object MethInfo {
    def unapply(methodInfo: Analysis.MethodInfo): Some[(String, String)] =
      Some((methodInfo.owner.className.nameString, methodInfo.methodName.nameString))
  }

  object ClsName {
    def unapply(className: ClassName): Some[String] =
      Some(className.nameString)
  }
}
