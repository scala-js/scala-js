package org.scalajs.core.tools.linker

import org.junit.Test
import org.junit.Assert._

import org.scalajs.core.ir
import org.scalajs.core.ir.ClassKind
import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.Trees._
import org.scalajs.core.ir.Types._

import org.scalajs.core.tools.logging.NullLogger
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.linker._
import org.scalajs.core.tools.linker.analyzer._
import org.scalajs.core.tools.linker.standard._

import Analysis._

import org.scalajs.core.tools.linker.testutils._

class AnalyzerTest {
  import AnalyzerTest._

  private val reqsFactory = SymbolRequirement.factory("unit test")

  private val emptyOptHints: OptimizerHints = OptimizerHints.empty

  implicit private val noPosition: ir.Position = ir.Position.NoPosition

  private val fromAnalyzer = FromCore("analyzer")
  private val fromUnitTest = FromCore("unit test")

  private def classDef(
      encodedName: String,
      kind: ClassKind = ClassKind.Class,
      superClass: Option[String] = None,
      interfaces: List[String] = Nil,
      jsNativeLoadSpec: Option[JSNativeLoadSpec] = None,
      memberDefs: List[MemberDef] = Nil,
      topLevelExportDefs: List[TopLevelExportDef] = Nil): ClassDef = {
    ClassDef(Ident(encodedName), kind, superClass.map(Ident(_)),
        interfaces.map(Ident(_)), jsNativeLoadSpec, memberDefs,
        topLevelExportDefs)(
        emptyOptHints)
  }

  private def trivialCtor(enclosingClassName: String): MethodDef = {
    MethodDef(static = false, Ident("init___"), Nil, NoType,
        Some(ApplyStatically(This()(ClassType(enclosingClassName)),
            ClassType(ObjectClass), Ident("init___"), Nil)(NoType)))(
        emptyOptHints, None)
  }

  @Test
  def trivialOK(): Unit = {
    val analysis = computeAnalysis(Nil)
    assertNoError(analysis)
  }

  @Test
  def missingJavaLangObject(): Unit = {
    val analysis = computeAnalysis(Nil, stdlib = false)
    assertExactErrors(analysis,
        MissingJavaLangObjectClass(fromAnalyzer))
  }

  @Test
  def invalidJavaLangObject(): Unit = {
    val invalidJLObjectDefs = Seq(
        // j.l.Object cannot have a super class
        classDef(ObjectClass, superClass = Some("Lparent")),
        // j.l.Object must have kind == ClassKind.Class
        classDef(ObjectClass, kind = ClassKind.ModuleClass),
        // j.l.Object cannot extend any interface
        classDef(ObjectClass, interfaces = List("Lparent"))
    )

    for (jlObjectDef <- invalidJLObjectDefs) {
      val analysis = computeAnalysis(Seq(jlObjectDef), stdlib = false)
      assertExactErrors(analysis,
          InvalidJavaLangObjectClass(fromAnalyzer))
    }
  }

  @Test
  def cycleInInheritanceChainThroughParentClasses(): Unit = {
    val classDefs = Seq(
        classDef("LA", superClass = Some("LB")),
        classDef("LB", superClass = Some("LA"))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.classData("LA"))

    assertContainsError("CycleInInheritanceChain(LA, LB)", analysis) {
      case CycleInInheritanceChain(List(ClsInfo("LA"), ClsInfo("LB")),
          `fromUnitTest`) =>
        true
    }
  }

  @Test
  def cycleInInheritanceChainThroughInterfaces(): Unit = {
    val classDefs = Seq(
        classDef("LA", superClass = Some("LB")),
        classDef("LB", superClass = Some(ObjectClass), interfaces = List("LA"))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.classData("LA"))

    assertContainsError("CycleInInheritanceChain(LA, LB)", analysis) {
      case CycleInInheritanceChain(List(ClsInfo("LA"), ClsInfo("LB")),
          `fromUnitTest`) =>
        true
    }
  }

  @Test
  def missingClassDirect(): Unit = {
    val analysis = computeAnalysis(Nil, reqsFactory.classData("LA"))

    assertContainsError("MissingClass(LA)", analysis) {
      case MissingClass(ClsInfo("LA"), `fromUnitTest`) => true
    }
  }

  @Test
  def missingClassParent(): Unit = {
    val classDefs = Seq(
        classDef("LA", superClass = Some("LB"))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.classData("LA"))

    assertContainsError("MissingClass(LB)", analysis) {
      case MissingClass(ClsInfo("LB"), FromClass(ClsInfo("LA"))) => true
    }
  }

  @Test
  def notAModule(): Unit = {
    val classDefs = Seq(
        classDef("LA", superClass = Some(ObjectClass),
            memberDefs = List(trivialCtor("LA")))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.accessModule("LA"))

    assertContainsError("NotAModule(LA)", analysis) {
      case NotAModule(ClsInfo("LA"), `fromUnitTest`) => true
    }
  }

  @Test
  def missingMethod(): Unit = {
    val classDefs = Seq(
        classDef("LA", superClass = Some(ObjectClass),
            memberDefs = List(trivialCtor("LA")))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("LA", "init___") ++
        reqsFactory.callMethod("LA", "foo__V"))

    assertContainsError("MissingMethod(LA.foo__V)", analysis) {
      case MissingMethod(MethInfo("LA", "foo__V"), `fromUnitTest`) => true
    }
  }

  @Test
  def conflictingDefaultMethods(): Unit = {
    val defaultMethodDef = MethodDef(static = false, Ident("foo__V"), Nil,
        NoType, Some(Skip()))(emptyOptHints, None)
    val classDefs = Seq(
        classDef("LI1", kind = ClassKind.Interface,
            memberDefs = List(defaultMethodDef)),
        classDef("LI2", kind = ClassKind.Interface,
            memberDefs = List(defaultMethodDef)),
        classDef("LA", superClass = Some(ObjectClass),
            interfaces = List("LI1", "LI2"),
            memberDefs = List(trivialCtor("LA")))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("LA", "init___") ++
        reqsFactory.callMethod("LA", "foo__V"))

    assertContainsError("ConflictingDefaultMethods(LI1.foo__V, LI2.foo__V)",
        analysis) {
      case ConflictingDefaultMethods(
          List(MethInfo("LI1", "foo__V"), MethInfo("LI2", "foo__V")),
          `fromAnalyzer`) =>
        true
      case ConflictingDefaultMethods(
          List(MethInfo("LI2", "foo__V"), MethInfo("LI1", "foo__V")),
          `fromAnalyzer`) =>
        true
    }
  }

  private def computeAnalysis(classDefs: Seq[ClassDef],
      symbolRequirements: SymbolRequirement = reqsFactory.none(),
      stdlib: Boolean = true): Analysis = {

    val classesWithEntryPoints0 = classDefs
      .map(ir.EntryPointsInfo.forClassDef)
      .withFilter(_.hasEntryPoint)
      .map(_.encodedName)

    val encodedNameToInfo =
      classDefs.map(c => c.name.name -> Infos.generateClassInfo(c)).toMap

    val inputProvider = new Analyzer.InputProvider {
      def classesWithEntryPoints(): TraversableOnce[String] =
        classesWithEntryPoints0

      def loadInfo(encodedName: String): Option[Infos.ClassInfo] = {
        val own = encodedNameToInfo.get(encodedName)
        if (stdlib) own.orElse(TestIRRepo.loadInfo(encodedName))
        else own
      }
    }

    Analyzer.computeReachability(CommonPhaseConfig(), symbolRequirements,
        allowAddingSyntheticMethods = true, inputProvider)
  }
}

object AnalyzerTest {
  private def assertNoError(analysis: Analysis): Unit =
    assertExactErrors(analysis)

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

  private def assertContainsError(msg: String, analysis: Analysis)(
      pf: PartialFunction[Error, Boolean]): Unit = {
    val fullMessage = s"Expected $msg, got ${analysis.errors}"
    assertTrue(fullMessage, analysis.errors.exists {
      e => pf.applyOrElse(e, (_: Error) => false)
    })
  }

  object ClsInfo {
    def unapply(classInfo: Analysis.ClassInfo): Some[String] =
      Some(classInfo.encodedName)
  }

  object MethInfo {
    def unapply(methodInfo: Analysis.MethodInfo): Some[(String, String)] =
      Some((methodInfo.owner.encodedName, methodInfo.encodedName))
  }
}
