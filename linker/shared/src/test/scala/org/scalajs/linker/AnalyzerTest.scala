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
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import Analysis._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._
import org.scalajs.linker.testutils.LinkingUtils._

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
    assertContainsError("MissingClass(jlObject)", analysis) {
      case MissingClass(ClsInfo(name), fromAnalyzer) =>
        name == ObjectClass.nameString
    }
  }

  @Test
  def missingJavaLangObjectButOthers(): AsyncResult = await {
    val classDefs = Seq(classDef("A", superClass = Some(ObjectClass)))

    val analysis = computeAnalysis(classDefs,
        reqsFactory.classData("A"), stdlib = TestIRRepo.empty)

    assertContainsError("MissingClass(jlObject)", analysis) {
      case MissingClass(ClsInfo(name), fromAnalyzer) =>
        name == ObjectClass.nameString
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
      case CycleInInheritanceChain(List(ClsName("A"), ClsName("B")), `fromAnalyzer`) => true
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
      case CycleInInheritanceChain(List(ClsName("A"), ClsName("B")), `fromAnalyzer`) => true
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

    assertContainsError("CycleInInheritanceChain(C, D, E)", analysis) {
      case CycleInInheritanceChain(List(ClsName("C"), ClsName("D"), ClsName("E")), `fromAnalyzer`) => true
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
  def missingClassParentSynthesizingConstructorPlusReflectiveCall_Issue4865(): AsyncResult = await {
    val classDefs = Seq(
        classDef("A", superClass = Some("B"),
            methods = List(trivialCtor("A", parentClassName = "B")))
    )

    val requirements = {
      reqsFactory.instantiateClass("A", NoArgConstructorName) ++
      reqsFactory.callMethod(ObjectClass, MethodName.reflectiveProxy("foo", Nil))
    }

    val analysis = computeAnalysis(classDefs, requirements)

    assertContainsError("MissingClass(B)", analysis) {
      case MissingClass(ClsInfo("B"), FromClass(ClsInfo("A"))) => true
    }
  }

  @Test
  def invalidSuperClass(): AsyncResult = await {
    val kindsSub = Seq(
        ClassKind.Class,
        ClassKind.ModuleClass,
        ClassKind.JSClass,
        ClassKind.JSModuleClass,
        ClassKind.NativeJSClass,
        ClassKind.NativeJSModuleClass,
        ClassKind.AbstractJSType
    )

    def kindsBaseFor(kindSub: ClassKind): Seq[ClassKind] = {
      import ClassKind._
      kindSub match {
        case Class | ModuleClass =>
          Seq(Interface, ModuleClass, JSClass, NativeJSClass)
        case Interface =>
          // interfaces are checked in the ClassDefChecker.
          throw new AssertionError("unreachable")
        case JSClass | JSModuleClass | NativeJSClass | NativeJSModuleClass |
            AbstractJSType =>
          Seq(Class, Interface, AbstractJSType, JSModuleClass)
        case HijackedClass =>
          throw new AssertionError("Cannot test HijackedClass because it fails earlier")
      }
    }

    Future.traverse(kindsSub) { kindSub =>
      Future.traverse(kindsBaseFor(kindSub)) { kindBase =>

        val classDefs = Seq(
            classDef("A", kind = kindSub,
                superClass = Some("B"),
                methods = requiredMethods("A", kindSub),
                jsConstructor = requiredJSConstructor(kindSub)),
            classDef("B", kind = kindBase,
                superClass = validParentForKind(kindBase),
                methods = requiredMethods("B", kindBase),
                jsConstructor = requiredJSConstructor(kindBase))
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
        case Class | ModuleClass | Interface =>
          Seq(Class, ModuleClass, JSClass, NativeJSClass, AbstractJSType)
        case JSClass | JSModuleClass | NativeJSClass | NativeJSModuleClass |
            AbstractJSType =>
          Seq(Class, ModuleClass, Interface, JSClass,
              JSModuleClass, NativeJSClass, NativeJSModuleClass)
        case HijackedClass =>
          throw new AssertionError("Cannot test HijackedClass because it fails earlier")
      }
    }

    Future.traverse(kindsCls) { kindCls =>
      Future.traverse(kindsIntfFor(kindCls)) { kindIntf =>
        val classDefs = Seq(
            classDef("A", kind = kindCls,
                superClass = validParentForKind(kindCls),
                interfaces = List("B"),
                methods = requiredMethods("A", kindCls),
                jsConstructor = requiredJSConstructor(kindCls)),
            classDef("B", kind = kindIntf,
                superClass = validParentForKind(kindIntf),
                methods = requiredMethods("B", kindIntf),
                jsConstructor = requiredJSConstructor(kindIntf))
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
            methods = List(trivialCtor("A")))
    )

    val analysis = computeAnalysis(classDefs, reqsFactory.accessModule("A"))

    assertContainsError("NotAModule(A)", analysis) {
      case NotAModule(ClsInfo("A"), `fromUnitTest`) => true
    }
  }

  @Test
  def missingMethod(): AsyncResult = await {
    val fooMethodName = m("foo", Nil, V)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            methods = List(trivialCtor("A")))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("A", NoArgConstructorName) ++
        reqsFactory.callMethod("A", fooMethodName))

    assertContainsError("MissingMethod(A.foo;V)", analysis) {
      case MissingMethod(MethInfo("A", "foo;V"), FromDispatch(ClsInfo("A"), `fooMethodName`)) => true
    }
  }

  @Test
  def missingInheritedMethod(): AsyncResult = await {
    /* Test that we do not use an inherited missing method:
     * Otherwise we'll open ourselves up to non-deterministic behavior that
     * depends on the call graph traversal order.
     *
     * The invocation below is carefully crafted to delay traversal of the call
     * to B#foo as late as possible. If the lookup of B#foo would re-use the
     * created missing method in A#foo, we'd get one error less.
     */

    val fooMethodName = m("foo", Nil, V)
    val barMethodName = m("bar", Nil, V)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            methods = List(
                trivialCtor("A"),
                MethodDef(EMF, barMethodName, NON, Nil, NoType, Some(Block(
                  Apply(EAF, This()(ClassType("A")), fooMethodName, Nil)(NoType),
                  Apply(EAF, New("B", NoArgConstructorName, Nil), fooMethodName, Nil)(NoType)
                )))(EOH, UNV)
            )),
        classDef("B", superClass = Some("A"),
            methods = List(trivialCtor("B")))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("A", NoArgConstructorName) ++
        reqsFactory.callMethod("A", barMethodName))

    assertContainsError("MissingMethod(A.foo;V) from A", analysis) {
      case MissingMethod(MethInfo("A", "foo;V"), FromDispatch(ClsInfo("A"), `fooMethodName`)) => true
    }

    assertContainsError("MissingMethod(B.foo;V) from A", analysis) {
      case MissingMethod(MethInfo("B", "foo;V"), FromDispatch(ClsInfo("A"), `fooMethodName`)) => true
    }

    assertContainsError("MissingMethod(B.foo;V) from B", analysis) {
      case MissingMethod(MethInfo("B", "foo;V"), FromDispatch(ClsInfo("B"), `fooMethodName`)) => true
    }
  }

  @Test
  def missingAbstractMethod(): AsyncResult = await {
    val fooMethodName = m("foo", Nil, IntRef)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            methods = List(trivialCtor("A"))),
        classDef("B", superClass = Some("A"),
            methods = List(
                trivialCtor("B"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, Some(int(5)))(EOH, UNV)
            ))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("B", NoArgConstructorName) ++
        reqsFactory.callMethod("A", fooMethodName))

    assertContainsError("MissingMethod(A.foo;I)", analysis) {
      case MissingMethod(MethInfo("A", "foo;I"), FromDispatch(ClsInfo("A"), `fooMethodName`)) => true
    }
  }

  @Test
  def callAbstractMethod(): AsyncResult = await {
    val fooMethodName = m("foo", Nil, IntRef)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            methods = List(
                trivialCtor("A"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, None)(EOH, UNV)
            )
        )
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.instantiateClass("A", NoArgConstructorName) ++
        reqsFactory.callMethod("A", fooMethodName))

    assertContainsError("MissingMethod(A.foo;I)", analysis) {
      case MissingMethod(MethInfo("A", "foo;I"), FromDispatch(ClsInfo("A"),`fooMethodName`)) => true
    }
  }

  @Test
  def staticCallAbstractMethod(): AsyncResult = await {
    val fooMethodName = m("foo", Nil, IntRef)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            methods = List(
                trivialCtor("A"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, None)(EOH, UNV)
            )
        )
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.callMethodStatically("A", fooMethodName))

    assertContainsError("MissingMethod(A.foo;I)", analysis) {
      case MissingMethod(MethInfo("A", "foo;I"), `fromUnitTest`) => true
    }
  }

  @Test
  def missingJSNativeMember(): AsyncResult = await {
    val mainName = m("main", Nil, V)
    val testName = m("test", Nil, O)
    val method = MethodDef(
        EMF.withNamespace(MemberNamespace.PublicStatic),
        mainName, NON, Nil, NoType,
        Some(SelectJSNativeMember("A", testName)))(EOH, UNV)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            methods = List(method))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.callStaticMethod("A", mainName))

    assertContainsError("MissingJSNativeMember(A.test;O)", analysis) {
      case MissingJSNativeMember(ClsInfo("A"), `testName`,
          FromMethod(MethInfo("A", "main;V"))) => true
    }
  }

  @Test
  def conflictingDefaultMethods(): AsyncResult = await {
    val defaultMethodDef = MethodDef(EMF, m("foo", Nil, V), NON, Nil,
        NoType, Some(Skip()))(EOH, UNV)
    val classDefs = Seq(
        classDef("I1", kind = ClassKind.Interface,
            methods = List(defaultMethodDef)),
        classDef("I2", kind = ClassKind.Interface,
            methods = List(defaultMethodDef)),
        classDef("A", superClass = Some(ObjectClass),
            interfaces = List("I1", "I2"),
            methods = List(trivialCtor("A")))
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
  def invalidTopLevelExportInScript(): AsyncResult = await {
    val classDefs = Seq(
        classDef(
            "A",
            kind = ClassKind.ModuleClass,
            superClass = Some(ObjectClass),
            methods = List(trivialCtor("A")),
            topLevelExportDefs = List(
                TopLevelMethodExportDef("main", JSMethodDef(
                    EMF.withNamespace(MemberNamespace.PublicStatic),
                    str("default"), Nil, None, Undefined())(
                    EOH, UNV))
            )
        )
    )

    testScriptAndModule(classDefs) { scriptAnalysis =>
      assertContainsError("InvalidTopLevelExportInScript(foo, A)", scriptAnalysis) {
        case InvalidTopLevelExportInScript(TLEInfo(ModID("main"), "default", ClsName("A"))) =>
          true
      }
    } { moduleAnalysis =>
      assertNoError(moduleAnalysis)
    }
  }

  @Test
  def conflictingTopLevelExportsDifferentModules(): AsyncResult = await {
    def singleDef(name: String) = {
      classDef(name,
          kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
          methods = List(trivialCtor(name)),
          topLevelExportDefs = List(TopLevelModuleExportDef(name, "foo")))
    }

    val classDefs = Seq(singleDef("A"), singleDef("B"))

    testScriptAndModule(classDefs) { scriptAnalysis =>
      assertContainsError("MultiplePublicModulesWithoutModuleSupport(A, B)", scriptAnalysis) {
        case MultiplePublicModulesWithoutModuleSupport(List(ModID("A"), ModID("B"))) =>
          true
        case MultiplePublicModulesWithoutModuleSupport(List(ModID("B"), ModID("A"))) =>
          true
      }
    } { moduleAnalysis =>
      assertNoError(moduleAnalysis)
    }
  }

  @Test
  def conflictingTopLevelExportsSameModule(): AsyncResult = await {
    def singleDef(name: String) = {
      classDef(name,
          kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
          methods = List(trivialCtor(name)),
          topLevelExportDefs = List(TopLevelModuleExportDef("main", "foo")))
    }

    val classDefs = Seq(singleDef("A"), singleDef("B"))

    val analysis = computeAnalysis(classDefs)
    assertContainsError("ConflictingTopLevelExport(main, foo, A, B)", analysis) {
      case ConflictingTopLevelExport(ModID("main"), "foo",
          List(TLEInfo(_, _, ClsName("A")), TLEInfo(_, _, ClsName("B")))) =>
        true
      case ConflictingTopLevelExport(ModID("main"), "foo",
          List(TLEInfo(_, _, ClsName("B")), TLEInfo(_, _, ClsName("A")))) =>
        true
    }
  }

  @Test
  def degenerateConflictingTopLevelExports(): AsyncResult = await {
    val classDefs = Seq(classDef("A",
        kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
        methods = List(trivialCtor("A")),
        topLevelExportDefs = List(
            TopLevelModuleExportDef("main", "foo"),
            TopLevelModuleExportDef("main", "foo"))))

    val analysis = computeAnalysis(classDefs)
    assertContainsError("ConflictingTopLevelExport(_, foo, <degenerate>)", analysis) {
      case ConflictingTopLevelExport(_, "foo", _) => true
    }
  }

  @Test
  def multipleModulesTopLevelExportAndModuleInitializer(): AsyncResult = await {
    val classDefs = Seq(classDef("A",
        kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
        methods = List(
            trivialCtor("A"),
            mainMethodDef(Skip())
        ),
        topLevelExportDefs = List(TopLevelModuleExportDef("A", "foo"))))

    val moduleInitializer =
      ModuleInitializer.mainMethodWithArgs("A", "main").withModuleID("B")

    testScriptAndModule(classDefs, moduleInitializers = List(moduleInitializer)) { scriptAnalysis =>
      assertContainsError("MultiplePublicModulesWithoutModuleSupport(A, B)", scriptAnalysis) {
        case MultiplePublicModulesWithoutModuleSupport(List(ModID("A"), ModID("B"))) =>
          true
        case MultiplePublicModulesWithoutModuleSupport(List(ModID("B"), ModID("A"))) =>
          true
      }
    } { moduleAnalysis =>
      assertNoError(moduleAnalysis)
    }
  }

  @Test
  def importClassWithoutModuleSupport(): AsyncResult = await {
    val kinds = Seq(
        ClassKind.NativeJSClass,
        ClassKind.NativeJSModuleClass
    )

    Future.traverse(kinds) { kind =>
      val classDefs = Seq(
          classDef("A", kind = kind, superClass = Some(ObjectClass),
              jsNativeLoadSpec = Some(JSNativeLoadSpec.Import("my-module", List("A"))))
      )

      val analysis = computeAnalysis(classDefs,
          reqsFactory.instantiateClass("A", NoArgConstructorName),
          config = StandardConfig().withModuleKind(ModuleKind.NoModule))

      assertContainsError("ImportWithoutModuleSupport(my-module, A, None)", analysis) {
        case ImportWithoutModuleSupport("my-module", ClsInfo("A"), None, `fromUnitTest`) =>
          true
      }
    }
  }

  @Test
  def importJSNativeMemberWithoutModuleSupport(): AsyncResult = await {
    val mainName = m("main", Nil, V)
    val testName = m("test", Nil, O)

    val mainMethod = MethodDef(
        EMF.withNamespace(MemberNamespace.PublicStatic),
        mainName, NON, Nil, NoType,
        Some(SelectJSNativeMember("A", testName)))(EOH, UNV)
    val nativeMember = JSNativeMemberDef(
        EMF.withNamespace(MemberNamespace.PublicStatic), testName,
        JSNativeLoadSpec.Import("my-module", List("test")))

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass),
            methods = List(mainMethod),
            jsNativeMembers = List(nativeMember))
    )

    val analysis = computeAnalysis(classDefs,
        reqsFactory.callStaticMethod("A", mainName))

    assertContainsError("ImportWithoutModuleSupport(my-module, A, None)", analysis) {
      case ImportWithoutModuleSupport("my-module", ClsInfo("A"), Some(`testName`),
          FromMethod(MethInfo("A", "main;V"))) => true
    }
  }

  @Test
  def importDynamicWithoutModuleSupport(): AsyncResult = await {
    val dynName = m("dyn", Nil, O)

    val classDefs = Seq(
        classDef("A",
            kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
            methods = List(
                trivialCtor("A"),
                mainMethodDef(ApplyDynamicImport(EAF, "B", dynName, Nil)))
        ),
        classDef("B",
            kind = ClassKind.Class, superClass = Some(ObjectClass),
            methods = List(
                MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                    dynName, NON, Nil, AnyType,
                    Some(consoleLog(str("hello world"))))(EOH, UNV)))
    )

    val moduleInitializer = ModuleInitializer.mainMethodWithArgs("A", "main")

    testScriptAndModule(classDefs, moduleInitializers = List(moduleInitializer)) { scriptAnalysis =>
      assertContainsError("DynamicImportWithoutModuleSupport", scriptAnalysis) {
        case DynamicImportWithoutModuleSupport(_) =>
          true
      }
    } { moduleAnalysis =>
      assertNoError(moduleAnalysis)
    }
  }

  @Test
  def newTargetWithoutES2015(): AsyncResult = await {
    val classDefs = Seq(
      mainTestClassDef(LoadJSConstructor("A")),
      classDef("A",
        kind = ClassKind.JSClass,
        superClass = Some(JSObjectLikeClass),
        jsConstructor = Some(
          JSConstructorDef(JSCtorFlags, Nil, None, JSConstructorBody(
            Nil,
            JSSuperConstructorCall(Nil),
            JSNewTarget() :: Nil
          ))(EOH, UNV)
        )
      ),
      JSObjectLikeClassDef
    )

    val moduleInitializer = MainTestModuleInitializers

    val analysis = computeAnalysis(classDefs,
        moduleInitializers = MainTestModuleInitializers,
        config = StandardConfig().withESFeatures(_.withESVersion(ESVersion.ES5_1)))

    assertContainsError("NewTargetWithoutES2015Support", analysis) {
      case NewTargetWithoutES2015Support(_) => true
    }
  }

  @Test
  def importMetaWithoutESModule(): AsyncResult = await {
    val classDefs = Seq(
      classDef("A",
        kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
        methods = List(
          trivialCtor("A"),
          mainMethodDef(JSImportMeta())
        )
      )
    )

    val moduleInitializer = ModuleInitializer.mainMethodWithArgs("A", "main")

    testForEachModuleKind(classDefs, moduleInitializers = List(moduleInitializer)) {
      (kind, analysis) =>
        if (kind == ModuleKind.ESModule) {
          assertNoError(analysis)
        } else {
          assertContainsError("ImportMetaWithoutESModule", analysis) {
            case ImportMetaWithoutESModule(_) =>
              true
          }
        }
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
            methods = List(
                trivialCtor("X"),
                MethodDef(EMF, fooAMethodName, NON, Nil, ClassType("A"),
                    Some(Null()))(EOH, UNV),
                MethodDef(EMF, fooBMethodName, NON, Nil, ClassType("B"),
                    Some(Null()))(EOH, UNV)
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
            methods = List(
                MethodDef(EMF, barMethodName, NON, Nil, IntType, None)(EOH, UNV)
            )),
        classDef("I2", kind = ClassKind.Interface,
            methods = List(
                MethodDef(EMF, barMethodName, NON, Nil, IntType, None)(EOH, UNV)
            )),
        classDef("A", superClass = Some(ObjectClass), interfaces = List("I1"),
            methods = List(
                trivialCtor("A"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, None)(EOH, UNV)
            )),
        classDef("B", superClass = Some("A"), interfaces = List("I2"),
            methods = List(
                trivialCtor("B"),
                MethodDef(EMF, fooMethodName, NON, Nil, IntType, Some(int(5)))(EOH, UNV)
            )),
        classDef("C", superClass = Some("B"),
            methods = List(
                trivialCtor("C"),
                MethodDef(EMF, barMethodName, NON, Nil, IntType, Some(int(5)))(EOH, UNV)
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

  @Test
  def linkTimeIfReachable(): AsyncResult = await {
    val mainMethodName = m("main", Nil, IntRef)
    val fooMethodName = m("foo", Nil, IntRef)
    val barMethodName = m("bar", Nil, IntRef)

    val productionMode = true

    /* linkTimeIf(productionMode) {
     *   this.foo()
     * } {
     *   this.bar()
     * }
     */
    val mainBody = LinkTimeIf(
      LinkTimeTree.BinaryOp(
        LinkTimeOp.Boolean_==,
        LinkTimeTree.Property(
            "core/productionMode", BooleanType),
        LinkTimeTree.BooleanConst(productionMode)),
      Apply(EAF, This()(ClassType("A")), fooMethodName, Nil)(IntType),
      Apply(EAF, This()(ClassType("A")), barMethodName, Nil)(IntType)
    )(IntType)

    val classDefs = Seq(
      classDef("A", superClass = Some(ObjectClass),
        methods = List(
          trivialCtor("A"),
          MethodDef(EMF, mainMethodName, NON, Nil, IntType,
              Some(mainBody))(EOH, UNV),
          MethodDef(EMF, fooMethodName, NON, Nil, IntType,
              Some(Null()))(EOH, UNV),
          MethodDef(EMF, barMethodName, NON, Nil, IntType,
              Some(Null()))(EOH, UNV)
        )
      )
    )

    val analysisFuture = computeAnalysis(classDefs,
      reqsFactory.instantiateClass("A", NoArgConstructorName) ++
        reqsFactory.callMethod("A", mainMethodName),
      config = StandardConfig().withSemantics(_.withProductionMode(productionMode))
    )

    for (analysis <- analysisFuture) yield {
      assertNoError(analysis)

      val AfooMethodInfo = analysis.classInfos("A")
        .methodInfos(MemberNamespace.Public)(fooMethodName)
      assertTrue(AfooMethodInfo.isReachable)

      val AbarMethodInfo = analysis.classInfos("A")
        .methodInfos(MemberNamespace.Public)(barMethodName)
      assertFalse(AbarMethodInfo.isReachable)
    }
  }
}

object AnalyzerTest {
  private val reqsFactory = SymbolRequirement.factory("unit test")

  private val fromAnalyzer = FromCore("analyzer")
  private val fromUnitTest = FromCore("unit test")

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

  private def testScriptAndModule(classDefs: Seq[ClassDef],
      moduleInitializers: Seq[ModuleInitializer] = Nil)(
      scriptTest: Analysis => Unit)(
      moduleTest: Analysis => Unit)(
      implicit ec: ExecutionContext): Future[Unit] = {

    testForEachModuleKind(classDefs, moduleInitializers) { (kind, analysis) =>
      if (kind == ModuleKind.NoModule)
        scriptTest(analysis)
      else
        moduleTest(analysis)
    }
  }

  private def testForEachModuleKind(classDefs: Seq[ClassDef],
      moduleInitializers: Seq[ModuleInitializer] = Nil)(
      test: (ModuleKind, Analysis) => Unit)(
      implicit ec: ExecutionContext): Future[Unit] = {

    val results = for (kind <- ModuleKind.All) yield {
      val analysis = computeAnalysis(classDefs,
          moduleInitializers = moduleInitializers,
          config = StandardConfig().withModuleKind(kind))
      analysis.map(test(kind, _))
    }

    Future.sequence(results).map(_ => ())
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

  object TLEInfo {
    def unapply(tleInfo: Analysis.TopLevelExportInfo): Some[(ModuleID, String, ClassName)] =
      Some((tleInfo.moduleID, tleInfo.exportName, tleInfo.owningClass))
  }

  object ClsName {
    def unapply(className: ClassName): Some[String] =
      Some(className.nameString)
  }

  object ModID {
    def unapply(moduleID: ModuleID): Some[String] =
      Some(moduleID.id)
  }
}
