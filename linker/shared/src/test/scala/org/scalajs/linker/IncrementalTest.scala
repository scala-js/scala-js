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

import java.nio.charset.StandardCharsets.UTF_8

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.Version
import org.scalajs.ir.WellKnownNames._

import org.scalajs.logging._

import org.scalajs.junit.async._

import org.scalajs.linker._
import org.scalajs.linker.interface._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class IncrementalTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  import IncrementalTest._

  @Test // #4335
  def testChangeMethodReachedFromExport(): AsyncResult = await {

    val FooClass = ClassName("Foo")

    val jsMethodName = str("foo")
    val staticMethodName = m("value", Nil, IntRef)

    def classDefs(pre: Boolean) = Seq(
      v0 -> mainTestClassDef(
        consoleLog(JSMethodApply(LoadModule(FooClass), jsMethodName, Nil))
      ),
      v(pre) -> classDef(
        FooClass,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(FooClass, forModuleClass = true),
          MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
              staticMethodName, NON, Nil, IntType, Some(int(6)))(EOH, UNV)
        ),
        jsMethodProps = List(
          JSMethodDef(
              EMF, jsMethodName, Nil, None,
              if (pre) int(5)
              else ApplyStatic(EAF, FooClass, staticMethodName, Nil)(IntType))(
              EOH, UNV)
        )
      )
    )

    testIncrementalBidirectional(classDefs(_), _ => MainTestModuleInitializers)
  }

  @Test
  def testChangeMethodAttributeInlineableForOptimizer(): AsyncResult = await {
    val FooClass = ClassName("Foo")

    val foo = m("foo", List(IntRef), IntRef)

    val x = LocalName("x")

    def classDefs(pre: Boolean) = Seq(
      v0 -> mainTestClassDef {
        consoleLog(Apply(EAF, New(FooClass, NoArgConstructorName, Nil), foo, List(int(5)))(IntType))
      },
      v(pre) -> classDef(
        FooClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(FooClass),
          MethodDef(EMF, foo, NON, List(paramDef(x, IntType)), IntType,
              Some(VarRef(x)(IntType)))(
              EOH.withNoinline(pre), UNV)
        )
      )
    )

    testIncrementalBidirectional(classDefs(_), _ => MainTestModuleInitializers)
  }

  @Test
  def testChangeMethodAttributeShouldInlineForOptimizer(): AsyncResult = await {
    val FooClass = ClassName("Foo")

    val foo = m("foo", List(IntRef), IntRef)

    val x = LocalName("x")

    def classDefs(pre: Boolean) = Seq(
      v0 -> mainTestClassDef {
        consoleLog(Apply(EAF, New(FooClass, NoArgConstructorName, Nil), foo, List(int(5)))(IntType))
      },
      v(pre) -> classDef(
        FooClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(FooClass),
          MethodDef(EMF, foo, NON, List(paramDef(x, IntType)), IntType,
              Some(Block(
                consoleLog(VarRef(x)(IntType)),
                VarRef(x)(IntType)
              )))(
              EOH.withInline(pre), UNV)
        )
      )
    )

    testIncrementalBidirectional(classDefs(_), _ => MainTestModuleInitializers)
  }

  @Test
  def testChangeMethodAttributeIsForwarderForOptimizer(): AsyncResult = await {
    val BarInterface = ClassName("Bar")
    val Foo1Class = ClassName("Foo1")
    val Foo2Class = ClassName("Foo2")

    val BarType = ClassType(BarInterface, nullable = true, exact = false)
    val Foo1Type = ClassType(Foo1Class, nullable = true, exact = false)

    val meth = m("meth", List(ClassRef(Foo1Class), I), I)

    val foo1 = LocalName("foo1")
    val bar = LocalName("bar")
    val x = LocalName("x")

    val foo1Ref = VarRef(foo1)(Foo1Type)
    val barRef = VarRef(bar)(BarType)
    val xRef = VarRef(x)(IntType)

    val methParamDefs = List(paramDef(foo1, Foo1Type), paramDef(x, IntType))

    def classDefs(pre: Boolean) = List(
      // Main
      v0 -> mainTestClassDef(Block(
        VarDef(foo1, NON, Foo1Type, mutable = false, New(Foo1Class, NoArgConstructorName, Nil)),
        VarDef(bar, NON, BarType, mutable = false,
            If(AsInstanceOf(JSGlobalRef("randomBool"), BooleanType),
                New(Foo1Class, NoArgConstructorName, Nil),
                New(Foo2Class, NoArgConstructorName, Nil))(
                BarType)),
        consoleLog(Apply(EAF, barRef, meth, List(foo1Ref, int(5)))(IntType))
      )),

      // Bar
      v0 -> classDef(BarInterface, kind = ClassKind.Interface,
          methods = List(
            MethodDef(EMF, meth, NON, methParamDefs, IntType, Some {
              BinaryOp(BinaryOp.Int_+, int(5), BinaryOp(BinaryOp.Int_*, xRef, int(2)))
            })(EOH, UNV)
          )),

      // Foo1
      v(pre) -> classDef(
        Foo1Class,
        superClass = Some(ObjectClass),
        interfaces = List(BarInterface),
        methods = List(
          trivialCtor(Foo1Class),
          MethodDef(EMF, meth, NON, methParamDefs, IntType, Some {
            ApplyStatically(EAF, if (pre) thisFor(Foo1Class) else foo1Ref,
                BarInterface, meth, List(foo1Ref, xRef))(IntType)
          })(EOH, UNV)
        )
      ),

      // Foo2
      v0 -> classDef(Foo2Class, superClass = Some(ObjectClass), interfaces = List(BarInterface),
          methods = List(
            trivialCtor(Foo2Class),
            MethodDef(EMF, meth, NON, methParamDefs, IntType, Some {
              ApplyStatically(EAF, thisFor(Foo2Class), BarInterface, meth, List(foo1Ref, xRef))(
                  IntType)
            })(EOH, UNV)
          ))
    )

    testIncrementalBidirectional(classDefs(_), _ => MainTestModuleInitializers)
  }

  @Test
  def testStaleMethodBodyAfterItReappears_Issue4416(): AsyncResult = await {
    val FooClass = ClassName("Foo$")

    val meth1 = m("meth1", Nil, VoidRef)
    val meth2 = m("meth2", Nil, VoidRef)

    def methDef(name: MethodName, body: Tree): MethodDef =
      MethodDef(EMF, name, NON, Nil, VoidType, Some(body))(EOH.withNoinline(true), UNV)

    def callMeth(targetMeth: MethodName): Tree =
      Apply(EAF, LoadModule(FooClass), targetMeth, Nil)(VoidType)

    def classDefs(step: Int) = {
      val stepDependentMembers = step match {
        case 0 =>
          List(
            methDef(meth1, consoleLog(str("a1"))),
            methDef(meth2, consoleLog(str("a2")))
          )
        case 1 =>
          List(
            methDef(meth1, consoleLog(str("b1")))
          )
        case 2 =>
          List(
            methDef(meth1, consoleLog(str("c1"))),
            methDef(meth2, consoleLog(str("c2")))
          )
      }

      val stepDependentMainStats = step match {
        case 0 => List(callMeth(meth1), callMeth(meth2))
        case 1 => List(callMeth(meth1))
        case 2 => List(callMeth(meth1), callMeth(meth2))
      }

      val v = Version.fromInt(step)

      List(
        v -> classDef(FooClass, kind = ClassKind.ModuleClass, superClass = Some(ObjectClass),
            methods = trivialCtor(FooClass, forModuleClass = true) :: stepDependentMembers),

        v -> mainTestClassDef(Block(stepDependentMainStats))
      )
    }

    testIncrementalSteps("issue 4416", steps = 3, classDefs(_),
        _ => MainTestModuleInitializers)
  }

  /** A variant of #4416 with static methods.
   *
   *  This variant was not affected by the bug, which in fact highlighted that
   *  they follow different code paths, which is why we have a dedicated test
   *  for it.
   */
  @Test
  def testStaleStaticMethodBodyAfterItReappears(): AsyncResult = await {
    val FooClass = ClassName("Foo")

    val meth1 = m("meth1", Nil, VoidRef)
    val meth2 = m("meth2", Nil, VoidRef)

    def methDef(name: MethodName, body: Tree): MethodDef = {
      MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic), name, NON, Nil,
          VoidType, Some(body))(
          EOH.withNoinline(true), UNV)
    }

    def callMeth(targetMeth: MethodName): Tree =
      ApplyStatic(EAF, FooClass, targetMeth, Nil)(VoidType)

    def classDefs(step: Int) = {
      val stepDependentMembers = step match {
        case 0 =>
          List(
            methDef(meth1, consoleLog(str("a1"))),
            methDef(meth2, consoleLog(str("a2")))
          )
        case 1 =>
          List(
            methDef(meth1, consoleLog(str("b1")))
          )
        case 2 =>
          List(
            methDef(meth1, consoleLog(str("c1"))),
            methDef(meth2, consoleLog(str("c2")))
          )
      }

      val stepDependentMainStats = step match {
        case 0 => List(callMeth(meth1), callMeth(meth2))
        case 1 => List(callMeth(meth1))
        case 2 => List(callMeth(meth1), callMeth(meth2))
      }

      val v = Version.fromInt(step)

      List(
        v -> classDef(FooClass, superClass = Some(ObjectClass),
            methods = trivialCtor(FooClass) :: stepDependentMembers),

        v -> mainTestClassDef(Block(stepDependentMainStats))
      )
    }

    testIncrementalSteps("issue 4416 static variant", steps = 3, classDefs(_),
        _ => MainTestModuleInitializers)
  }

  @Test
  def testInvalidateElidableModuleAccessors_Issue4593(): AsyncResult = await {
    val FooModule = ClassName("Foo")

    def fooCtor(pre: Boolean) = {
      val superCtor = Block(
        ApplyStatically(EAF.withConstructor(true),
            thisFor(FooModule),
            ObjectClass, MethodIdent(NoArgConstructorName),
            Nil)(VoidType),
        StoreModule()
      )

      val body =
        if (pre) superCtor
        else Block(superCtor, consoleLog(str("bar")))

      MethodDef(MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
          MethodIdent(NoArgConstructorName), NON, Nil, VoidType,
          Some(body))(EOH, UNV)
    }

    def classDefs(pre: Boolean) = Seq(
      v0 -> mainTestClassDef(Block(
        consoleLog(str("foo")),
        LoadModule(FooModule)
      )),
      v(pre) -> classDef(
        FooModule,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(fooCtor(pre))
      )
    )

    testIncrementalBidirectional(classDefs(_), _ => MainTestModuleInitializers)
  }

  @Test
  def testInvalidateExportedMethods_Issue4774(): AsyncResult = await {
    val AModule = ClassName("A")
    val BModule = ClassName("B")

    val jsMethodName = str("foo")
    val targetMethodName = m("value", Nil, IntRef)

    def classDefs(pre: Boolean) = Seq(
      v0 -> mainTestClassDef(
        consoleLog(JSMethodApply(LoadModule(AModule), jsMethodName, Nil))
      ),
      v0 -> classDef(
        AModule,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(AModule, forModuleClass = true)
        ),
        jsMethodProps = List(
          JSMethodDef(EMF, str("foo"), Nil, None,
              Apply(EAF, LoadModule(BModule), targetMethodName, Nil)(IntType))(EOH, UNV)
        )
      ),
      v(pre) -> classDef(
        BModule,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(BModule, forModuleClass = true),
          MethodDef(EMF, targetMethodName, NON, Nil, IntType,
              Some(int(if (pre) 1 else 2)))(EOH.withInline(true), UNV)
        )
      )
    )

    testIncrementalBidirectional(classDefs(_), _ => MainTestModuleInitializers)
  }

  @Test
  def testInvalidateJSCtor_Issue4774(): AsyncResult = await {
    val AClass = ClassName("A")
    val BModule = ClassName("B")
    val JSObject = ClassName("jso")

    val jsMethodName = str("foo")
    val targetMethodName = m("value", Nil, IntRef)

    def classDefs(pre: Boolean) = Seq(
      v0 -> mainTestClassDef(
        consoleLog(JSNew(LoadJSConstructor(AClass), Nil))
      ),
      v0 -> classDef(
        JSObject,
        kind = ClassKind.NativeJSClass,
        jsNativeLoadSpec = Some(JSNativeLoadSpec.Global("Object", Nil)),
        superClass = Some(ObjectClass)
      ),
      v0 -> classDef(
        AClass,
        kind = ClassKind.JSClass,
        superClass = Some(JSObject),
        jsConstructor = Some(
          JSConstructorDef(EMF.withNamespace(MemberNamespace.Constructor), Nil, None,
              JSConstructorBody(Nil, JSSuperConstructorCall(Nil), List {
                consoleLog(Apply(EAF, LoadModule(BModule), targetMethodName, Nil)(IntType))
              }))(EOH, UNV)
        )
      ),
      v(pre) -> classDef(
        BModule,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(BModule, forModuleClass = true),
          MethodDef(EMF, targetMethodName, NON, Nil, IntType,
              Some(int(if (pre) 1 else 2)))(EOH.withInline(true), UNV)
        )
      )
    )

    testIncrementalBidirectional(classDefs(_), _ => MainTestModuleInitializers)
  }

  @Test
  def testInvalidateTopLevelExportDependency(): AsyncResult = await {
    val AModule = ClassName("A")
    val BModule = ClassName("B")

    val targetMethodName = m("value", Nil, IntRef)

    def classDefs(pre: Boolean) = Seq(
      v0 -> classDef(
        AModule,
        kind = ClassKind.Interface,
        topLevelExportDefs = List(
          TopLevelMethodExportDef("main",
              JSMethodDef(EMF.withNamespace(MemberNamespace.PublicStatic),
                  str("foo"), Nil, None,
                  Apply(EAF, LoadModule(BModule), targetMethodName, Nil)(IntType))(EOH, UNV))
        )
      ),
      v(pre) -> classDef(
        BModule,
        kind = ClassKind.ModuleClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(BModule, forModuleClass = true),
          MethodDef(EMF, targetMethodName, NON, Nil, IntType,
              Some(int(if (pre) 1 else 2)))(EOH.withInline(true), UNV)
        )
      )
    )

    testIncrementalBidirectional(classDefs(_), _ => Nil)
  }

  @Test
  def testInvalidateInstanceTestsNeededWithModules_Issue5131(): AsyncResult = await {
    val AClass = ClassName("pack.A")
    val BClass = ClassName("pack.B")
    val CClass = ClassName("pack.C")

    val TestClass = ClassName("pack.Test")

    val testMethodName = m("test", List(ClassRef(AClass)), BooleanRef)

    val xParam = paramDef("x", ClassType(AClass, nullable = true, exact = false))

    def classDefs(pre: Boolean): Seq[(Version, ClassDef)] = Seq(
      v0 -> classDef(
        AClass,
        kind = ClassKind.Interface
      ),
      v0 -> classDef(
        BClass,
        superClass = Some(ObjectClass),
        interfaces = List(AClass),
        methods = List(trivialCtor(BClass))
      ),
      v0 -> classDef(
        CClass,
        superClass = Some(ObjectClass),
        interfaces = List(AClass),
        methods = List(trivialCtor(CClass))
      ),
      v(pre) -> classDef(
        TestClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(TestClass),
          mainMethodDef(Block(
            consoleLog(ApplyStatic(EAF, TestClass, testMethodName,
                List(New(BClass, NoArgConstructorName, Nil)))(BooleanType)),
            consoleLog(ApplyStatic(EAF, TestClass, testMethodName,
                List(New(CClass, NoArgConstructorName, Nil)))(BooleanType))
          )),
          MethodDef(
            EMF.withNamespace(MemberNamespace.PublicStatic),
            testMethodName,
            NON,
            List(xParam),
            BooleanType,
            Some {
              IsInstanceOf(
                  xParam.ref, ClassType(if (pre) BClass else CClass, nullable = false, exact = false))
            }
          )(EOH.withNoinline(true), UNV)
        )
      )
    )

    val moduleInits = mainModuleInitializers(TestClass.nameString)

    val config = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("pack")))

    testIncrementalBidirectional(classDefs(_), _ => moduleInits, config)
  }

  @Test
  def testInvalidateStaticLikeMethodSetWithModules_Issue5131(): AsyncResult = await {
    val AClass = ClassName("pack.A") // with instances
    val BClass = ClassName("pack.B") // static-like only

    val TestClass = ClassName("pack.Test")

    val fooMethodName = m("foo", List(IntRef), IntRef)
    val barMethodName = m("bar", List(IntRef), IntRef)

    val xParam = paramDef("x", IntType)

    def makeOneMethodDef(name: MethodName): MethodDef = {
      MethodDef(EMF.withNamespace(MemberNamespace.PublicStatic), name, NON,
          List(xParam), IntType, Some(xParam.ref))(
          EOH.withNoinline(true), UNV)
    }

    def classDefs(pre: Boolean): Seq[(Version, ClassDef)] = Seq(
      v0 -> classDef(
        AClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(AClass),
          makeOneMethodDef(fooMethodName),
          makeOneMethodDef(barMethodName)
        )
      ),
      v0 -> classDef(
        BClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(BClass),
          makeOneMethodDef(fooMethodName),
          makeOneMethodDef(barMethodName)
        )
      ),
      v(pre) -> classDef(
        TestClass,
        superClass = Some(ObjectClass),
        methods = List(
          trivialCtor(TestClass),
          mainMethodDef(Block(
            // make an instance of A, but not of B
            consoleLog(New(AClass, NoArgConstructorName, Nil)),

            // call both foo methods
            consoleLog(ApplyStatic(EAF, AClass, fooMethodName, List(int(1)))(IntType)),
            consoleLog(ApplyStatic(EAF, BClass, fooMethodName, List(int(2)))(IntType)),

            // call both bar methods only in v0
            if (pre) {
              Block(
                consoleLog(ApplyStatic(EAF, AClass, barMethodName, List(int(3)))(IntType)),
                consoleLog(ApplyStatic(EAF, BClass, barMethodName, List(int(4)))(IntType))
              )
            } else {
              Skip()
            }
          ))
        )
      )
    )

    val moduleInits = mainModuleInitializers(TestClass.nameString)

    val config = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("pack")))

    testIncrementalBidirectional(classDefs(_), _ => moduleInits, config)
  }
}

object IncrementalTest {

  def testIncrementalBidirectional(
      classDefs: Boolean => Seq[(Version, ClassDef)],
      moduleInitializers: Boolean => List[ModuleInitializer],
      config: StandardConfig = StandardConfig())(
      implicit ec: ExecutionContext): Future[Unit] = {

    def testOneDirection(contextMessage: String, forward: Boolean): Future[Unit] = {
      def pre(step: Int): Boolean =
        if (forward) step == 0
        else step == 1
      testIncrementalSteps(contextMessage, steps = 2,
          step => classDefs(pre(step)), step => moduleInitializers(pre(step)),
          config)
    }

    for {
      _ <- testOneDirection("forward", forward = true)
      _ <- testOneDirection("backward", forward = false)
    } yield ()
  }

  def testIncrementalSteps(
      contextMessage: String,
      steps: Int,
      stepToClassDefs: Int => Seq[(Version, ClassDef)],
      stepToModuleInitializers: Int => List[ModuleInitializer],
      config: StandardConfig = StandardConfig())(
      implicit ec: ExecutionContext): Future[Unit] = {

    require(steps >= 2, s"require at least 2 steps but got $steps")

    val outputInc = MemOutputDirectory()
    val configWithCheckIR = config.withCheckIR(true)
    val linkerInc = StandardImpl.linker(configWithCheckIR)

    val logger = new ScalaConsoleLogger(Level.Error)

    TestIRRepo.minilib.flatMap { minilib =>
      def loop(step: Int): Future[Unit] = {
        if (step == steps) {
          Future.successful(())
        } else {
          val outputBatch = MemOutputDirectory()
          val linkerBatch = StandardImpl.linker(configWithCheckIR)

          val irFiles = minilib ++ stepToClassDefs(step).map(x => MemClassDefIRFile(x._2, x._1))
          val moduleInitializers = stepToModuleInitializers(step)

          val thisStepResult = for {
            reportInc <- linkerInc.link(irFiles, moduleInitializers, outputInc, logger)
            reportBatch <- linkerBatch.link(irFiles, moduleInitializers, outputBatch, logger)
          } yield {
            assertModulesEqual(s"Public modules in report equal ($contextMessage, step $step)",
                reportBatch.publicModules, reportInc.publicModules)

            assertOutputEquals(s"Outputs equal ($contextMessage, step $step)",
                outputBatch, outputInc)
          }

          thisStepResult.flatMap(_ => loop(step + 1))
        }
      }

      loop(step = 0)
    }
  }

  private val v0 = Version.fromInt(0)

  private def v(pre: Boolean) =
    Version.fromInt(if (pre) 0 else 1)

  private def assertModulesEqual(msg: String, expected: Iterable[Report.Module],
      actual: Iterable[Report.Module]): Unit = {
    // Poor man's equality based on toString()

    def strs(ms: Iterable[Report.Module]) =
      ms.map(m => m.moduleID -> m.toString()).toMap

    assertEquals(msg, strs(expected), strs(actual))
  }

  private def assertOutputEquals(msg: String, expected: MemOutputDirectory,
      actual: MemOutputDirectory): Unit = {
    val filesExpected = expected.fileNames()
    val filesActual = actual.fileNames()

    assertEquals(s"$msg: set of files", filesExpected.toSet, filesActual.toSet)

    for (f <- filesExpected.sorted) {
      assertEquals(
        s"$msg: content of $f",
        new String(expected.content(f).get, UTF_8),
        new String(actual.content(f).get, UTF_8)
      )
    }
  }
}
