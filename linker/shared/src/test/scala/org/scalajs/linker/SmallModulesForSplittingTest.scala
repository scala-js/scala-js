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

import scala.collection.mutable
import scala.concurrent._

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard.ModuleSet
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class SmallModulesForSplittingTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def splitsModules(): AsyncResult = await {
    /* Test splitting in the degenerate case, where dependencies traverse the
     * split boundary multiple times.
     */
    val strClsType = ClassType(BoxedStringClass, nullable = true)

    val methodName = m("get", Nil, T)

    val SMF = EMF.withNamespace(MemberNamespace.PublicStatic)

    def methodHolder(name: ClassName, body: Tree) = {
      classDef(name,
        kind = ClassKind.Interface,
        methods = List(
          MethodDef(SMF, methodName, NON, Nil, strClsType, Some(body))(
              EOH.withNoinline(true), UNV)
        ))
    }

    def call(name: ClassName): Tree =
      ApplyStatic(EAF, name, methodName, Nil)(strClsType)

    val helloWorldClass = "helloworld.HelloWorld$"

    val classDefs = Seq(
      methodHolder("foo.A", str("Hello World")),
      methodHolder("bar.B", call("foo.A")),
      methodHolder("foo.C", call("bar.B")),
      mainTestClassDef(consoleLog(call("foo.C")))
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("foo")))
      .withSourceMap(false)

    for {
      moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers,
          config = linkerConfig)
    } yield {
      def moduleClasses(id: String) = {
        val module = moduleSet.modules.find(_.id.id == id).getOrElse {
          val ids = moduleSet.modules.map(_.id.id).mkString(", ")
          throw new AssertionError(s"couldn't find module with id: `$id`, got: [$ids]")
        }

        module.classDefs.map(_.name.name)
      }

      assertEquals(List[ClassName]("foo.A"), moduleClasses("foo.-A"))
      assertEquals(List[ClassName]("foo.C"), moduleClasses("foo.-C"))
      assertEquals(List(MainTestClassName), moduleClasses("main"))

      /* Expect two additional modules, one for each:
       * - Scala.js core
       * - bar.B
       */
      assertEquals(5, moduleSet.modules.size)
    }
  }

  @Test
  def noCircularDepsThroughFineGrainedClasses_Issue4835(): AsyncResult = await {
    /* Test a particular shape of dependencies that used to produce modules
     * with cyclic dependencies. Because of that, it used to fail when creating
     * the ModuleSet with
     *   "requirement failed: Must have exactly one root module".
     * With that `require` statement disabled in the `ModuleSet` constructor,
     * it used to then fail in `checkNoCyclicDependencies`.
     */

    val SMF = EMF.withNamespace(MemberNamespace.PublicStatic)

    def methodHolder(name: ClassName, methodName: String, body: Tree): ClassDef = {
      classDef(name,
        kind = ClassKind.Interface,
        methods = List(
          MethodDef(SMF, m(methodName, Nil, I), NON, Nil, IntType, Some(body))(
              EOH.withNoinline(true), UNV)
        ))
    }

    def call(name: ClassName, methodName: String): Tree =
      ApplyStatic(EAF, name, m(methodName, Nil, I), Nil)(IntType)

    val EntryPointsClass = ClassName("lib.EntryPoints")
    val entryPointsClassDef = classDef(
      EntryPointsClass,
      superClass = Some(ObjectClass),
      methods = List(
        trivialCtor(EntryPointsClass)
      ),
      topLevelExportDefs = List(
        TopLevelMethodExportDef("moda",
            JSMethodDef(SMF, str("expa"), Nil, None, call("lib.A", "baz"))(EOH, UNV)),
        TopLevelMethodExportDef("modb",
            JSMethodDef(SMF, str("expb"), Nil, None, call("lib.A", "baz"))(EOH, UNV))
      )
    )

    val classDefs = Seq(
      entryPointsClassDef,
      methodHolder("lib.A", "baz", BinaryOp(BinaryOp.Int_+, call("app.C", "foo"), call("lib.B", "bar"))),
      methodHolder("lib.B", "bar", int(1)),
      methodHolder("app.C", "foo", BinaryOp(BinaryOp.Int_+, call("lib.B", "bar"), int(1)))
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("app")))
      .withSourceMap(false)

    for {
      moduleSet <- linkToModuleSet(classDefs, Nil, config = linkerConfig)
    } yield {
      checkNoCyclicDependencies(moduleSet)
    }
  }

  @Test
  def noCircularDepsThroughFineGrainedClasses2_Issue4835(): AsyncResult = await {
    /* Another situation with potential circular dependencies, which was
     * imagined while fixing #4835.
     */

    val SMF = EMF.withNamespace(MemberNamespace.PublicStatic)

    def methodHolder(name: ClassName, methodName: String, body: Tree): ClassDef = {
      classDef(name,
        kind = ClassKind.Interface,
        methods = List(
          MethodDef(SMF, m(methodName, Nil, I), NON, Nil, IntType, Some(body))(
              EOH.withNoinline(true), UNV)
        ))
    }

    def call(name: ClassName, methodName: String): Tree =
      ApplyStatic(EAF, name, m(methodName, Nil, I), Nil)(IntType)

    val EntryPointsClass = ClassName("entry.EntryPoints")
    val entryPointsClassDef = classDef(
      EntryPointsClass,
      superClass = Some(ObjectClass),
      methods = List(
        trivialCtor(EntryPointsClass)
      ),
      topLevelExportDefs = List(
        TopLevelMethodExportDef("moda",
            JSMethodDef(SMF, str("expa"), Nil, None, call("app.A", "baz"))(EOH, UNV)),
        TopLevelMethodExportDef("modb",
            JSMethodDef(SMF, str("expb"), Nil, None, call("app.A", "baz"))(EOH, UNV))
      )
    )

    val classDefs = Seq(
      entryPointsClassDef,
      methodHolder("app.A", "baz", call("lib.B", "bar")),
      methodHolder("lib.B", "bar", call("app.C", "foo")),
      methodHolder("app.C", "foo", call("lib.D", "bar")),
      methodHolder("lib.D", "bar", int(1))
    )

    val linkerConfig = StandardConfig()
      .withModuleKind(ModuleKind.ESModule)
      .withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("app")))
      .withSourceMap(false)

    for {
      moduleSet <- linkToModuleSet(classDefs, Nil, config = linkerConfig)
    } yield {
      checkNoCyclicDependencies(moduleSet)
    }
  }

  private def checkNoCyclicDependencies(moduleSet: ModuleSet): Unit = {
    val processedModuleIDs = mutable.Set.empty[ModuleSet.ModuleID]
    var remainingModules = moduleSet.modules

    /* At each step of the loop, find all the modules in `remainingModules` for
     * which all `internalDependencies` already belong to `processedModuleIDs`.
     * Remove them from `remainingModules` and add them to `processedModuleIDs`
     * instead.
     * If no such module can be found, it means that there is a cycle within
     * the remaining ones.
     * When `remainingModules` is empty, we have shown that there is no cycle.
     */
    while (remainingModules.nonEmpty) {
      val (newRoots, nextRemaining) = remainingModules.partition { m =>
        m.internalDependencies.forall(processedModuleIDs.contains(_))
      }
      if (newRoots.isEmpty)
        fail("Found cycle in modules: " + remainingModules.map(_.id).mkString(", "))

      for (root <- newRoots)
        processedModuleIDs += root.id
      remainingModules = nextRemaining
    }
  }
}
