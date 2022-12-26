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
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.linker.interface._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.testutils.TestIRBuilder._

class SmallModulesForSplittingTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def splitsModules(): AsyncResult = await {
    /* Test splitting in the degenerate case, where dependencies traverse the
     * split boundary multiple times.
     */
    val strClsType = ClassType(BoxedStringClass)

    val methodName = m("get", Nil, T)

    val SMF = EMF.withNamespace(MemberNamespace.PublicStatic)

    def methodHolder(name: ClassName, body: Tree) = {
      classDef(name,
        kind = ClassKind.Interface,
        memberDefs = List(
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

      assertEquals(List[ClassName]("foo.A"), moduleClasses("foo.A"))
      assertEquals(List[ClassName]("foo.C"), moduleClasses("foo.C"))
      assertEquals(List(MainTestClassName), moduleClasses("main"))

      /* Expect two additional modules, one for each:
       * - Scala.js core
       * - bar.B
       */
      assertEquals(5, moduleSet.modules.size)
    }
  }
}
