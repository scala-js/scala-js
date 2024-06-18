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

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.logging._

import org.scalajs.linker.checker.ClassDefChecker
import org.scalajs.linker.interface.StandardConfig
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils.TestIRBuilder._
import org.scalajs.linker.testutils.LinkingUtils._
import org.scalajs.linker.interface.{Semantics, ESFeatures}

class BaseLinkerTest {
  import scala.concurrent.ExecutionContext.Implicits.global

  @Test
  def noUnnecessaryDefaultBridges(): AsyncResult = await {
    val fooName = m("foo", Nil, IntRef)
    val classDefs = Seq(
      classDef(
          "Intf",
          kind = ClassKind.Interface,
          methods = List(
            MethodDef(EMF, fooName, NON, Nil, IntType, Some(int(1)))(EOH, UNV))
      ),
      classDef(
          "Base",
          kind = ClassKind.Class,
          superClass = Some(ObjectClass),
          interfaces = List("Intf"),
          methods = List(trivialCtor("Base"))
      ),
      classDef(
          "Sub",
          kind = ClassKind.Class,
          superClass = Some("Base"),
          methods = List(trivialCtor("Sub"))
      ),
      mainTestClassDef(
        consoleLog(Apply(EAF, New("Sub", NoArgConstructorName, Nil), fooName, Nil)(IntType))
      )
    )

    val config = StandardConfig().withOptimizer(false)

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers, config = config)) yield {
      val clazz = findClass(moduleSet, "Sub").get
      assertFalse(clazz.methods.exists(_.name.name == fooName))
    }
  }

  @Test
  def correctThisTypeInHijackedClassReflectiveProxies_Issue4982(): AsyncResult = await {
    val compareTo = m("compareTo", List(ClassRef(BoxedIntegerClass)), IntRef)
    val compareToReflProxy =
      MethodName.reflectiveProxy("compareTo", List(ClassRef(BoxedIntegerClass)))

    val classDefs = Seq(
      mainTestClassDef(
        consoleLog(Apply(EAF, IntLiteral(5), compareToReflProxy, List(IntLiteral(6)))(AnyType))
      )
    )

    val config = StandardConfig().withOptimizer(false)

    for (moduleSet <- linkToModuleSet(classDefs, MainTestModuleInitializers, config = config)) yield {
      val clazz = findClass(moduleSet, BoxedIntegerClass).get
      val errorCount = ClassDefChecker.check(clazz, postOptimizer = false,
          new ScalaConsoleLogger(Level.Error), LinkTimeProperties.Defaults)
      assertEquals(0, errorCount)
    }
  }

  private def findClass(moduleSet: ModuleSet, name: ClassName): Option[LinkedClass] =
    moduleSet.modules.flatMap(_.classDefs).find(_.className == name)
}
