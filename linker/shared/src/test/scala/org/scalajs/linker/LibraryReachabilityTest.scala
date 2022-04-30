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

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.junit.async._

import org.scalajs.linker.analyzer._
import org.scalajs.linker.interface._
import org.scalajs.linker.standard._

import org.scalajs.linker.testutils._
import org.scalajs.linker.testutils.TestIRBuilder._

class LibraryReachabilityTest {
  import scala.concurrent.ExecutionContext.Implicits.global
  import LibraryReachabilityTest._

  @Test
  def juPropertiesNotReachableWhenUsingGetSetClearProperty(): AsyncResult = await {
    val systemMod = LoadModule("java.lang.System$")
    val emptyStr = str("")
    val StringType = ClassType(BoxedStringClass)

    val classDefs = Seq(
        classDef("A", superClass = Some(ObjectClass), memberDefs = List(
            trivialCtor("A"),
            MethodDef(EMF, m("test", Nil, V), NON, Nil, NoType, Some(Block(
                Apply(EAF, systemMod, m("getProperty", List(T), T), List(emptyStr))(StringType),
                Apply(EAF, systemMod, m("getProperty", List(T, T), T), List(emptyStr, emptyStr))(StringType),
                Apply(EAF, systemMod, m("setProperty", List(T, T), T), List(emptyStr, emptyStr))(StringType),
                Apply(EAF, systemMod, m("clearProperty", List(T), T), List(emptyStr))(StringType)
            )))(EOH, None)
        ))
    )

    for {
      analysis <- computeAnalysis(classDefs,
          reqsFactory.instantiateClass("A", NoArgConstructorName) ++
          reqsFactory.callMethod("A", m("test", Nil, V)))
    } yield {
      val juPropertiesClass = analysis.classInfos("java.util.Properties")
      assertFalse(juPropertiesClass.isAnySubclassInstantiated)
      assertFalse(juPropertiesClass.areInstanceTestsUsed)
      assertFalse(juPropertiesClass.isDataAccessed)
    }
  }

  @Test
  def jmBigNumbersNotInstantiatedWhenUsingStringFormat(): AsyncResult = await {
    val StringType = ClassType(BoxedStringClass)
    val formatMethod = m("format", List(T, ArrayTypeRef(O, 1)), T)

    val classDefs = Seq(
      classDef("A", superClass = Some(ObjectClass), memberDefs = List(
        trivialCtor("A"),
        MethodDef(EMF, m("test", Nil, V), NON, Nil, NoType, Some(Block(
          ApplyStatic(EAF, BoxedStringClass, formatMethod, List(str("hello %d"), int(42)))(StringType)
        )))(EOH, None)
      ))
    )

    for {
      analysis <- computeAnalysis(classDefs,
          reqsFactory.instantiateClass("A", NoArgConstructorName) ++
          reqsFactory.callMethod("A", m("test", Nil, V)))
    } yield {
      val jmBigIntegerClass = analysis.classInfos("java.math.BigInteger")
      assertFalse(jmBigIntegerClass.isAnySubclassInstantiated)
      assertFalse(jmBigIntegerClass.isDataAccessed)
      assertTrue(jmBigIntegerClass.areInstanceTestsUsed)

      val jmBigDecimalClass = analysis.classInfos("java.math.BigDecimal")
      assertFalse(jmBigDecimalClass.isAnySubclassInstantiated)
      assertFalse(jmBigDecimalClass.isDataAccessed)
      assertTrue(jmBigDecimalClass.areInstanceTestsUsed)
    }
  }
}

object LibraryReachabilityTest {
  private val reqsFactory = SymbolRequirement.factory("unit test")

  def computeAnalysis(classDefs: Seq[ClassDef],
      symbolRequirements: SymbolRequirement = reqsFactory.none(),
      moduleInitializers: Seq[ModuleInitializer] = Nil,
      config: StandardConfig = StandardConfig())(
      implicit ec: ExecutionContext): Future[Analysis] = {
    for {
      analysis <- LinkingUtils.computeAnalysis(classDefs, symbolRequirements,
          moduleInitializers, config, stdlib = TestIRRepo.fulllib)
    } yield {
      if (analysis.errors.nonEmpty)
        fail(analysis.errors.mkString("Unexpected errors:\n", "\n", ""))

      analysis
    }
  }
}
