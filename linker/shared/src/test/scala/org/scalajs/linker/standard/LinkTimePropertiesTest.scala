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

package org.scalajs.linker.standard

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.{Trees => js, Types => jstpe}
import org.scalajs.ir.Position
import org.scalajs.linker.interface.{Semantics, ESFeatures, ESVersion}

import js.LinkTimeOp._
import js.LinkTimeTree._

class LinkTimePropertiesTest {
  private def evalTrue(prop: LinkTimeProperties, cond: js.LinkTimeTree): Unit =
    assertTrue(prop.evaluateLinkTimeTree(cond))
  private def evalFalse(prop: LinkTimeProperties, cond: js.LinkTimeTree): Unit =
    assertFalse(prop.evaluateLinkTimeTree(cond))

  private def i(x: Int) = js.LinkTimeTree.IntConst(x)
  private def b(x: Boolean) = js.LinkTimeTree.BooleanConst(x)
  private def p(x: String, t: jstpe.Type) = js.LinkTimeTree.Property(x, t)

  private val productionMode =
    p("core/productionMode", jstpe.BooleanType)
  private val esVersion =
    p("core/esVersion", jstpe.IntType)

  private implicit val noPos: Position = Position.NoPosition

  @Test
  def evaluateLinkTimeTreeConst(): Unit = {
    val prop = new LinkTimeProperties(
      Semantics.Defaults,
      ESFeatures.Defaults
    )

    evalTrue(prop, b(true))
    evalFalse(prop, b(false))

    evalTrue(prop, BinaryOp(Boolean_==, b(true), b(true)))
    evalTrue(prop, BinaryOp(Boolean_!=, b(true), b(false)))
    evalFalse(prop, BinaryOp(Boolean_==, b(true), b(false)))
    evalFalse(prop, BinaryOp(Boolean_!=, b(true), b(true)))

    evalTrue(prop, BinaryOp(Int_!=, i(0), i(3)))
    evalTrue(prop, BinaryOp(Int_==, i(0), i(0)))
    evalTrue(prop, BinaryOp(Int_>,  i(1), i(0)))
    evalTrue(prop, BinaryOp(Int_>=, i(0), i(0)))
    evalTrue(prop, BinaryOp(Int_<,  i(0), i(1)))
    evalTrue(prop, BinaryOp(Int_<=, i(0), i(0)))
  }

  @Test
  def resolveLinkTimeProperty(): Unit = {
    val sem = Semantics.Defaults.withProductionMode(true)
    val esFeatures = ESFeatures.Defaults.withESVersion(ESVersion.ES2015)
    val prop = new LinkTimeProperties(sem, esFeatures)

    evalTrue(prop, productionMode)
    evalTrue(prop, BinaryOp(Boolean_==, productionMode, b(sem.productionMode)))
    evalTrue(prop, BinaryOp(Boolean_!=, b(!sem.productionMode), productionMode))
    evalTrue(prop, BinaryOp(Int_==, i(esFeatures.esVersion.edition), esVersion))
    evalTrue(prop, BinaryOp(Int_>, i(ESVersion.ES2016.edition), esVersion))
  }

  @Test
  def linkTimeConditionNested(): Unit = {
    val sem = Semantics.Defaults.withProductionMode(true)
    val esFeatures = ESFeatures.Defaults.withESVersion(ESVersion.ES2015)
    val prop = new LinkTimeProperties(sem, esFeatures)

    // esVersion >= ESVersion.ES2015 && esVersion <= ESVersion.ES2019
    evalTrue(prop,
      BinaryOp(
        Boolean_&&,
        BinaryOp(Int_>=, esVersion, i(ESVersion.ES2015.edition)),
        BinaryOp(Int_<=, esVersion, i(ESVersion.ES2019.edition))
      )
    )

    // (esVersion > ESVersion.ES5_1 && esVersion < ESVersion.ES2015) || productionMode
    evalTrue(prop,
      BinaryOp(
        Boolean_||,
        BinaryOp(Boolean_&&,
          BinaryOp(Int_>, esVersion, i(ESVersion.ES5_1.edition)),
          BinaryOp(Int_<, esVersion, i(ESVersion.ES2015.edition))
        ),
        productionMode
      )
    )
  }

  @Test
  def linkTimePropertyNotFound(): Unit = {
    val prop = new LinkTimeProperties(Semantics.Defaults, ESFeatures.Defaults)
    val tree = BinaryOp(
        Boolean_||,
        p("prop/notFound", jstpe.BooleanType),
        b(true)
    )
    assertThrows(classOf[IllegalArgumentException], () => prop.evaluateLinkTimeTree(tree))
  }

  @Test
  def linkTimePropertyInvalidInput(): Unit = {
    val prop = new LinkTimeProperties(Semantics.Defaults, ESFeatures.Defaults)
    def test(tree: js.LinkTimeTree): Unit =
      assertThrows(classOf[IllegalArgumentException], () => prop.evaluateLinkTimeTree(tree))

    test(p("core/esVersion", jstpe.IntType))
    test(BinaryOp(Boolean_||, i(1), b(true)))
    test(BinaryOp(Boolean_||, i(1), i(10)))
    test(BinaryOp(Int_==, b(true), b(false)))
  }
}
