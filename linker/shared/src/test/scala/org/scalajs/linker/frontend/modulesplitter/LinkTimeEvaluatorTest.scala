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

package org.scalajs.linker.frontend

import org.junit.Test
import org.junit.Assert._

import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.{ESFeatures, ESVersion, Semantics, StandardConfig}
import org.scalajs.linker.standard.CoreSpec
import org.scalajs.linker.testutils.TestIRBuilder._

class LinkTimeEvaluatorTest {
  /** Convenience builder for `LinkTimeProperties` with mostly-default configs. */
  private def make(
    semantics: Semantics => Semantics = identity,
    esFeatures: ESFeatures => ESFeatures = identity,
    isWebAssembly: Boolean = false
  ): LinkTimeProperties = {
    val config = StandardConfig()
      .withSemantics(semantics)
      .withESFeatures(esFeatures)
      .withExperimentalUseWebAssembly(isWebAssembly)
    LinkTimeProperties.fromCoreSpec(CoreSpec.fromStandardConfig(config))
  }

  @Test
  def testTryEvalLinkTimeBooleanExpr(): Unit = {
    val defaults = make()

    def test(expected: Option[Boolean], tree: Tree, config: LinkTimeProperties = defaults): Unit =
      assertEquals(expected, LinkTimeEvaluator.tryEvalLinkTimeBooleanExpr(config, tree))

    def testTrue(tree: Tree, config: LinkTimeProperties = defaults): Unit =
      test(Some(true), tree, config)

    def testFalse(tree: Tree, config: LinkTimeProperties = defaults): Unit =
      test(Some(false), tree, config)

    def testFail(tree: Tree, config: LinkTimeProperties = defaults): Unit =
      test(None, tree, config)

    // Boolean literal
    testTrue(bool(true))
    testFalse(bool(false))

    // Boolean link-time property
    testFalse(LinkTimeProperty("core/isWebAssembly")(BooleanType))
    testTrue(LinkTimeProperty("core/isWebAssembly")(BooleanType), make(isWebAssembly = true))
    testFail(LinkTimeProperty("core/missing")(BooleanType))
    testFail(LinkTimeProperty("core/esVersion")(BooleanType))

    // Int comparison
    for (l <- List(3, 5, 7); r <- List(3, 5, 7)) {
      test(Some(l == r), BinaryOp(BinaryOp.Int_==, int(l), int(r)))
      test(Some(l != r), BinaryOp(BinaryOp.Int_!=, int(l), int(r)))
      test(Some(l < r), BinaryOp(BinaryOp.Int_<, int(l), int(r)))
      test(Some(l <= r), BinaryOp(BinaryOp.Int_<=, int(l), int(r)))
      test(Some(l > r), BinaryOp(BinaryOp.Int_>, int(l), int(r)))
      test(Some(l >= r), BinaryOp(BinaryOp.Int_>=, int(l), int(r)))
    }

    // Boolean operator
    testTrue(UnaryOp(UnaryOp.Boolean_!, bool(false)))
    testFalse(UnaryOp(UnaryOp.Boolean_!, bool(true)))

    // Comparison with link-time property
    val esVersionProp = LinkTimeProperty("core/esVersion")(IntType)
    testTrue(BinaryOp(BinaryOp.Int_>=, esVersionProp, int(ESVersion.ES2015.edition)))
    testFalse(BinaryOp(BinaryOp.Int_>=, esVersionProp, int(ESVersion.ES2019.edition)))
    testTrue(BinaryOp(BinaryOp.Int_>=, esVersionProp, int(ESVersion.ES2019.edition)),
        make(esFeatures = _.withESVersion(ESVersion.ES2021)))

    // LinkTimeIf
    testTrue(LinkTimeIf(bool(true), bool(true), bool(false))(BooleanType))
    testFalse(LinkTimeIf(bool(true), bool(false), bool(true))(BooleanType))
    testFalse(LinkTimeIf(bool(false), bool(true), bool(false))(BooleanType))

    // Complex expression: esVersion >= ES2016 && esVersion <= ES2019
    val complexExpr = LinkTimeIf(
        BinaryOp(BinaryOp.Int_>=, esVersionProp, int(ESVersion.ES2016.edition)),
        BinaryOp(BinaryOp.Int_<=, esVersionProp, int(ESVersion.ES2019.edition)),
        bool(false))(
        BooleanType)
    testTrue(complexExpr, make(esFeatures = _.withESVersion(ESVersion.ES2017)))
    testTrue(complexExpr, make(esFeatures = _.withESVersion(ESVersion.ES2019)))
    testFalse(complexExpr, make(esFeatures = _.withESVersion(ESVersion.ES2015)))
    testFalse(complexExpr, make(esFeatures = _.withESVersion(ESVersion.ES2021)))
  }
}
