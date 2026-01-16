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

import org.scalajs.ir.Trees.LinkTimeProperty._
import org.scalajs.ir.Types._
import org.scalajs.ir.ScalaJSVersions

import org.scalajs.linker.interface.{ESVersion => _, _}
import org.scalajs.linker.standard.CoreSpec

final class LinkTimeProperties private (
    semantics: Semantics,
    esFeatures: ESFeatures,
    targetIsWebAssembly: Boolean
) {
  import LinkTimeProperties._

  private val linkTimeProperties: Map[String, LinkTimeValue] = Map(
    ESVersion ->
    LinkTimeInt(esFeatures.esVersion.edition),
    UseECMAScript2015Semantics ->
    LinkTimeBoolean(esFeatures.useECMAScript2015Semantics),
    IsWebAssembly ->
    LinkTimeBoolean(targetIsWebAssembly),
    ProductionMode ->
    LinkTimeBoolean(semantics.productionMode),
    LinkerVersion ->
    LinkTimeString(ScalaJSVersions.current)
  )

  def get(name: String): Option[LinkTimeValue] =
    linkTimeProperties.get(name)
}

object LinkTimeProperties {
  sealed abstract class LinkTimeValue(val tpe: Type)

  final case class LinkTimeInt(value: Int) extends LinkTimeValue(IntType)

  final case class LinkTimeBoolean(value: Boolean) extends LinkTimeValue(BooleanType)

  final case class LinkTimeString(value: String) extends LinkTimeValue(StringType) {
    // Being extra careful
    require(value != null, "LinkTimeString requires a non-null value.")
  }

  def fromCoreSpec(coreSpec: CoreSpec): LinkTimeProperties = {
    new LinkTimeProperties(coreSpec.semantics, coreSpec.esFeatures,
        coreSpec.targetIsWebAssembly)
  }
}
