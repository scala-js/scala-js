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

import org.scalajs.ir.{Types => jstpe, Trees => js}
import org.scalajs.ir.Trees.LinkTimeProperty._
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.Position.NoPosition
import org.scalajs.linker.interface.{Semantics, ESFeatures}

private[linker] final class LinkTimeProperties (
  semantics: Semantics,
  esFeatures: ESFeatures,
  targetIsWebAssembly: Boolean
) {
  import LinkTimeProperties._

  private val linkTimeProperties: Map[String, LinkTimeValue] = Map(
    ESVersion ->
      LinkTimeInt(esFeatures.esVersion.edition),
    UseECMAScript2015Semantics ->
      LinkTimeBoolean(true), // historical; always true
    IsWebAssembly ->
      LinkTimeBoolean(targetIsWebAssembly),
    ProductionMode ->
      LinkTimeBoolean(semantics.productionMode),
    LinkerVersion ->
      LinkTimeString(ScalaJSVersions.current)
  )

  def validate(name: String, tpe: jstpe.Type): Boolean = {
    linkTimeProperties.get(name).exists {
      case _: LinkTimeBoolean => tpe == jstpe.BooleanType
      case _: LinkTimeInt     => tpe == jstpe.IntType
      case _: LinkTimeString  => tpe == jstpe.StringType
    }
  }

  def transformLinkTimeProperty(prop: js.LinkTimeProperty): js.Literal = {
    val value = linkTimeProperties.getOrElse(prop.name,
        throw new IllegalArgumentException(s"link time property not found: '${prop.name}' of type ${prop.tpe}"))
    value match {
      case LinkTimeBoolean(value) =>
        js.BooleanLiteral(value)(prop.pos)
      case LinkTimeInt(value) =>
        js.IntLiteral(value)(prop.pos)
      case LinkTimeString(value) =>
        js.StringLiteral(value)(prop.pos)
    }
  }
}

private[linker] object LinkTimeProperties {
  sealed abstract class LinkTimeValue
  final case class LinkTimeInt(value: Int) extends LinkTimeValue
  final case class LinkTimeBoolean(value: Boolean) extends LinkTimeValue
  final case class LinkTimeString(value: String) extends LinkTimeValue
}
