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

import org.scalajs.ir.{Types => jstpe}
import org.scalajs.ir.Trees.LinkTimeTree
import org.scalajs.ir.Trees.LinkTimeOp
import org.scalajs.linker.interface.{Semantics, ESFeatures}

final class LinkTimeProperties private[standard] (
  semantics: Semantics,
  esFeatures: ESFeatures
) {
  import LinkTimeProperties._
  import LinkTimeProperties.ResolvedLinkTimeTree._

  private val linkTimeProperties: Map[String, ResolvedLinkTimeTree] = Map(
    // Must be in sync with the arguments of @linkTimeProperty("...")
    // for the fields in `scala.scalajs.LinkingInfo`.
    "core/productionMode" -> BooleanValue(semantics.productionMode),
    "core/esVersion" -> IntValue(esFeatures.esVersion.edition)
  )

  def evaluateLinkTimeTree(cond: LinkTimeTree): Boolean = {
    eval(cond) match {
      case BooleanValue(v) => v
      case IntValue(v) =>
        throw new IllegalArgumentException(
            "Link-time condition must be evaluated to be a boolean value, but int is found.")
    }
  }

  def exist(name: String, tpe: jstpe.Type): Boolean =
    linkTimeProperties.get(name).exists {
      case IntValue(_)     => tpe == jstpe.IntType
      case BooleanValue(_) => tpe == jstpe.BooleanType
    }

  private def eval(cond: LinkTimeTree): ResolvedLinkTimeTree = cond match {
    case LinkTimeTree.BinaryOp(op, lhs, rhs) =>
      ResolvedLinkTimeTree.BooleanValue {
        (eval(lhs), eval(rhs)) match {
          case (IntValue(l), IntValue(r)) =>
            op match {
              case LinkTimeOp.Int_== => l == r
              case LinkTimeOp.Int_!= => l != r
              case LinkTimeOp.Int_<  => l < r
              case LinkTimeOp.Int_<= => l <= r
              case LinkTimeOp.Int_>  => l > r
              case LinkTimeOp.Int_>= => l >= r
              case _ =>
                throw new IllegalArgumentException(s"Invalid operation $op for int values.")
            }
          case (BooleanValue(l), BooleanValue(r)) =>
            op match {
              case LinkTimeOp.Boolean_== => l == r
              case LinkTimeOp.Boolean_!= => l != r
              case LinkTimeOp.Boolean_|| => l || r
              case LinkTimeOp.Boolean_&& => l && r
              case _ =>
                throw new IllegalArgumentException(s"Invalid operation $op for boolean values.")
            }
          case _ =>
            throw new IllegalArgumentException("Type mismatch: binary operation with different types " +
              "is not allowed in linkTimeIf.")
        }
      }
    case LinkTimeTree.BooleanConst(v)   => BooleanValue(v)
    case LinkTimeTree.IntConst(v)       => IntValue(v)
    case LinkTimeTree.Property(name, _) => resolveLinkTimeProperty(name)
  }

  private def resolveLinkTimeProperty(prop: String): ResolvedLinkTimeTree =
    linkTimeProperties.getOrElse(prop, throw new IllegalArgumentException(s"link time property not found: '$prop'"))
}

object LinkTimeProperties {
  private sealed abstract class ResolvedLinkTimeTree
  private object ResolvedLinkTimeTree {
    case class IntValue(v: Int) extends ResolvedLinkTimeTree
    case class BooleanValue(v: Boolean) extends ResolvedLinkTimeTree
  }

  private[linker] val Defaults: LinkTimeProperties =
    new LinkTimeProperties(
        Semantics.Defaults,
        ESFeatures.Defaults
    )
}
