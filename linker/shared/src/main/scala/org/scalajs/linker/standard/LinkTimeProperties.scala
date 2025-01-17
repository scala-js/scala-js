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

import org.scalajs.ir.Trees._
import org.scalajs.ir.Trees.LinkTimeProperty._
import org.scalajs.ir.Types._
import org.scalajs.ir.ScalaJSVersions

import org.scalajs.linker.interface.{ESVersion => _, _}

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
      LinkTimeBoolean(esFeatures.useECMAScript2015Semantics),
    IsWebAssembly ->
      LinkTimeBoolean(targetIsWebAssembly),
    ProductionMode ->
      LinkTimeBoolean(semantics.productionMode),
    LinkerVersion ->
      LinkTimeString(ScalaJSVersions.current)
  )

  def validate(name: String, tpe: Type): Boolean = {
    linkTimeProperties.get(name).exists {
      case _: LinkTimeBoolean => tpe == BooleanType
      case _: LinkTimeInt     => tpe == IntType
      case _: LinkTimeString  => tpe == StringType
    }
  }

  def transformLinkTimeProperty(prop: LinkTimeProperty): Literal = {
    val value = linkTimeProperties.getOrElse(prop.name,
        throw new IllegalArgumentException(s"link time property not found: '${prop.name}' of type ${prop.tpe}"))
    value match {
      case LinkTimeBoolean(value) =>
        BooleanLiteral(value)(prop.pos)
      case LinkTimeInt(value) =>
        IntLiteral(value)(prop.pos)
      case LinkTimeString(value) =>
        StringLiteral(value)(prop.pos)
    }
  }

  /** Try and evaluate a link-time expression tree as a boolean value.
   *
   *  This method assumes that the given `tree` is valid according to the
   *  `ClassDefChecker` and that its `tpe` is `BooleanType`.
   *
   *  Returns `None` if any subtree that needed evaluation was an invalid
   *  `LinkTimeProperty` (i.e., one that does not pass the [[validate]] test).
   */
  def tryEvalLinkTimeBooleanExpr(tree: Tree): Option[Boolean] =
    tryEvalLinkTimeExpr(tree).map(_.booleanValue)

  /** Try and evaluate a link-time expression tree.
   *
   *  This method assumes that the given `tree` is valid according to the
   *  `ClassDefChecker`.
   *
   *  Returns `None` if any subtree that needed evaluation was an invalid
   *  `LinkTimeProperty` (i.e., one that does not pass the [[validate]] test).
   */
  def tryEvalLinkTimeExpr(tree: Tree): Option[LinkTimeValue] = tree match {
    case IntLiteral(value)     => Some(LinkTimeInt(value))
    case BooleanLiteral(value) => Some(LinkTimeBoolean(value))
    case StringLiteral(value)  => Some(LinkTimeString(value))

    case LinkTimeProperty(name) =>
      if (validate(name, tree.tpe))
        Some(linkTimeProperties(name))
      else
        None

    case UnaryOp(op, lhs) =>
      import UnaryOp._
      for {
        l <- tryEvalLinkTimeExpr(lhs)
      } yield {
        op match {
          case Boolean_! => LinkTimeBoolean(!l.booleanValue)

          case _ =>
            throw new LinkingException(
                s"Illegal unary op $op in link-time tree at ${tree.pos}")
        }
      }

    case BinaryOp(op, lhs, rhs) =>
      import BinaryOp._
      for {
        l <- tryEvalLinkTimeExpr(lhs)
        r <- tryEvalLinkTimeExpr(rhs)
      } yield {
        op match {
          case Boolean_== => LinkTimeBoolean(l.booleanValue == r.booleanValue)
          case Boolean_!= => LinkTimeBoolean(l.booleanValue != r.booleanValue)
          case Boolean_|  => LinkTimeBoolean(l.booleanValue | r.booleanValue)
          case Boolean_&  => LinkTimeBoolean(l.booleanValue & r.booleanValue)

          case Int_== => LinkTimeBoolean(l.intValue == r.intValue)
          case Int_!= => LinkTimeBoolean(l.intValue != r.intValue)
          case Int_<  => LinkTimeBoolean(l.intValue < r.intValue)
          case Int_<= => LinkTimeBoolean(l.intValue <= r.intValue)
          case Int_>  => LinkTimeBoolean(l.intValue > r.intValue)
          case Int_>= => LinkTimeBoolean(l.intValue >= r.intValue)

          case _ =>
            throw new LinkingException(
                s"Illegal binary op $op in link-time tree at ${tree.pos}")
        }
      }

    case LinkTimeIf(cond, thenp, elsep) =>
      tryEvalLinkTimeExpr(cond).flatMap { c =>
        if (c.booleanValue)
          tryEvalLinkTimeExpr(thenp)
        else
          tryEvalLinkTimeExpr(elsep)
      }

    case _ =>
      throw new LinkingException(
          s"Illegal tree of class ${tree.getClass().getName()} in link-time tree at ${tree.pos}")
  }
}

private[linker] object LinkTimeProperties {
  sealed abstract class LinkTimeValue {
    def intValue: Int = this.asInstanceOf[LinkTimeInt].value

    def booleanValue: Boolean = this.asInstanceOf[LinkTimeBoolean].value

    def stringValue: String = this.asInstanceOf[LinkTimeString].value
  }

  final case class LinkTimeInt(value: Int) extends LinkTimeValue
  final case class LinkTimeBoolean(value: Boolean) extends LinkTimeValue
  final case class LinkTimeString(value: String) extends LinkTimeValue
}
