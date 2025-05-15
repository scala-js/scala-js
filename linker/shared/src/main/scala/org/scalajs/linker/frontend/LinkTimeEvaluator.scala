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

import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Trees.LinkTimeProperty._

import org.scalajs.linker.frontend.LinkTimeProperties._
import org.scalajs.linker.interface.LinkingException

private[linker] object LinkTimeEvaluator {

  /** Try and evaluate a link-time expression tree as a boolean value.
   *
   *  This method assumes that the given `tree` is valid according to the
   *  `ClassDefChecker` and that its `tpe` is `BooleanType`.
   *  If that is not the case, it may throw or return an arbitrary result.
   *
   *  Returns `None` if any subtree that needed evaluation was a missing
   *  `LinkTimeProperty` or one with the wrong type (i.e., one that would not
   *  pass the reachability analysis).
   */
  def tryEvalLinkTimeBooleanExpr(
      linkTimeProperties: LinkTimeProperties, tree: Tree): Option[Boolean] = {
    implicit val pos = tree.pos

    tryEvalLinkTimeExpr(linkTimeProperties, tree).map(booleanValue(_))
  }

  /** Try and evaluate a link-time expression tree.
   *
   *  This method assumes that the given `tree` is valid according to the
   *  `ClassDefChecker`.
   *  If that is not the case, it may throw or return an arbitrary result.
   *
   *  Returns `None` if any subtree that needed evaluation was a missing
   *  `LinkTimeProperty` or one with the wrong type (i.e., one that would not
   *  pass the reachability analysis).
   */
  private def tryEvalLinkTimeExpr(
      props: LinkTimeProperties, tree: Tree): Option[LinkTimeValue] = {
    implicit val pos = tree.pos

    tree match {
      case IntLiteral(value)     => Some(LinkTimeInt(value))
      case BooleanLiteral(value) => Some(LinkTimeBoolean(value))
      case StringLiteral(value)  => Some(LinkTimeString(value))

      case LinkTimeProperty(name) =>
        props.get(name).filter(_.tpe == tree.tpe)

      case UnaryOp(op, lhs) =>
        import UnaryOp._
        for {
          l <- tryEvalLinkTimeExpr(props, lhs)
        } yield {
          op match {
            case Boolean_! => LinkTimeBoolean(!booleanValue(l))

            case _ =>
              throw new LinkingException(
                  s"Illegal unary op $op in link-time tree at $pos")
          }
        }

      case BinaryOp(op, lhs, rhs) =>
        import BinaryOp._
        for {
          l <- tryEvalLinkTimeExpr(props, lhs)
          r <- tryEvalLinkTimeExpr(props, rhs)
        } yield {
          op match {
            case Boolean_== => LinkTimeBoolean(booleanValue(l) == booleanValue(r))
            case Boolean_!= => LinkTimeBoolean(booleanValue(l) != booleanValue(r))
            case Boolean_|  => LinkTimeBoolean(booleanValue(l) | booleanValue(r))
            case Boolean_&  => LinkTimeBoolean(booleanValue(l) & booleanValue(r))

            case Int_== => LinkTimeBoolean(intValue(l) == intValue(r))
            case Int_!= => LinkTimeBoolean(intValue(l) != intValue(r))
            case Int_<  => LinkTimeBoolean(intValue(l) < intValue(r))
            case Int_<= => LinkTimeBoolean(intValue(l) <= intValue(r))
            case Int_>  => LinkTimeBoolean(intValue(l) > intValue(r))
            case Int_>= => LinkTimeBoolean(intValue(l) >= intValue(r))

            case _ =>
              throw new LinkingException(
                  s"Illegal binary op $op in link-time tree at $pos")
          }
        }

      case LinkTimeIf(cond, thenp, elsep) =>
        tryEvalLinkTimeExpr(props, cond).flatMap { c =>
          if (booleanValue(c))
            tryEvalLinkTimeExpr(props, thenp)
          else
            tryEvalLinkTimeExpr(props, elsep)
        }

      case _ =>
        throw new LinkingException(
            s"Illegal tree of class ${tree.getClass().getName()} in link-time tree at $pos")
    }
  }

  private def intValue(value: LinkTimeValue)(implicit pos: Position): Int = value match {
    case LinkTimeInt(value) =>
      value
    case _ =>
      throw new LinkingException(s"Value of type int expected but got $value at $pos")
  }

  private def booleanValue(value: LinkTimeValue)(implicit pos: Position): Boolean = value match {
    case LinkTimeBoolean(value) =>
      value
    case _ =>
      throw new LinkingException(s"Value of type boolean expected but got $value at $pos")
  }
}
