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

package org.scalajs.linker.frontend.optimizer

import scala.annotation.switch

import java.lang.{Long => JLong}

import org.scalajs.ir.Position.NoPosition
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees._

/** An IR interpreter for elementary operations, which we use for fuzzing. */
object ElementaryInterpreter {
  def eval(tree: Tree, args: (LocalName, Literal)*): Literal =
    eval(tree)(Env.fromArgs(args.toMap))

  private def eval(tree: Tree)(implicit env: Env): Literal = tree match {
    case lit: Literal =>
      lit

    case VarRef(name) =>
      env.local(name)

    case Block(stats) =>
      val exprEnv = stats.init.foldLeft(env)((env, stat) => evalBlockStat(stat)(env))
      eval(stats.last)(exprEnv)

    case tree: VarDef =>
      evalBlockStat(tree)
      Undefined()(NoPosition)

    case UnaryOp(op, arg) =>
      evalUnaryOp(op, eval(arg))

    case BinaryOp(op, lhs, rhs) =>
      evalBinaryOp(op, eval(lhs), eval(rhs))

    case _ =>
      throw new UnsupportedOperationException(
          s"Unsupported tree in the ElementaryInterpreter, of class " +
          s"${tree.getClass().getSimpleName()}:\n${tree.show}")
  }

  private def evalBlockStat(stat: Tree)(implicit env: Env): Env = {
    stat match {
      case VarDef(LocalIdent(name), _, _, mutable, rhs) =>
        if (mutable) {
          throw new UnsupportedOperationException(
              s"Unsupported mutable local var in the ElementaryInterpreter:\n${stat.show}")
        }
        env.withLocal(name, eval(rhs))

      case _ =>
        // dead code, since we don't support any side effect, but why not
        eval(stat)
        env
    }
  }

  private def evalUnaryOp(op: UnaryOp.Code, arg: Literal): Literal = {
    import UnaryOp._

    (op: @switch) match {
      case IntToLong         => long(asInt(arg).toLong)
      case LongToInt         => int(asLong(arg).toInt)
      case UnsignedIntToLong => long(Integer.toUnsignedLong(asInt(arg)))

      case _ =>
        throw new UnsupportedOperationException(
            s"Unsupported unary op in the ElementaryInterpreter\n" +
            s"${UnaryOp(op, arg)(NoPosition).show}")
    }
  }

  private def evalBinaryOp(op: UnaryOp.Code, lhs: Literal, rhs: Literal): Literal = {
    import BinaryOp._

    (op: @switch) match {
      case Int_+ => int(asInt(lhs) + asInt(rhs))
      case Int_- => int(asInt(lhs) - asInt(rhs))
      case Int_* => int(asInt(lhs) * asInt(rhs))
      case Int_/ => int(asInt(lhs) / asInt(rhs))
      case Int_% => int(asInt(lhs) % asInt(rhs))

      case Int_|   => int(asInt(lhs) | asInt(rhs))
      case Int_&   => int(asInt(lhs) & asInt(rhs))
      case Int_^   => int(asInt(lhs) ^ asInt(rhs))
      case Int_<<  => int(asInt(lhs) << asInt(rhs))
      case Int_>>> => int(asInt(lhs) >>> asInt(rhs))
      case Int_>>  => int(asInt(lhs) >> asInt(rhs))

      case Long_+ => long(asLong(lhs) + asLong(rhs))
      case Long_- => long(asLong(lhs) - asLong(rhs))
      case Long_* => long(asLong(lhs) * asLong(rhs))
      case Long_/ => long(asLong(lhs) / asLong(rhs))
      case Long_% => long(asLong(lhs) % asLong(rhs))

      case Long_|   => long(asLong(lhs) | asLong(rhs))
      case Long_&   => long(asLong(lhs) & asLong(rhs))
      case Long_^   => long(asLong(lhs) ^ asLong(rhs))
      case Long_<<  => long(asLong(lhs) << asInt(rhs))
      case Long_>>> => long(asLong(lhs) >>> asInt(rhs))
      case Long_>>  => long(asLong(lhs) >> asInt(rhs))

      case Int_unsigned_/ => int(Integer.divideUnsigned(asInt(lhs), asInt(rhs)))
      case Int_unsigned_% => int(Integer.remainderUnsigned(asInt(lhs), asInt(rhs)))

      case Long_unsigned_/ => long(JLong.divideUnsigned(asLong(lhs), asLong(rhs)))
      case Long_unsigned_% => long(JLong.remainderUnsigned(asLong(lhs), asLong(rhs)))

      case _ =>
        throw new UnsupportedOperationException(
            s"Unsupported binary op in the ElementaryInterpreter\n" +
            s"${BinaryOp(op, lhs, rhs)(NoPosition).show}")
    }
  }

  private def int(value: Int): IntLiteral = IntLiteral(value)(NoPosition)

  private def asInt(value: Literal): Int = {
    val IntLiteral(intValue) = value: @unchecked
    intValue
  }

  private def long(value: Long): LongLiteral = LongLiteral(value)(NoPosition)

  private def asLong(value: Literal): Long = {
    val LongLiteral(longValue) = value: @unchecked
    longValue
  }

  private class Env(locals: Map[LocalName, Literal]) {
    def local(name: LocalName): Literal =
      locals(name)

    def withLocal(name: LocalName, value: Literal): Env =
      new Env(locals + (name -> value))
  }

  private object Env {
    def fromArgs(args: Map[LocalName, Literal]): Env =
      new Env(args)
  }
}
