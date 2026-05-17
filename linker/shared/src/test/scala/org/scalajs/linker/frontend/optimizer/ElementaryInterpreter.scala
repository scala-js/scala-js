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
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.backend.emitter.{LongImpl, PrivateLibHolder}

/** An IR interpreter for elementary operations, which we use for fuzzing. */
object ElementaryInterpreter {
  private val divideUnsignedMethodName =
    MethodName("divideUnsigned", List(IntRef, IntRef), IntRef)

  private val remainderUnsignedMethodName =
    MethodName("remainderUnsigned", List(IntRef, IntRef), IntRef)

  private val uintToDoubleMethodName =
    MethodName("uintToDouble", List(IntRef), DoubleRef)

  private val rawToIntMethodName =
    MethodName("rawToInt", List(DoubleRef), IntRef)

  def eval(tree: Tree, args: (LocalName, Literal)*): Literal =
    eval(tree)(Env.fromArgs(args.toMap))

  private lazy val RuntimeClassDefs: Map[ClassName, ClassDef] = {
    PrivateLibHolder.directClassDefs.map { case (path, classDef) =>
      classDef.className -> classDef
    }.toMap
  }

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

    case If(cond, thenp, elsep) =>
      if (asBool(eval(cond)))
        eval(thenp)
      else
        eval(elsep)

    case UnaryOp(op, arg) =>
      evalUnaryOp(op, eval(arg))

    case BinaryOp(op, lhs, rhs) =>
      evalBinaryOp(op, eval(lhs), eval(rhs))

    case ApplyStatic(flags, cls, MethodIdent(methodName), args) =>
      val evaledArgs = args.map(eval(_))
      findAndEvalClassMethod(tree, cls, MemberNamespace.forStaticCall(flags),
          methodName, None, evaledArgs)

    case Apply(flags, receiver, MethodIdent(methodName), args) =>
      val evalReceiver = eval(receiver)
      val cls = evalReceiver match {
        case ClassOf(ClassRef(cls)) =>
          cls
        case _ =>
          throw new UnsupportedOperationException(s"Unknown receiver $evalReceiver:\n${tree.show}")
      }
      val evaledArgs = args.map(eval(_))
      findAndEvalClassMethod(tree, cls, MemberNamespace.forNonStaticCall(flags),
          methodName, Some(evalReceiver), evaledArgs)

    case LoadModule(cls) =>
      // Hack: use a classOf literal to remember the class of the module value
      ClassOf(ClassRef(cls))(NoPosition)

    case AsInstanceOf(expr, tpe) =>
      // Assume the cast succeeds; something should crash later if it's not the case
      eval(expr)

    case _ =>
      throw new UnsupportedOperationException(
          s"Unsupported tree in the ElementaryInterpreter, of class " +
          s"${tree.getClass().getSimpleName()}:\n${tree.show}")
  }

  private def findAndEvalClassMethod(ctx: Tree, cls: ClassName,
      namespace: MemberNamespace, methodName: MethodName,
      optReceiver: Option[Literal], args: List[Literal]): Literal = {

    (cls, methodName) match {
      case (BoxedIntegerClass, `divideUnsignedMethodName`) =>
        val List(num, divisor) = args: @unchecked
        int(Integer.divideUnsigned(asInt(num), asInt(divisor)))

      case (BoxedIntegerClass, `remainderUnsignedMethodName`) =>
        val List(num, divisor) = args: @unchecked
        int(Integer.remainderUnsigned(asInt(num), asInt(divisor)))

      case (LongImpl.RuntimeLongModClass, LongImpl.pack) =>
        val List(lo, hi) = args: @unchecked
        long(Integer.toUnsignedLong(asInt(lo)) | (asInt(hi).toLong << 32))

      case (LongImpl.RuntimeLongModClass, `uintToDoubleMethodName`) =>
        val List(arg) = args: @unchecked
        double(Integer.toUnsignedLong(asInt(arg)).toDouble)

      case (LongImpl.RuntimeLongModClass, `rawToIntMethodName`) =>
        val List(arg) = args: @unchecked
        int(jsToInt32(asDouble(arg)))

      case _ =>
        val classDef = RuntimeClassDefs.getOrElse(cls, {
          throw new UnsupportedOperationException(
              s"Unknown runtime class ${cls.nameString}:\n${ctx.show}")
        })
        val methodDef = classDef.methods.find { m =>
          m.flags.namespace == namespace && m.methodName == methodName
        }.getOrElse {
          throw new UnsupportedOperationException(
              s"Unknown method ${methodName.nameString} in ${cls.nameString}:\n${ctx.show}")
        }
        val bodyEnv0 = Env.fromArgs(methodDef.args.map(_.name.name).zip(args).toMap)
        val bodyEnv = optReceiver.fold(bodyEnv0)(r => bodyEnv0.withLocal(LocalName.This, r))
        eval(methodDef.body.get)(bodyEnv)
    }
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
      case IntToDouble       => double(asInt(arg).toDouble)
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

      case Double_+ => double(asDouble(lhs) + asDouble(rhs))
      case Double_- => double(asDouble(lhs) - asDouble(rhs))
      case Double_* => double(asDouble(lhs) * asDouble(rhs))
      case Double_/ => double(asDouble(lhs) / asDouble(rhs))
      case Double_% => double(asDouble(lhs) % asDouble(rhs))

      case Int_<  => bool(asInt(lhs) < asInt(rhs))
      case Long_< => bool(asLong(lhs) < asLong(rhs))

      case Int_unsigned_<  => bool(Integer.compareUnsigned(asInt(lhs), asInt(rhs)) < 0)
      case Long_unsigned_< => bool(JLong.compareUnsigned(asLong(lhs), asLong(rhs)) < 0)

      case _ =>
        throw new UnsupportedOperationException(
            s"Unsupported binary op in the ElementaryInterpreter\n" +
            s"${BinaryOp(op, lhs, rhs)(NoPosition).show}")
    }
  }

  /** Performs the JS operation `x | 0` and returns the result as an Int. */
  private def jsToInt32(x: Double): Int = {
    if (x < 0.0) {
      -jsToInt32(-x)
    } else {
      (Math.floor(x) % ((1L << 32).toDouble)).toLong.toInt
    }
  }

  private def bool(value: Boolean): BooleanLiteral = BooleanLiteral(value)(NoPosition)

  private def asBool(value: Literal): Boolean = {
    val BooleanLiteral(boolValue) = value: @unchecked
    boolValue
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

  private def double(value: Double): DoubleLiteral = DoubleLiteral(value)(NoPosition)

  private def asDouble(value: Literal): Double = {
    val DoubleLiteral(doubleValue) = value: @unchecked
    doubleValue
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
