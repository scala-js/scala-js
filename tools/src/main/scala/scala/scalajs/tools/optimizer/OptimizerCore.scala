/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.optimizer

import scala.annotation.{switch, tailrec}

import scala.collection.mutable

import scala.scalajs.ir._
import Definitions.{isConstructorName, isReflProxyName}
import Infos.OptimizerHints
import Trees._
import Types._

import scala.scalajs.tools.logging._

/** Optimizer core.
 *  Designed to be "mixed in" [[IncOptimizer#MethodImpl#Optimizer]].
 *  This is the core of the optimizer. It contains all the smart things the
 *  optimizer does. To perform inlining, it relies on abstract protected
 *  methods to identify the target of calls.
 */
abstract class OptimizerCore extends Transformers.Transformer {
  import OptimizerCore._

  /** Returns the list of possible targets for a dynamically linked call. */
  protected def dynamicCall(intfName: String,
      methodName: String): List[MethodImpl]

  /** Returns the target of a static call. */
  protected def staticCall(className: String,
      methodName: String): Option[MethodImpl]

  /** Returns the target of a trait impl call. */
  protected def traitImplCall(traitImplName: String,
      methodName: String): Option[MethodImpl]

  private var env: OptEnv = OptEnv.Empty
  private val usedLocalNames = mutable.Set.empty[String]
  private val usedLabelNames = mutable.Set.empty[String]
  private var statesInUse: List[State[_]] = Nil

  def optimize(originalDef: MethodDef): (MethodDef, Infos.MethodInfo) = {
    val MethodDef(name, params, resultType, body) = originalDef
    val (newParams, newBody) =
      transformIsolatedBody(params, resultType, body)
    val m = MethodDef(name, newParams, resultType, newBody)(originalDef.pos)
    val info = recreateInfo(m)
    (m, info)
  }

  private def withEnv[A](env: OptEnv)(body: => A): A = {
    val oldEnv = this.env
    this.env = env
    try body
    finally this.env = oldEnv
  }

  private def withState[A, B](state: State[A])(body: => B): B = {
    statesInUse ::= state
    try body
    finally statesInUse = statesInUse.tail
  }

  private def freshLocalName(base: String): String =
    freshNameGeneric(usedLocalNames, base)

  private def freshLabelName(base: String): String =
    freshNameGeneric(usedLabelNames, base)

  private val isReserved = isKeyword ++ Seq("arguments", "eval", "ScalaJS")

  private def freshNameGeneric(usedNames: mutable.Set[String], base: String): String = {
    val result = if (!usedNames.contains(base) && !isReserved(base)) {
      base
    } else {
      var i = 1
      while (usedNames.contains(base + "$" + i))
        i += 1
      base + "$" + i
    }
    usedNames += result
    result
  }

  private def tryOrRollback[A](body: CancelFun => A): Option[A] = {
    val savedUsedLocalNames = usedLocalNames.clone()
    val savedUsedLabelNames = usedLabelNames.clone()
    val savedStates = statesInUse.map(_.makeBackup())
    val breaks = new scala.util.control.Breaks
    breaks.tryBreakable[Option[A]] {
      Some(body(() => breaks.break()))
    } catchBreak {
      usedLocalNames.clear()
      usedLocalNames ++= savedUsedLocalNames
      usedLabelNames.clear()
      usedLabelNames ++= savedUsedLabelNames
      for ((state, backup) <- statesInUse zip savedStates)
        state.asInstanceOf[State[Any]].restore(backup)
      None
    }
  }

  private def superTransform(tree: Tree, isStat: Boolean): Tree =
    if (isStat) super.transformStat(tree)
    else super.transformExpr(tree)

  private def retransform(oldTree: Tree, newTree: Tree, isStat: Boolean): Tree =
    if (newTree eq oldTree) superTransform(oldTree, isStat)
    else transform(newTree, isStat)

  override def transformStat(tree: Tree): Tree =
    transform(tree, isStat = true)

  override def transformExpr(tree: Tree): Tree =
    transform(tree, isStat = false)

  private def transform(tree: Tree, isStat: Boolean): Tree = {
    tree match {
      case VarDef(_, _, _, rhs) =>
        /* A local var that is last (or alone) in its block is not terribly
         * useful. Get rid of it.
         * (Non-last VarDefs in blocks are handled in transformBlock.)
         */
        transformStat(rhs)

      case VarRef(ident @ Ident(name, _), mutable) =>
        val localDef = env.localDefs.getOrElse(name,
            sys.error(s"Cannot find local def '$name' at ${tree.pos}"))
        localDef.newReplacement(tree.pos)

      case This() =>
        env.localDefs.get("this").fold {
          tree
        } { thisRep =>
          thisRep.newReplacement(tree.pos)
        }

      case tree: Block =>
        transformBlock(tree, isStat)

      case Labeled(ident @ Ident(label, _), tpe, body) =>
        val newLabel = freshLabelName(label)
        Labeled(Ident(newLabel, None)(ident.pos), tpe, {
          withEnv(env.withLabelRep(label, newLabel)) {
            transform(body, isStat)
          }
        })(tree.pos)

      case Return(expr, None) =>
        env.returnRep.fold {
          superTransform(tree, isStat)
        } { returnRep =>
          val returnLabel = Ident(returnRep(), None)(tree.pos)
          Return(transformExpr(expr), Some(returnLabel))(tree.pos)
        }

      case Return(expr, Some(labelIdent @ Ident(label, _))) =>
        val newLabelIdent = Ident(env.labelReps(label), None)(labelIdent.pos)
        Return(transformExpr(expr), Some(newLabelIdent))(tree.pos)

      case While(cond, body, None) =>
        superTransform(tree, isStat)

      case While(cond, body, Some(labelIdent @ Ident(label, _))) =>
        val newLabel = freshLabelName(label)
        While(transformExpr(cond), {
          withEnv(env.withLabelRep(label, newLabel)) {
            transformStat(body)
          }
        }, Some(Ident(newLabel, None)(labelIdent.pos)))(tree.pos)

      case Try(block, errVar, EmptyTree, finalizer) =>
        superTransform(tree, isStat)

      case Try(block, errVar @ Ident(name, originalName), handler, finalizer) =>
        val newName = freshLocalName(name)
        val newOriginalName = originalName.orElse(Some(name))
        val localDef = LocalDef(AnyType, true,
            ReplaceWithVarRef(newName, newOriginalName))
        val newBlock = transform(block, isStat)
        val newHandler = withEnv(env.withLocalDef(name, localDef)) {
          transform(handler, isStat)
        }
        val newFinalizer = transformStat(finalizer)
        Try(newBlock, Ident(newName, newOriginalName)(errVar.pos),
            newHandler, newFinalizer)(tree.tpe)(tree.pos)

      case Continue(optLabel) =>
        val newOptLabel = optLabel map { label =>
          Ident(env.labelReps(label.name), None)(label.pos)
        }
        Continue(newOptLabel)(tree.pos)

      case Closure(thisType, params, resultType, body, captures) =>
        val (newParams, newBody) =
          transformIsolatedBody(params, resultType, body)
        Closure(thisType, newParams, resultType, newBody,
            captures.map(transformExpr))(tree.pos)

      case tree: Apply =>
        transformApply(tree, isStat)

      case tree: StaticApply =>
        transformStaticApply(tree, isStat)

      case tree: TraitImplApply =>
        transformTraitImplApply(tree, isStat)

      case UnaryOp(op, arg) =>
        foldUnaryOp(op, transformExpr(arg))(tree.pos)

      case BinaryOp(op, lhs, rhs) =>
        foldBinaryOp(op, transformExpr(lhs), transformExpr(rhs))(tree.pos)

      case CallHelper(helperName, List(arg))
          if helperName.length == 2 && helperName(0) == 'u' =>
        foldUnbox(helperName(1), transformExpr(arg))

      case _ =>
        superTransform(tree, isStat)
    }
  }

  private def transformBlock(tree: Block, isStat: Boolean): Tree = {
    def transformList(stats: List[Tree]): Tree = stats match {
      case last :: Nil =>
        transform(last, isStat)

      case (vDef @ VarDef(ident @ Ident(name, originalName),
          vtpe, mutable, rhs)) :: rest =>
        newLocalDefIn(name, originalName, vtpe, mutable,
            transformExpr(rhs), vDef.pos) {
          transformList(rest)
        }

      case stat :: rest =>
        Block(transformStat(stat), transformList(rest))(stat.pos)

      case Nil => // silence the exhaustivity warning in a sensible way
        Skip()(tree.pos)
    }
    transformList(tree.stats)
  }

  private def transformApply(tree: Apply, isStat: Boolean): Tree = {
    tryInlineAnonFunction(tree, isStat).getOrElse {
      val Apply(receiver, methodIdent @ Ident(methodName, _), args) = tree

      val treceiver = transformExpr(receiver)
      val targs = args.map(transformExpr)

      def treeNotInlined =
        Apply(treceiver, methodIdent, targs)(tree.tpe)(tree.pos)

      if (isReflProxyName(methodName)) {
        // Never inline reflective proxies
        treeNotInlined
      } else {
        val ClassType(cls) = treceiver.tpe
        val impls = dynamicCall(cls, methodName)
        if (impls.isEmpty) {
          // Could happen, have to leave it as is for the TypeError
          treeNotInlined
        } else if (impls.size == 1) {
          val target = impls.head
          if (!target.inlineable) {
            treeNotInlined
          } else {
            inline(tree, Some(treceiver), targs, target, isStat)
          }
        } else {
          if (impls.forall(_.isTraitImplForwarder)) {
            val reference = impls.head
            val TraitImplApply(ClassType(traitImpl), Ident(methodName, _), _) =
              reference.originalDef.body
            if (!impls.tail.forall(_.originalDef.body match {
              case TraitImplApply(ClassType(`traitImpl`),
                  Ident(`methodName`, _), _) => true
              case _ => false
            })) {
              // Not all calling the same method in the same trait impl
              treeNotInlined
            } else {
              inline(tree, Some(treceiver), targs, reference, isStat)
            }
          } else {
            // TODO? Inline multiple non-trait-impl-forwarder with the exact same body?
            treeNotInlined
          }
        }
      }
    }
  }

  private def tryInlineAnonFunction(tree: Apply, isStat: Boolean): Option[Tree] = {
    val Apply(receiver, Ident(methodName, _), args) = tree

    receiver match {
      case VarRef(Ident(f, _), false) =>
        val localDef = env.localDefs.getOrElse(f,
            sys.error(s"Cannot find local def '$f' at ${receiver.pos}"))
        localDef.replacement match {
          case TentativeAnonFunReplacement(closure, alreadyUsed, cancelFun) =>
            if (alreadyUsed.value)
              cancelFun() // no matter the method name
            if (methodName.matches("""^apply(__O)+$""")) {
              // Generic one, the one we can inline easily
              alreadyUsed.value = true
              Some(inlineClosure(tree, closure, args.map(transformExpr), isStat))
            } else if (methodName.startsWith("apply$mc") &&
                !isReflProxyName(methodName)) {
              // A specialized one, we have to introduce the box/unbox
              alreadyUsed.value = true
              val shortName = methodName.split("__")(0)
              assert(shortName.endsWith("$sp"))
              val resultCharCode :: paramCharCodes =
                shortName.stripPrefix("apply$mc").stripSuffix("$sp").toList
              assert(isPrimitiveCharCode(resultCharCode))
              assert(paramCharCodes.length == args.size)
              assert(paramCharCodes.forall(isPrimitiveCharCode))
              val boxedArgs = for {
                (charCode, arg) <- paramCharCodes zip args.map(transformExpr)
              } yield {
                if (charCode == 'C')
                  CallHelper("bC", arg)(
                      ClassType(Definitions.BoxedCharacterClass))(arg.pos)
                else
                  arg
              }
              val isVoid = resultCharCode == 'V'
              val inlined = inlineClosure(tree, closure, boxedArgs, isVoid)
              if (isVoid) Some(inlined)
              else Some(foldUnbox(resultCharCode, inlined))
            } else {
              None
            }
          case _ =>
            None
        }
      case _ =>
        None
    }
  }

  private def transformStaticApply(tree: StaticApply, isStat: Boolean): Tree = {
    val StaticApply(receiver, clsType @ ClassType(cls),
        methodIdent @ Ident(methodName, _), args) = tree
    val treceiver = transformExpr(receiver)
    val targs = args.map(transformExpr)

    def treeNotInlined =
      StaticApply(treceiver, clsType, methodIdent, targs)(tree.tpe)(tree.pos)

    if (isReflProxyName(methodName)) {
      // Never inline reflective proxies
      treeNotInlined
    } else {
      val optTarget = staticCall(cls, methodName)
      if (optTarget.isEmpty) {
        // just in case
        treeNotInlined
      } else {
        val target = optTarget.get
        if (!target.inlineable) {
          treeNotInlined
        } else {
          inline(tree, Some(treceiver), targs, target, isStat)
        }
      }
    }
  }

  private def transformTraitImplApply(tree: TraitImplApply,
      isStat: Boolean): Tree = {
    val TraitImplApply(implType @ ClassType(impl),
        methodIdent @ Ident(methodName, _), args) = tree
    val targs = args.map(transformExpr)

    def treeNotInlined =
      TraitImplApply(implType, methodIdent, targs)(tree.tpe)(tree.pos)

    val optTarget = traitImplCall(impl, methodName)
    if (optTarget.isEmpty) {
      // just in case
      treeNotInlined
    } else {
      val target = optTarget.get
      if (!target.inlineable) {
        treeNotInlined
      } else {
        inline(tree, None, targs, target, isStat)
      }
    }
  }

  private def inline(tree: Tree, optReceiver: Option[Tree],
      args: List[Tree], target: MethodImpl, isStat: Boolean): Tree = {
    val MethodDef(_, formals, resultType, body) = target.originalDef

    body match {
      case Skip() =>
        assert(isStat, "Found Skip() in expression position")
        Block(optReceiver ++: args)(tree.pos)

      case _:Literal =>
        Block(optReceiver ++: (args :+ body))(tree.pos)

      case This() if args.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        optReceiver.get

      case Select(This(), field, mutable) if formals.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        Select(optReceiver.get, field, mutable)(body.tpe)(body.pos)

      case body @ Assign(sel @ Select(This(), field, mutable),
          rhs @ VarRef(Ident(rhsName, _), _))
          if formals.size == 1 && formals.head.name.name == rhsName =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        Assign(
            Select(optReceiver.get, field, mutable)(sel.tpe)(sel.pos),
            args.head)(body.pos)

      case _ =>
        inlineBody(tree, optReceiver, formals, resultType, body,
            args, isStat)
    }
  }

  private def inlineClosure(tree: Tree, closure: Closure,
      args: List[Tree], isStat: Boolean): Tree = {
    val Closure(thisType, formals, resultType, body, captures) = closure
    inlineBody(tree, None, formals, resultType, body,
        captures ++ args, isStat)
  }

  private def inlineBody(tree: Tree, optReceiver: Option[Tree],
      formals: List[ParamDef], resultType: Type, body: Tree,
      args: List[Tree], isStat: Boolean): Tree = {
    def transformWithArgs(formalsAndArgs: List[(ParamDef, Tree)]): Tree = {
      formalsAndArgs match {
        case (formal @ ParamDef(ident @ Ident(name, originalName),
            ptpe, mutable), arg) :: rest =>
          newLocalDefIn(name, originalName, ptpe, mutable, arg, arg.pos) {
            transformWithArgs(rest)
          }
        case Nil =>
          var returnLabel: Option[String] = None
          val getReturnLabel: () => String = { () =>
            returnLabel.getOrElse {
              val label = freshLabelName("inlinereturn")
              returnLabel = Some(label)
              label
            }
          }
          val inlinedBody = withEnv(env.withReturnRep(getReturnLabel)) {
            transform(body, isStat)
          }
          returnLabel.fold {
            inlinedBody
          } { retLabel =>
            Labeled(Ident(retLabel, None)(tree.pos),
                resultType, inlinedBody)(tree.pos)
          }
      }
    }
    optReceiver match {
      case Some(receiver) =>
        newLocalDefIn("this", None, receiver.tpe, false,
            receiver, receiver.pos) {
          transformWithArgs(formals zip args)
        }
      case None =>
        transformWithArgs(formals zip args)
    }
  }

  private def foldUnaryOp(op: UnaryOp.Code, arg: Tree)(
      implicit pos: Position): Tree = {
    import UnaryOp._
    @inline def default = UnaryOp(op, arg)
    (op: @switch) match {
      case Int_+ | Double_+ =>
        arg
      case Int_- =>
        arg match {
          case IntLiteral(v)     => IntLiteral(-v)
          case UnaryOp(Int_-, x) => x
          case _                 => default
        }
      case Int_~ =>
        arg match {
          case IntLiteral(v)     => IntLiteral(~v)
          case UnaryOp(Int_~, x) => x
          case _                 => default
        }
      case Double_- =>
        arg match {
          case IntOrDoubleLit(v) => DoubleLiteral(-v)
          case _                 => default
        }
      case Boolean_! =>
        arg match {
          case BooleanLiteral(v)     => BooleanLiteral(!v)
          case UnaryOp(Boolean_!, x) => x
          case _                     => default
        }
      case DoubleToInt =>
        arg match {
          case DoubleLiteral(v)        => IntLiteral(v.toInt)
          case _ if arg.tpe == IntType => arg
          case _                       => default
        }
      case _ =>
        default
    }
  }

  private def foldBinaryOp(op: BinaryOp.Code, lhs: Tree, rhs: Tree)(
      implicit pos: Position): Tree = {
    import BinaryOp._
    @inline def default = BinaryOp(op, lhs, rhs)
    (op: @switch) match {
      case === | !== =>
        def lit(v: Boolean) = BooleanLiteral(if (op == ===) v else !v)
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))             => lit(l == r)
          case (IntOrDoubleLit(l), IntOrDoubleLit(r))     => lit(l == r)
          case (BooleanLiteral(l), BooleanLiteral(r))     => lit(l == r)
          case (StringLiteral(l, _), StringLiteral(r, _)) => lit(l == r)
          case (Undefined(), Undefined())                 => lit(true)
          case (Null(), Null())                           => lit(true)
          case (_: Literal, _: Literal)                   => lit(false)
          case _                                          => default
        }

      case Int_+ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l + r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => rhs
          case (_, UnaryOp(UnaryOp.Int_-, x)) => foldBinaryOp(Int_-, lhs, x)
          case _                              => default
        }

      case Int_- =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l - r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => foldUnaryOp(UnaryOp.Int_-, rhs)
          case (_, UnaryOp(UnaryOp.Int_-, x)) => foldBinaryOp(Int_+, lhs, x)
          case _                              => default
        }

      case Int_* =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l * r)
          case (_, IntLiteral(1))             => lhs
          case (IntLiteral(1), _)             => rhs
          case (_, IntLiteral(-1))            => foldUnaryOp(UnaryOp.Int_-, lhs)
          case (IntLiteral(-1), _)            => foldUnaryOp(UnaryOp.Int_-, rhs)
          case _                              => default
        }

      case Int_/ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) if r != 0 => IntLiteral(l / r)
          case (_, IntLiteral(1))                       => lhs
          case (_, IntLiteral(-1))                      => foldUnaryOp(UnaryOp.Int_-, lhs)
          case _                                        => default
        }

      case Int_% =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) if r != 0 => IntLiteral(l % r)
          case _                                        => default
        }

      case Int_| =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l | r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => rhs
          case _                              => default
        }

      case Int_& =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l & r)
          case (_, IntLiteral(-1))            => lhs
          case (IntLiteral(-1), _)            => rhs
          case _                              => default
        }

      case Int_^ =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r)) => IntLiteral(l ^ r)
          case (_, IntLiteral(0))             => lhs
          case (IntLiteral(0), _)             => rhs
          case (_, IntLiteral(-1))            => foldUnaryOp(UnaryOp.Int_~, lhs)
          case (IntLiteral(-1), _)            => foldUnaryOp(UnaryOp.Int_~, rhs)
          case _                              => default
        }

      case Int_<< =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))    => IntLiteral(l << r)
          case (_, IntLiteral(x)) if x % 32 == 0 => lhs
          case _                                 => default
        }

      case Int_>>> =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))    => IntLiteral(l >>> r)
          case (_, IntLiteral(x)) if x % 32 == 0 => lhs
          case _                                 => default
        }

      case Int_>> =>
        (lhs, rhs) match {
          case (IntLiteral(l), IntLiteral(r))    => IntLiteral(l >> r)
          case (_, IntLiteral(x)) if x % 32 == 0 => lhs
          case _                                 => default
        }

      case Double_+ =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l + r)
          case (_, IntOrDoubleLit(0))                 => lhs
          case (IntOrDoubleLit(0), _)                 => rhs
          case (_, UnaryOp(UnaryOp.Double_-, x))      => foldBinaryOp(Double_-, lhs, x)
          case _                                      => default
        }

      case Double_- =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l - r)
          case (_, IntOrDoubleLit(0))                 => lhs
          case (IntOrDoubleLit(0), _)                 => foldUnaryOp(UnaryOp.Double_-, rhs)
          case (_, UnaryOp(UnaryOp.Double_-, x))      => foldBinaryOp(Double_+, lhs, x)
          case _                                      => default
        }

      case Double_* =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l * r)
          case (_, IntOrDoubleLit(1))                 => lhs
          case (IntOrDoubleLit(1), _)                 => rhs
          case (_, IntOrDoubleLit(-1))                => foldUnaryOp(UnaryOp.Double_-, lhs)
          case (IntOrDoubleLit(-1), _)                => foldUnaryOp(UnaryOp.Double_-, rhs)
          case _                                      => default
        }

      case Double_/ =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l / r)
          case (_, IntOrDoubleLit(1))                 => lhs
          case (_, IntOrDoubleLit(-1))                => foldUnaryOp(UnaryOp.Double_-, lhs)
          case _                                      => default
        }

      case Double_% =>
        (lhs, rhs) match {
          case (IntOrDoubleLit(l), IntOrDoubleLit(r)) => DoubleLiteral(l % r)
          case _                                      => default
        }

      case Boolean_| =>
        (lhs, rhs) match {
          case (BooleanLiteral(l), BooleanLiteral(r)) => BooleanLiteral(l | r)
          case (_, BooleanLiteral(false))             => lhs
          case (BooleanLiteral(false), _)             => rhs
          case _                                      => default
        }

      case Boolean_& =>
        (lhs, rhs) match {
          case (BooleanLiteral(l), BooleanLiteral(r)) => BooleanLiteral(l & r)
          case (_, BooleanLiteral(true))              => lhs
          case (BooleanLiteral(true), _)              => rhs
          case _                                      => default
        }

      case Boolean_^ =>
        (lhs, rhs) match {
          case (BooleanLiteral(l), BooleanLiteral(r)) => BooleanLiteral(l ^ r)
          case (_, BooleanLiteral(false))             => lhs
          case (BooleanLiteral(false), _)             => rhs
          case (_, BooleanLiteral(true))              => foldUnaryOp(UnaryOp.Boolean_!, lhs)
          case (BooleanLiteral(true), _)              => foldUnaryOp(UnaryOp.Boolean_!, rhs)
          case _                                      => default
        }

      case Boolean_|| =>
        (lhs, rhs) match {
          case (BooleanLiteral(true), _)  => lhs
          case (BooleanLiteral(false), _) => rhs
          case (_, BooleanLiteral(false)) => lhs
          case _                          => default
        }

      case Boolean_&& =>
        (lhs, rhs) match {
          case (BooleanLiteral(false), _) => lhs
          case (BooleanLiteral(true), _)  => rhs
          case (_, BooleanLiteral(true))  => lhs
          case _                          => default
        }

      case _ =>
        default
    }
  }

  private def foldUnbox(charCode: Char, arg: Tree): Tree = {
    (charCode: @switch) match {
      case 'Z' if arg.tpe == BooleanType => arg
      case 'I' if arg.tpe == IntType     => arg
      case 'F' | 'D' if arg.tpe == DoubleType || arg.tpe == IntType => arg
      case _ =>
        CallHelper("u"+charCode, arg)((charCode: @switch) match {
          case 'Z'                   => BooleanType
          case 'C' | 'B' | 'S' | 'I' => IntType
          case 'J'                   => ClassType(Definitions.RuntimeLongClass)
          case 'F' | 'D'             => DoubleType
        })(arg.pos)
    }
  }

  def transformIsolatedBody(params: List[ParamDef], resultType: Type,
      body: Tree): (List[ParamDef], Tree) = {
    val (paramLocalDefs, newParamDefs) = (for {
      p @ ParamDef(ident @ Ident(name, originalName), ptpe, mutable) <- params
    } yield {
      val newName = freshLocalName(name)
      val newOriginalName = originalName.orElse(Some(newName))
      val localDef = LocalDef(ptpe, mutable,
          ReplaceWithVarRef(newName, newOriginalName))
      val newParamDef = ParamDef(
          Ident(newName, newOriginalName)(ident.pos), ptpe, mutable)(p.pos)
      ((name -> localDef), newParamDef)
    }).unzip

    val newBody = withEnv(OptEnv.Empty.withLocalDefs(paramLocalDefs)) {
      transform(body, resultType == NoType)
    }

    (newParamDefs, newBody)
  }

  private def newLocalDefIn(name: String,
      originalName: Option[String], declaredType: Type,
      mutable: Boolean, transformedRhs: Tree, pos: Position)(
      body: => Tree): Tree = {

    def bodyWithLocalDef(oldName: String, localDef: LocalDef) = {
      withEnv(env.withLocalDef(name, localDef)) {
        body
      }
    }

    def withDedicatedVar(tpe: Type) = {
      val newName = freshLocalName(name)
      val newOriginalName = originalName.orElse(Some(name))
      val localDef = LocalDef(tpe, mutable,
          ReplaceWithVarRef(newName, newOriginalName))
      val varDef =
        VarDef(Ident(newName, newOriginalName)(pos),
            tpe, mutable, transformedRhs)(pos)
      Block(varDef, bodyWithLocalDef(name, localDef))(pos)
    }

    if (mutable) {
      withDedicatedVar(declaredType)
    } else {
      val refinedType = transformedRhs.tpe
      transformedRhs match {
        case literal: Literal =>
          bodyWithLocalDef(name, LocalDef(refinedType, false,
              ReplaceWithConstant(literal)))

        case VarRef(Ident(refName, refOriginalName), false) =>
          bodyWithLocalDef(name, LocalDef(refinedType, false,
              ReplaceWithVarRef(refName, refOriginalName)))

        case New(ClassType(wrapperName), _, List(closure: Closure))
            if wrapperName.startsWith(AnonFunctionClassPrefix) =>
          tryOrRollback { cancelFun =>
            val alreadyUsedState = new SimpleState[Boolean](false)
            withState(alreadyUsedState) {
              bodyWithLocalDef(name, LocalDef(refinedType, false,
                  TentativeAnonFunReplacement(closure, alreadyUsedState, cancelFun)))
            }
          } getOrElse {
            withDedicatedVar(refinedType)
          }

        case _ =>
          withDedicatedVar(refinedType)
      }
    }
  }

}

object OptimizerCore {

  private val AnonFunctionClassPrefix = "sjsr_AnonFunction"

  private val isPrimitiveCharCode =
    Set('V', 'Z', 'C', 'B', 'S', 'I', 'J', 'F', 'D')

  private type CancelFun = () => Nothing

  private case class LocalDef(
      tpe: Type,
      mutable: Boolean,
      replacement: LocalDefReplacement) {

    def newReplacement(implicit pos: Position): Tree = replacement match {
      case ReplaceWithVarRef(name, originalName) =>
        VarRef(Ident(name, originalName), mutable)(tpe)

      case ReplaceWithConstant(value) =>
        value

      case TentativeAnonFunReplacement(closure, alreadyUsed, cancelFun) =>
        cancelFun()
    }
  }

  private sealed abstract class LocalDefReplacement

  private final case class ReplaceWithVarRef(name: String,
      originalName: Option[String]) extends LocalDefReplacement

  private final case class ReplaceWithConstant(
      value: Tree) extends LocalDefReplacement

  private final case class TentativeAnonFunReplacement(
      closure: Closure,
      alreadyUsed: SimpleState[Boolean],
      cancelFun: CancelFun) extends LocalDefReplacement

  private class OptEnv(
      val localDefs: Map[String, LocalDef],
      val returnRep: Option[() => String],
      val labelReps: Map[String, String]) {

    def withLocalDef(oldName: String, rep: LocalDef): OptEnv =
      new OptEnv(localDefs + (oldName -> rep), returnRep, labelReps)

    def withLocalDefs(reps: List[(String, LocalDef)]): OptEnv =
      new OptEnv(localDefs ++ reps, returnRep, labelReps)

    def withReturnRep(rep: () => String): OptEnv =
      new OptEnv(localDefs, Some(rep), labelReps)

    def withLabelRep(oldName: String, newName: String): OptEnv =
      new OptEnv(localDefs, returnRep, labelReps + (oldName -> newName))

    def withinFunction(paramLocalDefs: List[(String, LocalDef)]): OptEnv =
      new OptEnv(localDefs ++ paramLocalDefs, None, Map.empty)
  }

  private object OptEnv {
    val Empty: OptEnv = new OptEnv(Map.empty, None, Map.empty)
  }

  private object IntOrDoubleLit {
    def unapply(tree: Literal): Option[Double] = tree match {
      case DoubleLiteral(v) => Some(v)
      case IntLiteral(v)    => Some(v.toDouble)
      case _                => None
    }
  }

  private trait State[A] {
    def makeBackup(): A
    def restore(backup: A): Unit
  }

  private class SimpleState[A](var value: A) extends State[A] {
    def makeBackup(): A = value
    def restore(backup: A): Unit = value = backup
  }

  abstract class MethodImpl {
    def encodedName: String
    def optimizerHints: OptimizerHints
    def originalDef: MethodDef

    var inlineable: Boolean = false
    var isTraitImplForwarder: Boolean = false

    protected def updateInlineable(): Unit = {
      val MethodDef(_, params, _, body) = originalDef

      isTraitImplForwarder = body match {
        // Shape of forwarders to trait impls
        case TraitImplApply(impl, method, args) =>
          ((args.size == params.size + 1) &&
              (args.head.isInstanceOf[This]) &&
              (args.tail.zip(params).forall {
                case (VarRef(Ident(aname, _), _),
                    ParamDef(Ident(pname, _), _, _)) => aname == pname
                case _ => false
              }))

        case _ => false
      }

      inlineable = optimizerHints.hasInlineAnnot || isTraitImplForwarder || {
        val MethodDef(_, params, _, body) = originalDef
        body match {
          case _:Skip | _:This | _:Literal                          => true

          // Shape of accessors
          case Select(This(), _, _) if params.isEmpty               => true
          case Assign(Select(This(), _, _), VarRef(_, _))
              if params.size == 1                                   => true

          // Shape of trivial call-super constructors
          case Block(List(StaticApply(This(), _, _, Nil), This()))
              if params.isEmpty && isConstructorName(encodedName)   => true

          case _ => false
        }
      }
    }
  }

  /** Recreates precise [[Infos.MethodInfo]] from the optimized [[MethodDef]]. */
  private def recreateInfo(methodDef: MethodDef): Infos.MethodInfo = {
    new RecreateInfoTraverser().recreateInfo(methodDef)
  }

  private final class RecreateInfoTraverser extends Traversers.Traverser {
    import RecreateInfoTraverser._

    private val calledMethods = mutable.Map.empty[String, mutable.Set[String]]
    private val calledMethodsStatic = mutable.Map.empty[String, mutable.Set[String]]
    private val instantiatedClasses = mutable.Set.empty[String]
    private val accessedModules = mutable.Set.empty[String]
    private val accessedClassData = mutable.Set.empty[String]

    def recreateInfo(methodDef: MethodDef): Infos.MethodInfo = {
      traverse(methodDef.body)
      Infos.MethodInfo(
          encodedName = methodDef.name.name,
          calledMethods = calledMethods.toMap.mapValues(_.toList),
          calledMethodsStatic = calledMethodsStatic.toMap.mapValues(_.toList),
          instantiatedClasses = instantiatedClasses.toList,
          accessedModules = accessedModules.toList,
          accessedClassData = accessedClassData.toList)
    }

    private def addCalledMethod(container: String, methodName: String): Unit =
      calledMethods.getOrElseUpdate(container, mutable.Set.empty) += methodName

    private def addCalledMethodStatic(container: String, methodName: String): Unit =
      calledMethodsStatic.getOrElseUpdate(container, mutable.Set.empty) += methodName

    private def typeToContainer(tpe: Type): String = tpe match {
      case ClassType(cls) => cls
      case _              => Definitions.ObjectClass
    }

    private def refTypeToClassData(tpe: ReferenceType): String = tpe match {
      case ClassType(cls)     => cls
      case ArrayType(base, _) => base
    }

    def addAccessedClassData(encodedName: String): Unit = {
      if (!AlwaysPresentClassData.contains(encodedName))
        accessedClassData += encodedName
    }

    def addAccessedClassData(tpe: ReferenceType): Unit =
      addAccessedClassData(refTypeToClassData(tpe))

    override def traverse(tree: Tree): Unit = {
      tree match {
        case New(ClassType(cls), ctor, _) =>
          instantiatedClasses += cls
          addCalledMethodStatic(cls, ctor.name)

        case Apply(receiver, method, _) =>
          addCalledMethod(typeToContainer(receiver.tpe), method.name)
        case StaticApply(_, ClassType(cls), method, _) =>
          addCalledMethodStatic(cls, method.name)
        case TraitImplApply(ClassType(impl), method, _) =>
          addCalledMethodStatic(impl, method.name)

        case LoadModule(ClassType(cls)) =>
          accessedModules += cls.stripSuffix("$")

        case NewArray(tpe, _) =>
          addAccessedClassData(tpe)
        case ArrayValue(tpe, _) =>
          addAccessedClassData(tpe)
        case IsInstanceOf(_, cls) =>
          addAccessedClassData(cls)
        case AsInstanceOf(_, cls) =>
          addAccessedClassData(cls)
        case ClassOf(cls) =>
          addAccessedClassData(cls)

        case CallHelper(helper, _) =>
          HelperTargets.get(helper) foreach {
            case (cls, method) => addCalledMethod(cls, method)
          }

        case _ =>
      }
      super.traverse(tree)
    }

    private val HelperTargets = {
      import Definitions._
      Map[String, (String, String)](
        "objectToString"  -> (ObjectClass, "toString__T"),
        "objectGetClass"  -> (ObjectClass, s"getClass__$ClassClass"),
        "objectClone"     -> (ObjectClass, "clone__O"),
        "objectFinalize"  -> (ObjectClass, "finalize__V"),
        "objectNotify"    -> (ObjectClass, "notify__V"),
        "objectNotifyAll" -> (ObjectClass, "notifyAll__V"),
        "objectEquals"    -> (ObjectClass, "equals__O__Z"),
        "objectHashCode"  -> (ObjectClass, "hashCode__I"),

        "charSequenceLength"      -> (CharSequenceClass, "length__I"),
        "charSequenceCharAt"      -> (CharSequenceClass, "charAt__I__C"),
        "charSequenceSubSequence" -> (CharSequenceClass, s"subSequence__I__I__$CharSequenceClass"),

        "comparableCompareTo" -> (ComparableClass, "compareTo__O__I"),

        "numberByteValue"   -> (NumberClass, "byteValue__B"),
        "numberShortValue"  -> (NumberClass, "shortValue__S"),
        "numberIntValue"    -> (NumberClass, "intValue__I"),
        "numberLongValue"   -> (NumberClass, "longValue__J"),
        "numberFloatValue"  -> (NumberClass, "floatValue__F"),
        "numberDoubleValue" -> (NumberClass, "doubleValue__D")
      )
    }
  }

  private object RecreateInfoTraverser {
    /** Class data that are never eliminated by dce, so we don't need to
     *  record them.
     */
    private val AlwaysPresentClassData = {
      import Definitions._
      Set("V", "Z", "C", "B", "S", "I", "J", "F", "D",
          ObjectClass, StringClass, RuntimeLongClass)
    }
  }

}
