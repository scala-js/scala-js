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
abstract class OptimizerCore(
    myself: OptimizerCore.MethodImpl) extends Transformers.Transformer {
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

  /** Tests whether the given module class has an elidable accessor.
   *  In other words, whether it is safe to discard a LoadModule of that
   *  module class which is not used.
   */
  protected def hasElidableModuleAccessor(moduleClassName: String): Boolean

  private var implsBeingInlined: Set[MethodImpl] = Set.empty
  private var env: OptEnv = OptEnv.Empty
  private val usedLocalNames = mutable.Set.empty[String]
  private val usedLabelNames = mutable.Set.empty[String]
  private var statesInUse: List[State[_]] = Nil

  def optimize(originalDef: MethodDef): (MethodDef, Infos.MethodInfo) = {
    val MethodDef(name, params, resultType, body) = originalDef
    val thisType = myself.thisType
    val (newParams, newBody) = inlining(myself) {
      transformIsolatedBody(thisType, params, resultType, body)
    }
    val m = MethodDef(name, newParams, resultType, newBody)(originalDef.pos)
    val info = recreateInfo(m)
    (m, info)
  }

  private def inlining[A](impl: MethodImpl)(body: => A): A = {
    assert(!implsBeingInlined(impl), s"Circular inlining of $impl")
    val saved = implsBeingInlined
    implsBeingInlined += impl
    try body
    finally implsBeingInlined = saved
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

  /** Transforms a statement.
   *
   *  It is always the case that
   *  {{{
   *  transformStat(tree)
   *  ===
   *  finishTransformStat(pretransform(tree))
   *  }}}
   */
  override def transformStat(tree: Tree): Tree =
    transform(tree, isStat = true)

  /** Transforms an expression.
   *
   *  It is always the case that
   *  {{{
   *  transformExpr(tree)
   *  ===
   *  finishTransformExpr(pretransform(tree))
   *  }}}
   */
  override def transformExpr(tree: Tree): Tree =
    transform(tree, isStat = false)

  /** Transforms a tree. */
  private def transform(tree: Tree, isStat: Boolean): Tree = {
    def superTransform(tree: Tree, isStat: Boolean): Tree =
      if (isStat) super.transformStat(tree)
      else        super.transformExpr(tree)

    @inline implicit def pos = tree.pos
    val result = tree match {
      case VarDef(_, _, _, rhs) =>
        /* A local var that is last (or alone) in its block is not terribly
         * useful. Get rid of it.
         * (Non-last VarDefs in blocks are handled in transformBlock.)
         */
        transformStat(rhs)

      case _:VarRef | _:This =>
        val preTrans = pretransform(tree)
        if (isStat) finishTransformStat(preTrans)
        else        finishTransformExpr(preTrans)

      case tree: Block =>
        transformBlock(tree, isStat)

      case Labeled(ident @ Ident(label, _), tpe, body) =>
        returnable(label, if (isStat) NoType else tpe, body)

      case Return(expr, None) =>
        env.labelInfos.get("").fold {
          superTransform(tree, isStat)
        } { info =>
          val newExpr = transformExpr(expr)
          info.returnedTypes.value ::= newExpr.tpe
          Return(newExpr, Some(Ident(info.newName, None)))
        }

      case Return(expr, Some(labelIdent @ Ident(label, _))) =>
        val info = env.labelInfos(label)
        val newExpr = transformExpr(expr)
        info.returnedTypes.value ::= newExpr.tpe
        Return(newExpr, Some(Ident(info.newName, None)))

      case If(cond, thenp, elsep) =>
        val newCond = transformExpr(cond)
        newCond match {
          case BooleanLiteral(condValue) =>
            if (condValue) transform(thenp, isStat)
            else           transform(elsep, isStat)
          case _ =>
            val newThenp = transform(thenp, isStat)
            val newElsep = transform(elsep, isStat)
            val refinedType =
              constrainedLub(newThenp.tpe, newElsep.tpe, tree.tpe)
            foldIf(newCond, newThenp, newElsep)(refinedType)
        }

      case While(cond, body, None) =>
        superTransform(tree, isStat)

      case While(cond, body, Some(labelIdent @ Ident(label, _))) =>
        val newLabel = freshLabelName(label)
        val info = new LabelInfo(newLabel)
        While(transformExpr(cond), {
          withEnv(env.withLabelInfo(label, info)) {
            transformStat(body)
          }
        }, Some(Ident(newLabel, None)(labelIdent.pos)))

      case Try(block, errVar, EmptyTree, finalizer) =>
        superTransform(tree, isStat)

      case Try(block, errVar @ Ident(name, originalName), handler, finalizer) =>
        val newName = freshLocalName(name)
        val newOriginalName = originalName.orElse(Some(name))
        val localDef = LocalDef(AnyType, true,
            ReplaceWithVarRef(newName, newOriginalName, new SimpleState(true)))
        val newBlock = transform(block, isStat)
        val newHandler = withEnv(env.withLocalDef(name, localDef)) {
          transform(handler, isStat)
        }
        val newFinalizer = transformStat(finalizer)
        Try(newBlock, Ident(newName, newOriginalName)(errVar.pos),
            newHandler, newFinalizer)(tree.tpe)

      case Continue(optLabel) =>
        val newOptLabel = optLabel map { label =>
          Ident(env.labelInfos(label.name).newName, None)(label.pos)
        }
        Continue(newOptLabel)

      case Closure(thisType, params, resultType, body, captures) =>
        val (newParams, newBody) =
          transformIsolatedBody(thisType, params, resultType, body)
        Closure(thisType, newParams, resultType, newBody,
            captures.map(transformExpr))

      case tree: Apply =>
        transformApply(tree, isStat)

      case tree: StaticApply =>
        transformStaticApply(tree, isStat)

      case tree: TraitImplApply =>
        transformTraitImplApply(tree, isStat)

      case UnaryOp(op, arg) =>
        foldUnaryOp(op, transformExpr(arg))(tree.pos)

      case BinaryOp(op, lhs, rhs) =>
        foldBinaryOp(op, transformExpr(lhs), transformExpr(rhs))

      case CallHelper(helperName, List(arg))
          if helperName.length == 2 && helperName(0) == 'u' =>
        foldUnbox(helperName(1), transformExpr(arg))

      case _ =>
        superTransform(tree, isStat)
    }

    if (isStat) keepOnlySideEffects(result)
    else result
  }

  private def transformBlock(tree: Block, isStat: Boolean): Tree = {
    def transformList(stats: List[Tree]): Tree = stats match {
      case last :: Nil =>
        transform(last, isStat)

      case (vDef @ VarDef(ident @ Ident(name, originalName),
          vtpe, mutable, rhs)) :: rest =>
        withBinding(Binding(name, originalName, vtpe, mutable,
            pretransform(rhs))) {
          transformList(rest)
        }

      case stat :: rest =>
        Block(transformStat(stat), transformList(rest))(stat.pos)

      case Nil => // silence the exhaustivity warning in a sensible way
        Skip()(tree.pos)
    }
    transformList(tree.stats)
  }

  /** Pretransforms a tree to get a refined type while avoiding to force
   *  things we might be able to optimize by folding and aliasing.
   */
  private def pretransform(tree: Tree): PreTransform = tree match {
    case VarRef(Ident(name, _), _) =>
      implicit val pos = tree.pos
      val localDef = env.localDefs.getOrElse(name,
          sys.error(s"Cannot find local def '$name' at $pos"))
      PreTransLocalDef(localDef)

    case This() =>
      implicit val pos = tree.pos
      val localDef = env.localDefs.getOrElse("this",
          sys.error(s"Found invalid 'this' at $pos"))
      PreTransLocalDef(localDef)

    case New(cls, ctor, args) =>
      PreTransNew(cls, ctor, args.map(pretransform))(tree.pos)

    case Closure(thisType, params, resultType, body, captures) =>
      PreTransClosure(thisType, params, resultType, body,
          captures.map(pretransform))(tree.pos)

    case _ =>
      PreTransTree(transformExpr(tree))
  }

  /** Finishes an expression pretransform to get a normal [[Tree]].
   *  This method (together with finishTransformStat) must not be called more
   *  than once per pretransform and per translation.
   *  By "per translation", we mean in an alternative path through
   *  `tryOrRollback`. It could still be called several times as long as
   *  it is once in the 'try' part and once in the 'fallback' part.
   */
  def finishTransformExpr(preTrans: PreTransform): Tree = {
    implicit val pos = preTrans.pos
    preTrans match {
      case PreTransLocalDef(localDef) =>
        localDef.newReplacement
      case PreTransNew(cls, ctor, args) =>
        New(cls, ctor, args.map(finishTransformExpr))
      case PreTransClosure(thisType, params, resultType, body, captures) =>
        val (newParams, newBody) =
          transformIsolatedBody(thisType, params, resultType, body)
        Closure(thisType, newParams, newBody.tpe, newBody,
            captures.map(finishTransformExpr))
      case PreTransTree(tree) =>
        tree
    }
  }

  /** Finishes a statement pretransform to get a normal [[Tree]].
   *  This method (together with finishTransformExpr) must not be called more
   *  than once per pretransform and per translation.
   *  By "per translation", we mean in an alternative path through
   *  `tryOrRollback`. It could still be called several times as long as
   *  it is once in the 'try' part and once in the 'fallback' part.
   */
  private def finishTransformStat(stat: PreTransform): Tree = stat match {
    case PreTransLocalDef(_) =>
      Skip()(stat.pos)
    case PreTransNew(_, _, _) =>
      finishTransformExpr(stat)
    case PreTransClosure(_, _, _, _, captures) =>
      Block(captures.map(finishTransformStat))(stat.pos)
    case PreTransTree(tree) =>
      keepOnlySideEffects(tree)
  }

  /** Keeps only the side effects of a Tree (overapproximation). */
  private def keepOnlySideEffects(stat: Tree): Tree = stat match {
    case _:VarRef | _:This | _:Literal =>
      Skip()(stat.pos)
    case Block(init :+ last) =>
      Block(init :+ keepOnlySideEffects(last))(stat.pos)
    case LoadModule(ClassType(moduleClassName)) =>
      if (hasElidableModuleAccessor(moduleClassName)) Skip()(stat.pos)
      else stat
    case Closure(_, _, _, _, captures) =>
      Block(captures.map(keepOnlySideEffects))(stat.pos)
    case _ =>
      stat
  }

  private def transformApply(tree: Apply, isStat: Boolean): Tree = {
    val Apply(receiver0, methodIdent @ Ident(methodName, _), args0) = tree
    implicit val pos = tree.pos

    val receiver = pretransform(receiver0)
    val args = args0.map(pretransform)

    tryInlineAnonFunction(receiver, methodName, args, isStat).getOrElse {
      def treeNotInlined =
        Apply(finishTransformExpr(receiver), methodIdent,
            args.map(finishTransformExpr))(tree.tpe)(tree.pos)

      if (isReflProxyName(methodName)) {
        // Never inline reflective proxies
        treeNotInlined
      } else {
        val ClassType(cls) = receiver.tpe
        val impls = dynamicCall(cls, methodName)
        if (impls.isEmpty || impls.exists(implsBeingInlined)) {
          // isEmpty could happen, have to leave it as is for the TypeError
          treeNotInlined
        } else if (impls.size == 1) {
          val target = impls.head
          if (!target.inlineable) {
            treeNotInlined
          } else {
            inline(Some(receiver), args, target, isStat)
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
              inline(Some(receiver), args, reference, isStat)
            }
          } else {
            // TODO? Inline multiple non-trait-impl-forwarder with the exact same body?
            treeNotInlined
          }
        }
      }
    }
  }

  private def tryInlineAnonFunction(receiver: PreTransform,
      methodName: String, args: List[PreTransform], isStat: Boolean)(
      implicit pos: Position): Option[Tree] = {
    receiver match {
      case PreTransLocalDef(localDef) if !localDef.mutable =>
        localDef.replacement match {
          case TentativeAnonFunReplacement(closure, alreadyUsed, cancelFun) =>
            if (alreadyUsed.value)
              cancelFun() // no matter the method name
            if (methodName.matches("""^apply(__O)+$""")) {
              // Generic one, the one we can inline easily
              alreadyUsed.value = true
              Some(inlineClosure(closure, args, isStat))
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
                (charCode, arg) <- paramCharCodes zip args
              } yield {
                if (charCode == 'C')
                  PreTransTree(CallHelper("bC", finishTransformExpr(arg))(
                      ClassType(Definitions.BoxedCharacterClass))(arg.pos))
                else
                  arg
              }
              val isVoid = resultCharCode == 'V'
              val inlined = inlineClosure(closure, boxedArgs, isVoid)
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
    val StaticApply(receiver0, clsType @ ClassType(cls),
        methodIdent @ Ident(methodName, _), args0) = tree
    implicit val pos = tree.pos

    val receiver = pretransform(receiver0)
    val args = args0.map(pretransform)

    def treeNotInlined =
      StaticApply(finishTransformExpr(receiver), clsType, methodIdent,
          args.map(finishTransformExpr))(tree.tpe)

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
        if (!target.inlineable || implsBeingInlined(target)) {
          treeNotInlined
        } else {
          inline(Some(receiver), args, target, isStat)
        }
      }
    }
  }

  private def transformTraitImplApply(tree: TraitImplApply,
      isStat: Boolean): Tree = {
    val TraitImplApply(implType @ ClassType(impl),
        methodIdent @ Ident(methodName, _), args0) = tree
    implicit val pos = tree.pos

    val args = args0.map(pretransform)

    def treeNotInlined =
      TraitImplApply(implType, methodIdent,
          args.map(finishTransformExpr))(tree.tpe)

    val optTarget = traitImplCall(impl, methodName)
    if (optTarget.isEmpty) {
      // just in case
      treeNotInlined
    } else {
      val target = optTarget.get
      if (!target.inlineable || implsBeingInlined(target)) {
        treeNotInlined
      } else {
        inline(None, args, target, isStat)
      }
    }
  }

  private def inline(optReceiver: Option[PreTransform],
      args: List[PreTransform], target: MethodImpl, isStat: Boolean)(
      implicit pos: Position): Tree = inlining(target) {

    assert(target.inlineable, s"Trying to inline non-inlineable method $target")

    val MethodDef(_, formals, resultType, body) = target.originalDef

    body match {
      case Skip() =>
        assert(isStat, "Found Skip() in expression position")
        Block((optReceiver ++: args).map(finishTransformStat))

      case _:Literal =>
        Block((optReceiver ++: args).map(finishTransformStat) :+ body)

      case This() if args.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        finishTransformExpr(optReceiver.get)

      case Select(This(), field, mutable) if formals.isEmpty =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        Select(finishTransformExpr(optReceiver.get),
            field, mutable)(body.tpe)(body.pos)

      case body @ Assign(sel @ Select(This(), field, mutable),
          rhs @ VarRef(Ident(rhsName, _), _))
          if formals.size == 1 && formals.head.name.name == rhsName =>
        assert(optReceiver.isDefined,
            "There was a This(), there should be a receiver")
        Assign(
            Select(finishTransformExpr(optReceiver.get),
                field, mutable)(sel.tpe)(sel.pos),
            finishTransformExpr(args.head))(body.pos)

      case _ =>
        inlineBody(optReceiver, formals, resultType, body, args, isStat)
    }
  }

  private def inlineClosure(closure: PreTransClosure, args: List[PreTransform],
      isStat: Boolean)(implicit pos: Position): Tree = {
    val PreTransClosure(thisType, formals, resultType, body, captures) = closure
    inlineBody(None, formals, resultType, body,
        captures ++ args, isStat)
  }

  private def inlineBody(optReceiver: Option[PreTransform],
      formals: List[ParamDef], resultType: Type, body: Tree,
      args: List[PreTransform], isStat: Boolean)(
      implicit pos: Position): Tree = {

    val optReceiverBinding = optReceiver map { receiver =>
      Binding("this", None, receiver.tpe, false, receiver)
    }

    val argsBindings = for {
      (ParamDef(Ident(name, originalName), tpe, mutable), arg) <- formals zip args
    } yield {
      Binding(name, originalName, tpe, mutable, arg)
    }

    withBindings(optReceiverBinding ++: argsBindings) {
      returnable("", resultType, body)
    }
  }

  private def foldIf(cond: Tree, thenp: Tree, elsep: Tree)(tpe: Type)(
      implicit pos: Position): Tree = {
    @inline def default = If(cond, thenp, elsep)(tpe)
    cond match {
      case BooleanLiteral(v) =>
        if (v) thenp
        else elsep

      case _ =>
        @inline def negCond = foldUnaryOp(UnaryOp.Boolean_!, cond)
        if (thenp.tpe == BooleanType && elsep.tpe == BooleanType) {
          (thenp, elsep) match {
            case (BooleanLiteral(true), BooleanLiteral(false)) => cond
            case (BooleanLiteral(false), BooleanLiteral(true)) => negCond

            case (BooleanLiteral(true), _) =>
              foldBinaryOp(BinaryOp.Boolean_||, cond, elsep)
            case (_, BooleanLiteral(false)) =>
              foldBinaryOp(BinaryOp.Boolean_&&, cond, thenp)

            case (BooleanLiteral(false), _) =>
              foldBinaryOp(BinaryOp.Boolean_&&, negCond, elsep)
            case (_, BooleanLiteral(true)) =>
              foldBinaryOp(BinaryOp.Boolean_||, negCond, thenp)

            case _ => default
          }
        } else {
          (thenp, elsep) match {
            case (Skip(), Skip()) => cond
            case (Skip(), _)      => foldIf(negCond, elsep, thenp)(tpe)

            case _ => default
          }
        }
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

  def transformIsolatedBody(thisType: Type, params: List[ParamDef],
      resultType: Type, body: Tree): (List[ParamDef], Tree) = {
    val (paramLocalDefs, newParamDefs) = (for {
      p @ ParamDef(ident @ Ident(name, originalName), ptpe, mutable) <- params
    } yield {
      val newName = freshLocalName(name)
      val newOriginalName = originalName.orElse(Some(newName))
      val localDef = LocalDef(ptpe, mutable,
          ReplaceWithVarRef(newName, newOriginalName, new SimpleState(true)))
      val newParamDef = ParamDef(
          Ident(newName, newOriginalName)(ident.pos), ptpe, mutable)(p.pos)
      ((name -> localDef), newParamDef)
    }).unzip

    val thisLocalDef =
      if (thisType == NoType) None
      else Some("this" -> LocalDef(thisType, false, ReplaceWithThis()))

    val allLocalDefs = thisLocalDef ++: paramLocalDefs

    val newBody = withEnv(OptEnv.Empty.withLocalDefs(allLocalDefs)) {
      transform(body, resultType == NoType)
    }

    (newParamDefs, newBody)
  }

  private def returnable(oldLabelName: String, resultType: Type,
      body: Tree)(implicit pos: Position): Tree = {
    val newLabel = freshLabelName(
        if (oldLabelName.isEmpty) "inlinereturn" else oldLabelName)
    val info = new LabelInfo(newLabel)
    withState(info.returnedTypes) {
      val newBody = withEnv(env.withLabelInfo(oldLabelName, info)) {
        transform(body, resultType == NoType)
      }
      val returnedTypes = info.returnedTypes.value
      if (returnedTypes.isEmpty) {
        // no return to that label, we can eliminate it
        newBody
      } else {
        val refinedType =
          returnedTypes.foldLeft(newBody.tpe)(constrainedLub(_, _, resultType))
        val returnCount = returnedTypes.size

        tryOptimizePatternMatch(oldLabelName, refinedType, returnCount,
            newBody) getOrElse {
          Labeled(Ident(newLabel, None), refinedType, newBody)
        }
      }
    }
  }

  def tryOptimizePatternMatch(oldLabelName: String, refinedType: Type,
      returnCount: Int, newBody: Tree): Option[Tree] = {
    if (!oldLabelName.startsWith("matchEnd")) None
    else {
      newBody match {
        case Block(stats) =>
          @tailrec
          def createRevAlts(xs: List[Tree], acc: List[(Tree, Tree)]): List[(Tree, Tree)] = xs match {
            case If(cond, body, Skip()) :: xr =>
              createRevAlts(xr, (cond, body) :: acc)
            case remaining =>
              (EmptyTree, Block(remaining)(remaining.head.pos)) :: acc
          }
          val revAlts = createRevAlts(stats, Nil)

          if (revAlts.size == returnCount) {
            @tailrec
            def constructOptimized(revAlts: List[(Tree, Tree)], elsep: Tree): Option[Tree] = {
              revAlts match {
                case (cond, body) :: revAltsRest =>
                  body match {
                    case BlockOrAlone(prep,
                        Return(result, Some(Ident(newLabel, _)))) =>
                      val result1 =
                        if (refinedType == NoType) keepOnlySideEffects(result)
                        else result
                      val prepAndResult = Block(prep :+ result1)(body.pos)
                      if (cond == EmptyTree) {
                        assert(elsep == EmptyTree)
                        constructOptimized(revAltsRest, prepAndResult)
                      } else {
                        assert(elsep != EmptyTree)
                        constructOptimized(revAltsRest,
                            foldIf(cond, prepAndResult, elsep)(refinedType)(cond.pos))
                      }
                    case _ =>
                      None
                  }
                case Nil =>
                  Some(elsep)
              }
            }
            constructOptimized(revAlts, EmptyTree)
          } else None
        case _ =>
          None
      }
    }
  }

  private def withBindings(bindings: List[Binding])(body: => Tree): Tree = {
    withNewLocalDefs(bindings) { localDefs =>
      val newMappings = for {
        (binding, localDef) <- bindings zip localDefs
      } yield {
        binding.name -> localDef
      }
      withEnv(env.withLocalDefs(newMappings)) {
        body
      }
    }
  }

  private def withBinding(binding: Binding)(body: => Tree): Tree = {
    withNewLocalDef(binding) { localDef =>
      withEnv(env.withLocalDef(binding.name, localDef)) {
        body
      }
    }
  }

  private def withNewLocalDefs(bindings: List[Binding])(
      body: List[LocalDef] => Tree): Tree = {
    bindings match {
      case first :: rest =>
        withNewLocalDef(first) { firstLocalDef =>
          withNewLocalDefs(rest) { restLocalDefs =>
            body(firstLocalDef :: restLocalDefs)
          }
        }

      case Nil =>
        body(Nil)
    }
  }

  private def withNewLocalDef(binding: Binding)(body: LocalDef => Tree): Tree = {
    val Binding(name, originalName, declaredType, mutable, value) = binding
    implicit val pos = value.pos

    def withDedicatedVar(tpe: Type) = {
      val newName = freshLocalName(name)
      val newOriginalName = originalName.orElse(Some(name))
      val used = new SimpleState(false)
      withState(used) {
        val localDef = LocalDef(tpe, mutable,
            ReplaceWithVarRef(newName, newOriginalName, used))
        val newBody = body(localDef)
        if (used.value) {
          val varDef =
            VarDef(Ident(newName, newOriginalName), tpe, mutable,
                finishTransformExpr(value))
          Block(varDef, newBody)
        } else {
          val rhsSideEffects = finishTransformStat(value)
          Block(rhsSideEffects, newBody)
        }
      }
    }

    if (mutable) {
      withDedicatedVar(declaredType)
    } else {
      val refinedType = value.tpe
      value match {
        case PreTransLocalDef(localDef) if !localDef.mutable =>
          body(localDef)

        case PreTransNew(ClassType(wrapperName), _, List(closure: PreTransClosure))
            if wrapperName.startsWith(AnonFunctionClassPrefix) =>
          tryOrRollback { cancelFun =>
            val alreadyUsedState = new SimpleState[Boolean](false)
            withState(alreadyUsedState) {
              body(LocalDef(refinedType, false,
                  TentativeAnonFunReplacement(closure, alreadyUsedState, cancelFun)))
            }
          } getOrElse {
            withDedicatedVar(refinedType)
          }

        case PreTransTree(literal: Literal) =>
          body(LocalDef(refinedType, false,
              ReplaceWithConstant(literal)))

        case PreTransTree(VarRef(Ident(refName, refOriginalName), false)) =>
          body(LocalDef(refinedType, false,
              ReplaceWithVarRef(refName, refOriginalName,
                  new SimpleState(true))))

        case _ =>
          withDedicatedVar(refinedType)
      }
    }
  }

  /** Finds a type as precise as possible which is a supertype of lhs and rhs
   *  but still a subtype of upperBound.
   *  Requires that lhs and rhs be subtypes of upperBound, obviously.
   */
  private def constrainedLub(lhs: Type, rhs: Type, upperBound: Type): Type = {
    // TODO Improve this
    if (upperBound == NoType) upperBound
    else if (lhs == rhs) lhs
    else if (lhs == NothingType) rhs
    else if (rhs == NothingType) lhs
    else upperBound
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
      case ReplaceWithVarRef(name, originalName, used) =>
        used.value = true
        VarRef(Ident(name, originalName), mutable)(tpe)

      case ReplaceWithThis() =>
        This()(tpe)

      case ReplaceWithConstant(value) =>
        value

      case TentativeAnonFunReplacement(closure, alreadyUsed, cancelFun) =>
        cancelFun()
    }
  }

  private sealed abstract class LocalDefReplacement

  private final case class ReplaceWithVarRef(name: String,
      originalName: Option[String],
      used: SimpleState[Boolean]) extends LocalDefReplacement

  private final case class ReplaceWithThis() extends LocalDefReplacement

  private final case class ReplaceWithConstant(
      value: Tree) extends LocalDefReplacement

  private final case class TentativeAnonFunReplacement(
      closure: PreTransClosure,
      alreadyUsed: SimpleState[Boolean],
      cancelFun: CancelFun) extends LocalDefReplacement

  private final class LabelInfo(
      val newName: String,
      val returnedTypes: SimpleState[List[Type]] = new SimpleState(Nil))

  private class OptEnv(
      val localDefs: Map[String, LocalDef],
      val labelInfos: Map[String, LabelInfo]) {

    def withLocalDef(oldName: String, rep: LocalDef): OptEnv =
      new OptEnv(localDefs + (oldName -> rep), labelInfos)

    def withLocalDefs(reps: List[(String, LocalDef)]): OptEnv =
      new OptEnv(localDefs ++ reps, labelInfos)

    def withLabelInfo(oldName: String, info: LabelInfo): OptEnv =
      new OptEnv(localDefs, labelInfos + (oldName -> info))

    def withinFunction(paramLocalDefs: List[(String, LocalDef)]): OptEnv =
      new OptEnv(localDefs ++ paramLocalDefs, Map.empty)
  }

  private object OptEnv {
    val Empty: OptEnv = new OptEnv(Map.empty, Map.empty)
  }

  /** The result of pretransform().
   *  It has a `tpe` as precisely refined as if a full transformExpr() had
   *  been performed.
   *  It is also not dependent on the environment anymore. In some sense, it
   *  has "captured" its environment at definition site.
   */
  private sealed abstract class PreTransform {
    def pos: Position
    val tpe: Type
  }

  private final case class PreTransLocalDef(localDef: LocalDef)(
      implicit val pos: Position) extends PreTransform {
    val tpe: Type = localDef.tpe
  }

  private final case class PreTransNew(cls: ClassType, ctor: Ident,
      args: List[PreTransform])(
      implicit val pos: Position) extends PreTransform {
    val tpe: Type = cls
  }

  private final case class PreTransClosure(
      thisType: Type, params: List[ParamDef], resultType: Type, body: Tree,
      captures: List[PreTransform])(
      implicit val pos: Position) extends PreTransform {
    val tpe: Type = DynType
  }

  private final case class PreTransTree(tree: Tree) extends PreTransform {
    def pos: Position = tree.pos
    val tpe: Type = tree.tpe
  }

  private final case class Binding(name: String, originalName: Option[String],
      declaredType: Type, mutable: Boolean, value: PreTransform)

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
    def thisType: Type

    var inlineable: Boolean = false
    var isTraitImplForwarder: Boolean = false

    protected def updateInlineable(): Unit = {
      val MethodDef(Ident(methodName, _), params, _, body) = originalDef

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

      val adHocInlineAnnot = {
        (methodName.contains("sc_IndexedSeqOptimized$class") &&
            (methodName.contains("__F1__") || methodName.contains("__F2__")))
      }

      inlineable = optimizerHints.hasInlineAnnot || isTraitImplForwarder ||
          adHocInlineAnnot || {
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

          // Simple method
          case SimpleMethodBody()                                   => true

          case _ => false
        }
      }
    }
  }

  private object SimpleMethodBody {
    @tailrec
    def unapply(body: Tree): Boolean = body match {
      case New(_, _, args)                   => areSimpleArgs(args)
      case Apply(receiver, _, args)          => areSimpleArgs(receiver :: args)
      case StaticApply(receiver, _, _, args) => areSimpleArgs(receiver :: args)
      case TraitImplApply(_, _, args)        => areSimpleArgs(args)
      case Select(qual, _, _)                => isSimpleArg(qual)

      case CallHelper(helper, List(inner)) =>
        isBoxUnboxHelper(helper) && unapply(inner)
      case Block(List(inner, Undefined())) =>
        unapply(inner)

      case _ => isSimpleArg(body)
    }

    private def areSimpleArgs(args: List[Tree]): Boolean =
      args.forall(isSimpleArg)

    @tailrec
    private def isSimpleArg(arg: Tree): Boolean = arg match {
      case New(_, _, Nil)                   => true
      case Apply(receiver, _, Nil)          => isTrivialArg(receiver)
      case StaticApply(receiver, _, _, Nil) => isTrivialArg(receiver)
      case TraitImplApply(_, _, Nil)        => true

      case ArrayLength(array)        => isTrivialArg(array)
      case ArraySelect(array, index) => isTrivialArg(array) && isTrivialArg(index)

      case CallHelper(helper, List(inner)) =>
        isBoxUnboxHelper(helper) && isSimpleArg(inner)
      case _ =>
        isTrivialArg(arg)
    }

    private def isTrivialArg(arg: Tree): Boolean = arg match {
      case _:VarRef | _:This | _:Literal | _:LoadModule => true
      case _                                            => false
    }

    private val isBoxUnboxHelper =
      Set("bC", "uZ", "uC", "uB", "uS", "uI", "uJ", "uF", "uD")
  }

  private object BlockOrAlone {
    def unapply(tree: Tree): Some[(List[Tree], Tree)] = Some(tree match {
      case Block(init :+ last) => (init, last)
      case _                   => (Nil, tree)
    })
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

        case CallHelper(helper, receiver :: _) =>
          import Definitions._
          HelperTargets.get(helper) foreach { method =>
            receiver.tpe match {
              case AnyType =>
                addCalledMethod(ObjectClass, method)
              case ClassType(cls) if (!HijackedClasses.contains(cls)) =>
                addCalledMethod(cls, method)
              case _ =>
            }
          }

        case _ =>
      }
      super.traverse(tree)
    }

    private val HelperTargets = {
      import Definitions._
      Map[String, String](
        "objectToString"  -> "toString__T",
        "objectGetClass"  -> s"getClass__$ClassClass",
        "objectClone"     -> "clone__O",
        "objectFinalize"  -> "finalize__V",
        "objectNotify"    -> "notify__V",
        "objectNotifyAll" -> "notifyAll__V",
        "objectEquals"    -> "equals__O__Z",
        "objectHashCode"  -> "hashCode__I",

        "charSequenceLength"      -> "length__I",
        "charSequenceCharAt"      -> "charAt__I__C",
        "charSequenceSubSequence" -> s"subSequence__I__I__$CharSequenceClass",

        "comparableCompareTo" -> "compareTo__O__I",

        "numberByteValue"   -> "byteValue__B",
        "numberShortValue"  -> "shortValue__S",
        "numberIntValue"    -> "intValue__I",
        "numberLongValue"   -> "longValue__J",
        "numberFloatValue"  -> "floatValue__F",
        "numberDoubleValue" -> "doubleValue__D"
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
