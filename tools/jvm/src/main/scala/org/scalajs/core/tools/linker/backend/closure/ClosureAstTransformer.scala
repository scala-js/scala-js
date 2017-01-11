package org.scalajs.core.tools.linker.backend.closure

import scala.annotation.switch

import org.scalajs.core.ir
import ir.Position
import ir.Position.NoPosition

import org.scalajs.core.tools.javascript.Trees._

import com.google.javascript.rhino._
import com.google.javascript.jscomp._

import scala.collection.mutable
import scala.annotation.tailrec

import java.net.URI

private[closure] class ClosureAstTransformer(relativizeBaseURI: Option[URI]) {

  private val inputId = new InputId("Scala.js IR")

  private val dummySourceName = new java.net.URI("virtualfile:scala.js-ir")

  def transformStat(tree: Tree)(implicit parentPos: Position): Node =
    innerTransformStat(tree, tree.pos orElse parentPos)

  private def innerTransformStat(tree: Tree, pos_in: Position): Node = {
    implicit val pos = pos_in

    wrapTransform(tree) {
      case VarDef(ident, optRhs) =>
        val node = transformName(ident)
        optRhs.foreach(rhs => node.addChildToFront(transformExpr(rhs)))
        new Node(Token.VAR, node)
      case Skip() =>
        new Node(Token.EMPTY)
      case Block(stats) =>
        transformBlock(stats, pos)
      case Labeled(label, body) =>
        new Node(Token.LABEL, transformLabel(label), transformBlock(body))
      case Return(expr) =>
        new Node(Token.RETURN, transformExpr(expr))
      case If(cond, thenp, Skip()) =>
        new Node(Token.IF, transformExpr(cond), transformBlock(thenp))
      case If(cond, thenp, elsep) =>
        new Node(Token.IF, transformExpr(cond),
            transformBlock(thenp), transformBlock(elsep))
      case While(cond, body, None) =>
        new Node(Token.WHILE, transformExpr(cond), transformBlock(body))
      case While(cond, body, Some(label)) =>
        val whileNode =
          new Node(Token.WHILE, transformExpr(cond), transformBlock(body))
        new Node(Token.LABEL, transformLabel(label),
            setNodePosition(whileNode, pos))
      case DoWhile(body, cond, None) =>
        new Node(Token.DO, transformBlock(body), transformExpr(cond))
      case DoWhile(body, cond, Some(label)) =>
        val doNode =
          new Node(Token.DO, transformBlock(body), transformExpr(cond))
        new Node(Token.LABEL, transformLabel(label),
            setNodePosition(doNode, pos))
      case TryFinally(TryCatch(block, errVar, handler), finalizer) =>
        val catchPos = handler.pos orElse pos
        val catchNode =
          new Node(Token.CATCH, transformName(errVar), transformBlock(handler))
        val blockNode =
          new Node(Token.BLOCK, setNodePosition(catchNode, catchPos))
        new Node(Token.TRY, transformBlock(block),
            setNodePosition(blockNode, catchPos), transformBlock(finalizer))
      case TryCatch(block, errVar, handler) =>
        val catchPos = handler.pos orElse pos
        val catchNode =
          new Node(Token.CATCH, transformName(errVar), transformBlock(handler))
        val blockNode =
          new Node(Token.BLOCK, setNodePosition(catchNode, catchPos))
        new Node(Token.TRY, transformBlock(block),
            setNodePosition(blockNode, catchPos))
      case TryFinally(block, finalizer) =>
        val blockNode = setNodePosition(new Node(Token.BLOCK), pos)
        new Node(Token.TRY, transformBlock(block), blockNode,
            transformBlock(finalizer))
      case Throw(expr) =>
        new Node(Token.THROW, transformExpr(expr))
      case Break(None) =>
        new Node(Token.BREAK)
      case Break(Some(label)) =>
        new Node(Token.BREAK, transformLabel(label))
      case Continue(None) =>
        new Node(Token.CONTINUE)
      case Continue(Some(label)) =>
       new Node(Token.CONTINUE, transformLabel(label))

      case Switch(selector, cases, default) =>
        val switchNode = new Node(Token.SWITCH, transformExpr(selector))

        for ((expr, body) <- cases) {
          val bodyNode = transformBlock(body)
          bodyNode.putBooleanProp(Node.SYNTHETIC_BLOCK_PROP, true)
          val caseNode = new Node(Token.CASE, transformExpr(expr), bodyNode)
          switchNode.addChildToBack(
              setNodePosition(caseNode, expr.pos orElse pos))
        }

        val bodyNode = transformBlock(default)
        bodyNode.putBooleanProp(Node.SYNTHETIC_BLOCK_PROP, true)
        val caseNode = new Node(Token.DEFAULT_CASE, bodyNode)
        switchNode.addChildToBack(
            setNodePosition(caseNode, default.pos orElse pos))

        switchNode

      case Debugger() =>
        new Node(Token.DEBUGGER)

      case FunctionDef(name, args, body) =>
        val node = transformName(name)
        val rhs = genFunction(name.name, args, body)
        node.addChildToFront(rhs)
        new Node(Token.VAR, node)

      case _ =>
        // We just assume it is an expression
        new Node(Token.EXPR_RESULT, transformExpr(tree))
    }
  }

  def transformExpr(tree: Tree)(implicit parentPos: Position): Node =
    innerTransformExpr(tree, tree.pos orElse parentPos)

  private def innerTransformExpr(tree: Tree, pos_in: Position): Node = {
    implicit val pos = pos_in

    wrapTransform(tree) {
      case Block(exprs) =>
        exprs.map(transformExpr).reduceRight { (expr1, expr2) =>
          setNodePosition(new Node(Token.COMMA, expr1, expr2), pos)
        }
      case If(cond, thenp, elsep) =>
        new Node(Token.HOOK, transformExpr(cond),
            transformExpr(thenp), transformExpr(elsep))
      case Assign(lhs, rhs) =>
        new Node(Token.ASSIGN, transformExpr(lhs), transformExpr(rhs))
      case New(ctor, args) =>
        val node = new Node(Token.NEW, transformExpr(ctor))
        args.foreach(arg => node.addChildToBack(transformExpr(arg)))
        node
      case DotSelect(qualifier, item) =>
        new Node(Token.GETPROP, transformExpr(qualifier), transformString(item))
      case BracketSelect(qualifier, item) =>
        new Node(Token.GETELEM, transformExpr(qualifier), transformExpr(item))

      case Apply(fun, args) =>
        val node = new Node(Token.CALL, transformExpr(fun))
        args.foreach(arg => node.addChildToBack(transformExpr(arg)))

        // Closure needs to know (from the parser), if the call has a bound
        // `this` or not. Since JSDesugar inserts protects calls if necessary,
        // it is sufficient to check if we have a select as target
        if (!fun.isInstanceOf[DotSelect] &&
            !fun.isInstanceOf[BracketSelect])
          node.putBooleanProp(Node.FREE_CALL, true)

        node

      case Delete(prop) =>
        new Node(Token.DELPROP, transformExpr(prop))
      case UnaryOp(op, lhs) =>
        mkUnaryOp(op, transformExpr(lhs))
      case BinaryOp(op, lhs, rhs) =>
        mkBinaryOp(op, transformExpr(lhs), transformExpr(rhs))
      case ArrayConstr(items) =>
        val node = new Node(Token.ARRAYLIT)
        items.foreach(i => node.addChildToBack(transformExpr(i)))
        node

      case ObjectConstr(fields) =>
        val node = new Node(Token.OBJECTLIT)

        for ((name, expr) <- fields) {
          val fldNode = transformStringKey(name)
          fldNode.addChildToBack(transformExpr(expr))
          node.addChildToBack(fldNode)
        }

        node

      case Undefined() =>
        new Node(Token.VOID, setNodePosition(Node.newNumber(0.0), pos))
      case Null() =>
        new Node(Token.NULL)
      case BooleanLiteral(value) =>
        if (value) new Node(Token.TRUE) else new Node(Token.FALSE)
      case IntLiteral(value) =>
        Node.newNumber(value)
      case DoubleLiteral(value) =>
        Node.newNumber(value)
      case StringLiteral(value) =>
        Node.newString(value)
      case VarRef(ident) =>
        transformName(ident)
      case This() =>
        new Node(Token.THIS)

      case Function(args, body) =>
        genFunction("", args, body)
      case FunctionDef(name, args, body) =>
        genFunction(name.name, args, body)

      case _ =>
        throw new TransformException(s"Unknown tree of class ${tree.getClass()}")
    }
  }

  private def genFunction(name: String, args: List[ParamDef], body: Tree)(
      implicit pos: Position): Node = {
    val paramList = new Node(Token.PARAM_LIST)
    args.foreach(arg => paramList.addChildToBack(transformParam(arg)))

    val nameNode = setNodePosition(Node.newString(Token.NAME, name), pos)

    new Node(Token.FUNCTION, nameNode, paramList, transformBlock(body))
  }

  def transformParam(param: ParamDef)(implicit parentPos: Position): Node =
    transformName(param.name)

  def transformName(ident: Ident)(implicit parentPos: Position): Node =
    setNodePosition(Node.newString(Token.NAME, ident.name),
        ident.pos orElse parentPos)

  def transformLabel(ident: Ident)(implicit parentPos: Position): Node =
    setNodePosition(Node.newString(Token.LABEL_NAME, ident.name),
        ident.pos orElse parentPos)

  def transformString(ident: Ident)(implicit parentPos: Position): Node =
    setNodePosition(Node.newString(ident.name), ident.pos orElse parentPos)

  def transformStringKey(pName: PropertyName)(
      implicit parentPos: Position): Node = {
    val node = pName match {
      case Ident(name, _) =>
        Node.newString(Token.STRING_KEY, name)

      case StringLiteral(name) =>
        val node = Node.newString(Token.STRING_KEY, name)
        node.setQuotedString()
        node

      case ComputedName(tree) =>
        throw new AssertionError(
            "The Closure AST compiler received a ComputedName, which it " +
            "cannot translate because it always emits ES 5.1 code. " +
            "Position: " + parentPos)
    }

    setNodePosition(node, pName.pos orElse parentPos)
  }

  def transformBlock(tree: Tree)(implicit parentPos: Position): Node = {
    val pos = if (tree.pos.isDefined) tree.pos else parentPos
    wrapTransform(tree) {
      case Block(stats) =>
        transformBlock(stats, pos)
      case tree =>
        transformBlock(List(tree), pos)
    } (pos)
  }

  def transformBlock(stats: List[Tree], blockPos: Position): Node = {
    @inline def ctorDoc() = {
      val b = new JSDocInfoBuilder(false)
      b.recordConstructor()
      b.build()
    }

    val block = new Node(Token.BLOCK)

    // The Rhino IR attaches DocComments to the following nodes (rather than
    // having individual nodes). We preprocess these here.
    @tailrec
    def loop(ts: List[Tree], nextIsCtor: Boolean = false): Unit = ts match {
      case DocComment(text) :: tss if text.startsWith("@constructor") =>
        loop(tss, nextIsCtor = true)
      case DocComment(text) :: tss =>
        loop(tss)
      case t :: tss =>
        val node = transformStat(t)(blockPos)
        if (nextIsCtor) {
          // The @constructor must be propagated through an ExprResult node
          val trg =
            if (node.isExprResult()) node.getChildAtIndex(0)
            else node

          trg.setJSDocInfo(ctorDoc())
        }

        block.addChildToBack(node)

        loop(tss)
      case Nil =>
    }

    loop(stats)

    block
  }

  @inline
  private def wrapTransform(tree: Tree)(body: Tree => Node)(
      implicit pos: Position): Node = {
    try {
      setNodePosition(body(tree), pos)
    } catch {
      case e: TransformException =>
        throw e // pass through
      case e: RuntimeException =>
        throw new TransformException(tree, e)
    }
  }

  def setNodePosition(node: Node, pos: ir.Position): node.type = {
    if (pos != ir.Position.NoPosition) {
      attachSourceFile(node, pos.source)
      node.setLineno(pos.line+1)
      node.setCharno(pos.column)
    } else {
      attachSourceFile(node, dummySourceName)
    }
    node
  }

  private def attachSourceFile(node: Node, source: URI): node.type = {
    val str = sourceUriToString(source)

    node.setInputId(inputId)
    node.setStaticSourceFile(new SourceFile(str))

    node
  }

  private def sourceUriToString(uri: URI): String = {
    val relURI = relativizeBaseURI.fold(uri)(ir.Utils.relativize(_, uri))
    ir.Utils.fixFileURI(relURI).toASCIIString
  }

  // Helpers for IR
  @inline
  private def mkUnaryOp(op: UnaryOp.Code, lhs: Node): Node = {
    import ir.Trees.JSUnaryOp._
    val tok = (op: @switch) match {
      case !        => Token.NOT
      case ~        => Token.BITNOT
      case +        => Token.POS
      case -        => Token.NEG
      case `typeof` => Token.TYPEOF
    }

    new Node(tok, lhs)
  }

  @inline
  private def mkBinaryOp(op: BinaryOp.Code, lhs: Node, rhs: Node): Node = {
    import ir.Trees.JSBinaryOp._
    val tok = (op: @switch) match {
      case === => Token.SHEQ
      case !== => Token.SHNE

      case + => Token.ADD
      case - => Token.SUB
      case * => Token.MUL
      case / => Token.DIV
      case % => Token.MOD

      case |   => Token.BITOR
      case &   => Token.BITAND
      case ^   => Token.BITXOR
      case <<  => Token.LSH
      case >>  => Token.RSH
      case >>> => Token.URSH

      case <  => Token.LT
      case <= => Token.LE
      case >  => Token.GT
      case >= => Token.GE

      case || => Token.OR
      case && => Token.AND

      case `in`         => Token.IN
      case `instanceof` => Token.INSTANCEOF
    }

    new Node(tok, lhs, rhs)
  }

  // Exception wrapper in transforms

  class TransformException private (msg: String, e: Throwable)
      extends RuntimeException(msg, e) {

    def this(tree: Tree, e: Throwable) =
      this(TransformException.mkMsg(tree), e)

    def this(msg: String) = this(msg, null)
  }

  object TransformException {
    import ir.Printers._
    import java.io._

    private def mkMsg(tree: Tree): String = {
      "Exception while translating Scala.js JS tree to GCC IR at tree:\n" +
        tree.show
    }
  }

}
