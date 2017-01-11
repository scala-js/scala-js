package org.scalajs.core.ir

import java.security.{MessageDigest, DigestOutputStream}
import java.io.{OutputStream, DataOutputStream}
import java.util.Arrays

import Trees._
import Types._
import Tags._

object Hashers {

  def hashMethodDef(methodDef: MethodDef): MethodDef = {
    if (methodDef.hash.isDefined) methodDef
    else {
      val hasher = new TreeHasher()
      val MethodDef(static, name, args, resultType, body) = methodDef

      hasher.mixPos(methodDef.pos)
      hasher.mixBoolean(static)
      hasher.mixPropertyName(name)
      hasher.mixTrees(args)
      hasher.mixType(resultType)
      body.foreach(hasher.mixTree)
      hasher.mixInt(methodDef.optimizerHints.bits)

      val hash = hasher.finalizeHash()

      MethodDef(static, name, args, resultType, body)(
          methodDef.optimizerHints, Some(hash))(methodDef.pos)
    }
  }

  /** Hash definitions from a ClassDef where applicable */
  def hashDefs(defs: List[Tree]): List[Tree] = defs map {
    case methodDef: MethodDef => hashMethodDef(methodDef)
    case otherDef             => otherDef
  }

  /** Hash the definitions in a ClassDef (where applicable) */
  def hashClassDef(classDef: ClassDef): ClassDef = {
    classDef.copy(defs = hashDefs(classDef.defs))(
        classDef.optimizerHints)(classDef.pos)
  }

  def hashesEqual(x: TreeHash, y: TreeHash, considerPos: Boolean): Boolean = {
    Arrays.equals(x.treeHash, y.treeHash) &&
      (!considerPos || Arrays.equals(x.posHash, y.posHash))
  }

  def hashAsVersion(hash: TreeHash, considerPos: Boolean): String = {
    // 2 chars per byte, 20 bytes per hash
    val size = 2 * (if (considerPos) 2 else 1) * 20
    val builder = new StringBuilder(size)

    def hexDigit(digit: Int): Char = Character.forDigit(digit, 16)

    def append(hash: Array[Byte]): Unit = {
      for (b <- hash)
        builder.append(hexDigit(b >> 4)).append(hexDigit(b & 0xF))
    }
    append(hash.treeHash)

    if (considerPos)
      append(hash.posHash)

    builder.toString
  }

  private final class TreeHasher {
    private def newDigest = MessageDigest.getInstance("SHA-1")
    private def newDigestStream(digest: MessageDigest) = {
      val out = new OutputStream {
        def write(b: Int): Unit = ()
      }
      val digOut = new DigestOutputStream(out, digest)
      new DataOutputStream(digOut)
    }

    private[this] val treeDigest = newDigest
    private[this] val treeStream = newDigestStream(treeDigest)

    private[this] val posDigest = newDigest
    private[this] val posStream = newDigestStream(posDigest)

    def finalizeHash(): TreeHash =
      new TreeHash(treeDigest.digest(), posDigest.digest())

    def mixTree(tree: Tree): Unit = {
      mixPos(tree.pos)
      tree match {
        case VarDef(ident, vtpe, mutable, rhs) =>
          mixTag(TagVarDef)
          mixIdent(ident)
          mixType(vtpe)
          mixBoolean(mutable)
          mixTree(rhs)

        case ParamDef(ident, ptpe, mutable, rest) =>
          mixTag(TagParamDef)
          mixIdent(ident)
          mixType(ptpe)
          mixBoolean(mutable)
          /* TODO Remove this test in the next major release.
           * In 0.6.x we need this test so that the hash of a non-rest ParamDef
           * emitted in 0.6.3 format is the same as an (implicitly non-rest)
           * ParamDef emitted in 0.6.0 format.
           */
          if (rest)
            mixBoolean(rest)

        case Skip() =>
          mixTag(TagSkip)

        case Block(stats) =>
          mixTag(TagBlock)
          mixTrees(stats)

        case Labeled(label, tpe, body) =>
          mixTag(TagLabeled)
          mixIdent(label)
          mixType(tpe)
          mixTree(body)

        case Assign(lhs, rhs) =>
          mixTag(TagAssign)
          mixTree(lhs)
          mixTree(rhs)

        case Return(expr, label) =>
          mixTag(TagReturn)
          mixTree(expr)
          mixOptIdent(label)

        case If(cond, thenp, elsep) =>
          mixTag(TagIf)
          mixTree(cond)
          mixTree(thenp)
          mixTree(elsep)
          mixType(tree.tpe)

        case While(cond, body, label) =>
          mixTag(TagWhile)
          mixTree(cond)
          mixTree(body)
          mixOptIdent(label)

        case DoWhile(body, cond, label) =>
          mixTag(TagDoWhile)
          mixTree(body)
          mixTree(cond)
          mixOptIdent(label)

        case TryCatch(block, errVar, handler) =>
          mixTag(TagTryCatch)
          mixTree(block)
          mixIdent(errVar)
          mixTree(handler)
          mixType(tree.tpe)

        case TryFinally(block, finalizer) =>
          mixTag(TagTryFinally)
          mixTree(block)
          mixTree(finalizer)
          mixType(tree.tpe)

        case Throw(expr) =>
          mixTag(TagThrow)
          mixTree(expr)

        case Continue(label) =>
          mixTag(TagContinue)
          mixOptIdent(label)

        case Match(selector, cases, default) =>
          mixTag(TagMatch)
          mixTree(selector)
          cases foreach { case (patterns, body) =>
            mixTrees(patterns)
            mixTree(body)
          }
          mixTree(default)
          mixType(tree.tpe)

        case Debugger() =>
          mixTag(TagDebugger)

        case New(cls, ctor, args) =>
          mixTag(TagNew)
          mixType(cls)
          mixIdent(ctor)
          mixTrees(args)

        case LoadModule(cls) =>
          mixTag(TagLoadModule)
          mixType(cls)

        case StoreModule(cls, value) =>
          mixTag(TagStoreModule)
          mixType(cls)
          mixTree(value)

        case Select(qualifier, item) =>
          mixTag(TagSelect)
          mixTree(qualifier)
          mixIdent(item)
          mixType(tree.tpe)

        case SelectStatic(cls, item) =>
          mixTag(TagSelectStatic)
          mixType(cls)
          mixIdent(item)
          mixType(tree.tpe)

        case Apply(receiver, method, args) =>
          mixTag(TagApply)
          mixTree(receiver)
          mixIdent(method)
          mixTrees(args)
          mixType(tree.tpe)

        case ApplyStatically(receiver, cls, method, args) =>
          mixTag(TagApplyStatically)
          mixTree(receiver)
          mixType(cls)
          mixIdent(method)
          mixTrees(args)
          mixType(tree.tpe)

        case ApplyStatic(cls, method, args) =>
          mixTag(TagApplyStatic)
          mixType(cls)
          mixIdent(method)
          mixTrees(args)
          mixType(tree.tpe)

        case UnaryOp(op, lhs) =>
          mixTag(TagUnaryOp)
          mixInt(op)
          mixTree(lhs)

        case BinaryOp(op, lhs, rhs) =>
          mixTag(TagBinaryOp)
          mixInt(op)
          mixTree(lhs)
          mixTree(rhs)

        case NewArray(tpe, lengths) =>
          mixTag(TagNewArray)
          mixType(tpe)
          mixTrees(lengths)

        case ArrayValue(tpe, elems) =>
          mixTag(TagArrayValue)
          mixType(tpe)
          mixTrees(elems)

        case ArrayLength(array) =>
          mixTag(TagArrayLength)
          mixTree(array)

        case ArraySelect(array, index) =>
          mixTag(TagArraySelect)
          mixTree(array)
          mixTree(index)
          mixType(tree.tpe)

        case RecordValue(tpe, elems) =>
          mixTag(TagRecordValue)
          mixType(tpe)
          mixTrees(elems)

        case IsInstanceOf(expr, cls) =>
          mixTag(TagIsInstanceOf)
          mixTree(expr)
          mixRefType(cls)

        case AsInstanceOf(expr, cls) =>
          mixTag(TagAsInstanceOf)
          mixTree(expr)
          mixRefType(cls)

        case Unbox(expr, charCode) =>
          mixTag(TagUnbox)
          mixTree(expr)
          mixInt(charCode)

        case GetClass(expr) =>
          mixTag(TagGetClass)
          mixTree(expr)

        case CallHelper(helper, args) =>
          mixTag(TagCallHelper)
          mixString(helper)
          mixTrees(args)
          mixType(tree.tpe)

        case JSNew(ctor, args) =>
          mixTag(TagJSNew)
          mixTree(ctor)
          mixTrees(args)

        case JSDotSelect(qualifier, item) =>
          mixTag(TagJSDotSelect)
          mixTree(qualifier)
          mixIdent(item)

        case JSBracketSelect(qualifier, item) =>
          mixTag(TagJSBracketSelect)
          mixTree(qualifier)
          mixTree(item)

        case JSFunctionApply(fun, args) =>
          mixTag(TagJSFunctionApply)
          mixTree(fun)
          mixTrees(args)

        case JSDotMethodApply(receiver, method, args) =>
          mixTag(TagJSDotMethodApply)
          mixTree(receiver)
          mixIdent(method)
          mixTrees(args)

        case JSBracketMethodApply(receiver, method, args) =>
          mixTag(TagJSBracketMethodApply)
          mixTree(receiver)
          mixTree(method)
          mixTrees(args)

        case JSSuperBracketSelect(cls, qualifier, item) =>
          mixTag(TagJSSuperBracketSelect)
          mixType(cls)
          mixTree(qualifier)
          mixTree(item)

        case JSSuperBracketCall(cls, receiver, method, args) =>
          mixTag(TagJSSuperBracketCall)
          mixType(cls)
          mixTree(receiver)
          mixTree(method)
          mixTrees(args)

        case JSSuperConstructorCall(args) =>
          mixTag(TagJSSuperConstructorCall)
          mixTrees(args)

        case LoadJSConstructor(cls) =>
          mixTag(TagLoadJSConstructor)
          mixType(cls)

        case LoadJSModule(cls) =>
          mixTag(TagLoadJSModule)
          mixType(cls)

        case JSSpread(items) =>
          mixTag(TagJSSpread)
          mixTree(items)

        case JSDelete(prop) =>
          mixTag(TagJSDelete)
          mixTree(prop)

        case JSUnaryOp(op, lhs) =>
          mixTag(TagJSUnaryOp)
          mixInt(op)
          mixTree(lhs)

        case JSBinaryOp(op, lhs, rhs) =>
          mixTag(TagJSBinaryOp)
          mixInt(op)
          mixTree(lhs)
          mixTree(rhs)

        case JSArrayConstr(items) =>
          mixTag(TagJSArrayConstr)
          mixTrees(items)

        case JSObjectConstr(fields) =>
          mixTag(TagJSObjectConstr)
          fields foreach { case (pn, value) =>
            mixPropertyName(pn)
            mixTree(value)
          }

        case JSLinkingInfo() =>
          mixTag(TagJSLinkingInfo)

        case Undefined() =>
          mixTag(TagUndefined)

        case UndefinedParam() =>
          mixTag(TagUndefinedParam)
          mixType(tree.tpe)

        case Null() =>
          mixTag(TagNull)

        case BooleanLiteral(value) =>
          mixTag(TagBooleanLiteral)
          mixBoolean(value)

        case IntLiteral(value) =>
          mixTag(TagIntLiteral)
          mixInt(value)

        case LongLiteral(value) =>
          mixTag(TagLongLiteral)
          mixLong(value)

        case FloatLiteral(value) =>
          mixTag(TagFloatLiteral)
          mixFloat(value)

        case DoubleLiteral(value) =>
          mixTag(TagDoubleLiteral)
          mixDouble(value)

        case StringLiteral(value) =>
          mixTag(TagStringLiteral)
          mixString(value)

        case ClassOf(cls) =>
          mixTag(TagClassOf)
          mixRefType(cls)

        case VarRef(ident) =>
          mixTag(TagVarRef)
          mixIdent(ident)
          mixType(tree.tpe)

        case This() =>
          mixTag(TagThis)
          mixType(tree.tpe)

        case Closure(captureParams, params, body, captureValues) =>
          mixTag(TagClosure)
          mixTrees(captureParams)
          mixTrees(params)
          mixTree(body)
          mixTrees(captureValues)

        case _ =>
          sys.error(s"Unable to hash tree of class ${tree.getClass}")

      }
    }

    def mixTrees(trees: List[Tree]): Unit =
      trees.foreach(mixTree)

    def mixRefType(tpe: ReferenceType): Unit =
      mixType(tpe.asInstanceOf[Type])

    def mixType(tpe: Type): Unit = tpe match {
      case AnyType     => mixTag(TagAnyType)
      case NothingType => mixTag(TagNothingType)
      case UndefType   => mixTag(TagUndefType)
      case BooleanType => mixTag(TagBooleanType)
      case IntType     => mixTag(TagIntType)
      case LongType    => mixTag(TagLongType)
      case FloatType   => mixTag(TagFloatType)
      case DoubleType  => mixTag(TagDoubleType)
      case StringType  => mixTag(TagStringType)
      case NullType    => mixTag(TagNullType)
      case NoType      => mixTag(TagNoType)

      case tpe: ClassType =>
        mixTag(TagClassType)
        mixString(tpe.className)

      case tpe: ArrayType =>
        mixTag(TagArrayType)
        mixString(tpe.baseClassName)
        mixInt(tpe.dimensions)

      case RecordType(fields) =>
        mixTag(TagRecordType)
        for (RecordType.Field(name, originalName, tpe, mutable) <- fields) {
          mixString(name)
          originalName.foreach(mixString)
          mixType(tpe)
          mixBoolean(mutable)
        }
    }

    def mixIdent(ident: Ident): Unit = {
      mixPos(ident.pos)
      mixString(ident.name)
      ident.originalName.foreach(mixString)
    }

    def mixOptIdent(optIdent: Option[Ident]): Unit = optIdent.foreach(mixIdent)

    def mixPropertyName(name: PropertyName): Unit = name match {
      case name: Ident =>
        mixTag(TagPropertyNameIdent)
        mixIdent(name)

      case name: StringLiteral =>
        mixTag(TagPropertyNameStringLiteral)
        mixTree(name)

      case ComputedName(tree, logicalName) =>
        mixTag(TagPropertyNameComputedName)
        mixTree(tree)
        mixString(logicalName)
    }

    def mixPos(pos: Position): Unit = {
      posStream.writeUTF(pos.source.toString)
      posStream.writeInt(pos.line)
      posStream.writeInt(pos.column)
    }

    @inline
    final def mixTag(tag: Int): Unit = mixInt(tag)

    @inline
    final def mixString(str: String): Unit = treeStream.writeUTF(str)

    @inline
    final def mixInt(i: Int): Unit = treeStream.writeInt(i)

    @inline
    final def mixLong(l: Long): Unit = treeStream.writeLong(l)

    @inline
    final def mixBoolean(b: Boolean): Unit = treeStream.writeBoolean(b)

    @inline
    final def mixFloat(f: Float): Unit = treeStream.writeFloat(f)

    @inline
    final def mixDouble(d: Double): Unit = treeStream.writeDouble(d)

  }

}
