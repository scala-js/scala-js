package org.scalajs.ir

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
      hasher.mixParamDefs(args)
      hasher.mixType(resultType)
      body.foreach(hasher.mixTree)
      hasher.mixInt(OptimizerHints.toBits(methodDef.optimizerHints))

      val hash = hasher.finalizeHash()

      MethodDef(static, name, args, resultType, body)(
          methodDef.optimizerHints, Some(hash))(methodDef.pos)
    }
  }

  /** Hash definitions from a ClassDef where applicable */
  def hashMemberDefs(memberDefs: List[MemberDef]): List[MemberDef] = memberDefs.map {
    case methodDef: MethodDef => hashMethodDef(methodDef)
    case otherDef             => otherDef
  }

  /** Hash the definitions in a ClassDef (where applicable) */
  def hashClassDef(classDef: ClassDef): ClassDef = {
    import classDef._
    val newMemberDefs = hashMemberDefs(memberDefs)
    ClassDef(name, kind, jsClassCaptures, superClass, interfaces, jsSuperClass,
        jsNativeLoadSpec, newMemberDefs, topLevelExportDefs)(
        optimizerHints)
  }

  def hashesEqual(x: TreeHash, y: TreeHash): Boolean =
    Arrays.equals(x.hash, y.hash)

  def hashAsVersion(hash: TreeHash): String = {
    // 2 chars per byte, 20 bytes in a hash
    val size = 2 * 20
    val builder = new StringBuilder(size)

    def hexDigit(digit: Int): Char = Character.forDigit(digit, 16)

    for (b <- hash.hash)
      builder.append(hexDigit(b >> 4)).append(hexDigit(b & 0xF))

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

    private[this] val digest = newDigest
    private[this] val digestStream = newDigestStream(digest)

    def finalizeHash(): TreeHash =
      new TreeHash(digest.digest())

    def mixParamDef(paramDef: ParamDef): Unit = {
      mixPos(paramDef.pos)
      mixIdent(paramDef.name)
      mixType(paramDef.ptpe)
      mixBoolean(paramDef.mutable)
      mixBoolean(paramDef.rest)
    }

    def mixParamDefs(paramDefs: List[ParamDef]): Unit =
      paramDefs.foreach(mixParamDef)

    def mixTree(tree: Tree): Unit = {
      mixPos(tree.pos)
      tree match {
        case VarDef(ident, vtpe, mutable, rhs) =>
          mixTag(TagVarDef)
          mixIdent(ident)
          mixType(vtpe)
          mixBoolean(mutable)
          mixTree(rhs)

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

        case While(cond, body) =>
          mixTag(TagWhile)
          mixTree(cond)
          mixTree(body)

        case DoWhile(body, cond) =>
          mixTag(TagDoWhile)
          mixTree(body)
          mixTree(cond)

        case ForIn(obj, keyVar, body) =>
          mixTag(TagForIn)
          mixTree(obj)
          mixIdent(keyVar)
          mixTree(body)

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
          mixTypeRef(cls)

        case AsInstanceOf(expr, cls) =>
          mixTag(TagAsInstanceOf)
          mixTree(expr)
          mixTypeRef(cls)

        case Unbox(expr, charCode) =>
          mixTag(TagUnbox)
          mixTree(expr)
          mixInt(charCode)

        case GetClass(expr) =>
          mixTag(TagGetClass)
          mixTree(expr)

        case JSNew(ctor, args) =>
          mixTag(TagJSNew)
          mixTree(ctor)
          mixTreeOrJSSpreads(args)

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
          mixTreeOrJSSpreads(args)

        case JSDotMethodApply(receiver, method, args) =>
          mixTag(TagJSDotMethodApply)
          mixTree(receiver)
          mixIdent(method)
          mixTreeOrJSSpreads(args)

        case JSBracketMethodApply(receiver, method, args) =>
          mixTag(TagJSBracketMethodApply)
          mixTree(receiver)
          mixTree(method)
          mixTreeOrJSSpreads(args)

        case JSSuperBracketSelect(superClass, qualifier, item) =>
          mixTag(TagJSSuperBracketSelect)
          mixTree(superClass)
          mixTree(qualifier)
          mixTree(item)

        case JSSuperBracketCall(superClass, receiver, method, args) =>
          mixTag(TagJSSuperBracketCall)
          mixTree(superClass)
          mixTree(receiver)
          mixTree(method)
          mixTreeOrJSSpreads(args)

        case JSSuperConstructorCall(args) =>
          mixTag(TagJSSuperConstructorCall)
          mixTreeOrJSSpreads(args)

        case LoadJSConstructor(cls) =>
          mixTag(TagLoadJSConstructor)
          mixType(cls)

        case LoadJSModule(cls) =>
          mixTag(TagLoadJSModule)
          mixType(cls)

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
          mixTreeOrJSSpreads(items)

        case JSObjectConstr(fields) =>
          mixTag(TagJSObjectConstr)
          fields foreach { case (pn, value) =>
            mixPropertyName(pn)
            mixTree(value)
          }

        case JSGlobalRef(ident) =>
          mixTag(TagJSGlobalRef)
          mixIdent(ident)

        case JSLinkingInfo() =>
          mixTag(TagJSLinkingInfo)

        case Undefined() =>
          mixTag(TagUndefined)

        case Null() =>
          mixTag(TagNull)

        case BooleanLiteral(value) =>
          mixTag(TagBooleanLiteral)
          mixBoolean(value)

        case CharLiteral(value) =>
          mixTag(TagCharLiteral)
          mixChar(value)

        case ByteLiteral(value) =>
          mixTag(TagByteLiteral)
          mixByte(value)

        case ShortLiteral(value) =>
          mixTag(TagShortLiteral)
          mixShort(value)

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
          mixTypeRef(cls)

        case VarRef(ident) =>
          mixTag(TagVarRef)
          mixIdent(ident)
          mixType(tree.tpe)

        case This() =>
          mixTag(TagThis)
          mixType(tree.tpe)

        case Closure(arrow, captureParams, params, body, captureValues) =>
          mixTag(TagClosure)
          mixBoolean(arrow)
          mixParamDefs(captureParams)
          mixParamDefs(params)
          mixTree(body)
          mixTrees(captureValues)

        case CreateJSClass(cls, captureValues) =>
          mixTag(TagCreateJSClass)
          mixClassRef(cls)
          mixTrees(captureValues)

        case Transient(value) =>
          throw new InvalidIRException(tree,
              "Cannot hash a transient IR node (its value is of class " +
              s"${value.getClass})")

        case _ =>
          throw new IllegalArgumentException(
              s"Unable to hash tree of class ${tree.getClass}")
      }
    }

    def mixOptTree(optTree: Option[Tree]): Unit =
      optTree.foreach(mixTree)

    def mixTrees(trees: List[Tree]): Unit =
      trees.foreach(mixTree)

    def mixTreeOrJSSpreads(trees: List[TreeOrJSSpread]): Unit =
      trees.foreach(mixTreeOrJSSpread)

    def mixTreeOrJSSpread(tree: TreeOrJSSpread): Unit = {
      tree match {
        case JSSpread(items) =>
          mixTag(TagJSSpread)
          mixTree(items)
        case tree: Tree =>
          mixTree(tree)
      }
    }

    def mixTypeRef(typeRef: TypeRef): Unit = typeRef match {
      case ClassRef(className) =>
        mixTag(TagClassRef)
        mixString(className)
      case ArrayTypeRef(baseClassName, dimensions) =>
        mixTag(TagArrayTypeRef)
        mixString(baseClassName)
        mixInt(dimensions)
    }

    def mixClassRef(classRef: ClassRef): Unit =
      mixString(classRef.className)

    def mixType(tpe: Type): Unit = tpe match {
      case AnyType     => mixTag(TagAnyType)
      case NothingType => mixTag(TagNothingType)
      case UndefType   => mixTag(TagUndefType)
      case BooleanType => mixTag(TagBooleanType)
      case CharType    => mixTag(TagCharType)
      case ByteType    => mixTag(TagByteType)
      case ShortType   => mixTag(TagShortType)
      case IntType     => mixTag(TagIntType)
      case LongType    => mixTag(TagLongType)
      case FloatType   => mixTag(TagFloatType)
      case DoubleType  => mixTag(TagDoubleType)
      case StringType  => mixTag(TagStringType)
      case NullType    => mixTag(TagNullType)
      case NoType      => mixTag(TagNoType)

      case ClassType(className) =>
        mixTag(TagClassType)
        mixString(className)

      case ArrayType(ArrayTypeRef(baseClassName, dimensions)) =>
        mixTag(TagArrayType)
        mixString(baseClassName)
        mixInt(dimensions)

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
      digestStream.writeUTF(pos.source.toString)
      digestStream.writeInt(pos.line)
      digestStream.writeInt(pos.column)
    }

    @inline
    final def mixTag(tag: Int): Unit = mixInt(tag)

    @inline
    final def mixString(str: String): Unit = digestStream.writeUTF(str)

    @inline
    final def mixChar(c: Char): Unit = digestStream.writeChar(c)

    @inline
    final def mixByte(b: Byte): Unit = digestStream.writeByte(b)

    @inline
    final def mixShort(s: Short): Unit = digestStream.writeShort(s)

    @inline
    final def mixInt(i: Int): Unit = digestStream.writeInt(i)

    @inline
    final def mixLong(l: Long): Unit = digestStream.writeLong(l)

    @inline
    final def mixBoolean(b: Boolean): Unit = digestStream.writeBoolean(b)

    @inline
    final def mixFloat(f: Float): Unit = digestStream.writeFloat(f)

    @inline
    final def mixDouble(d: Double): Unit = digestStream.writeDouble(d)

  }

}
