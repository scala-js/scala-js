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

package org.scalajs.ir

import java.security.{MessageDigest, DigestOutputStream}
import java.io.{OutputStream, DataOutputStream}
import java.util.Arrays

import Names._
import Trees._
import Types._
import Tags._

object Hashers {

  def hashMethodDef(methodDef: MethodDef): MethodDef = {
    if (methodDef.hash.isDefined) methodDef
    else {
      val hasher = new TreeHasher()
      val MethodDef(flags, name, args, resultType, body) = methodDef

      hasher.mixPos(methodDef.pos)
      hasher.mixInt(MemberFlags.toBits(flags))
      hasher.mixMethodIdent(name)
      hasher.mixParamDefs(args)
      hasher.mixType(resultType)
      body.foreach(hasher.mixTree)
      hasher.mixInt(OptimizerHints.toBits(methodDef.optimizerHints))

      val hash = hasher.finalizeHash()

      MethodDef(flags, name, args, resultType, body)(
          methodDef.optimizerHints, Some(hash))(methodDef.pos)
    }
  }

  def hashJSMethodDef(methodDef: JSMethodDef): JSMethodDef = {
    if (methodDef.hash.isDefined) methodDef
    else {
      val hasher = new TreeHasher()
      val JSMethodDef(flags, name, args, body) = methodDef

      hasher.mixPos(methodDef.pos)
      hasher.mixInt(MemberFlags.toBits(flags))
      hasher.mixTree(name)
      hasher.mixParamDefs(args)
      hasher.mixTree(body)
      hasher.mixInt(OptimizerHints.toBits(methodDef.optimizerHints))

      val hash = hasher.finalizeHash()

      JSMethodDef(flags, name, args, body)(
          methodDef.optimizerHints, Some(hash))(methodDef.pos)
    }
  }

  /** Hash definitions from a ClassDef where applicable */
  def hashMemberDefs(memberDefs: List[MemberDef]): List[MemberDef] = memberDefs.map {
    case methodDef: MethodDef   => hashMethodDef(methodDef)
    case methodDef: JSMethodDef => hashJSMethodDef(methodDef)
    case otherDef               => otherDef
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
      mixLocalIdent(paramDef.name)
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
          mixLocalIdent(ident)
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
          mixLabelIdent(label)
          mixType(tpe)
          mixTree(body)

        case Assign(lhs, rhs) =>
          mixTag(TagAssign)
          mixTree(lhs)
          mixTree(rhs)

        case Return(expr, label) =>
          mixTag(TagReturn)
          mixTree(expr)
          mixLabelIdent(label)

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
          mixLocalIdent(keyVar)
          mixTree(body)

        case TryCatch(block, errVar, handler) =>
          mixTag(TagTryCatch)
          mixTree(block)
          mixLocalIdent(errVar)
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

        case New(className, ctor, args) =>
          mixTag(TagNew)
          mixName(className)
          mixMethodIdent(ctor)
          mixTrees(args)

        case LoadModule(className) =>
          mixTag(TagLoadModule)
          mixName(className)

        case StoreModule(className, value) =>
          mixTag(TagStoreModule)
          mixName(className)
          mixTree(value)

        case Select(qualifier, className, field) =>
          mixTag(TagSelect)
          mixTree(qualifier)
          mixName(className)
          mixFieldIdent(field)
          mixType(tree.tpe)

        case SelectStatic(className, field) =>
          mixTag(TagSelectStatic)
          mixName(className)
          mixFieldIdent(field)
          mixType(tree.tpe)

        case Apply(flags, receiver, method, args) =>
          mixTag(TagApply)
          mixInt(ApplyFlags.toBits(flags))
          mixTree(receiver)
          mixMethodIdent(method)
          mixTrees(args)
          mixType(tree.tpe)

        case ApplyStatically(flags, receiver, className, method, args) =>
          mixTag(TagApplyStatically)
          mixInt(ApplyFlags.toBits(flags))
          mixTree(receiver)
          mixName(className)
          mixMethodIdent(method)
          mixTrees(args)
          mixType(tree.tpe)

        case ApplyStatic(flags, className, method, args) =>
          mixTag(TagApplyStatic)
          mixInt(ApplyFlags.toBits(flags))
          mixName(className)
          mixMethodIdent(method)
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

        case NewArray(typeRef, lengths) =>
          mixTag(TagNewArray)
          mixArrayTypeRef(typeRef)
          mixTrees(lengths)

        case ArrayValue(typeRef, elems) =>
          mixTag(TagArrayValue)
          mixArrayTypeRef(typeRef)
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

        case RecordSelect(record, field) =>
          mixTag(TagRecordSelect)
          mixTree(record)
          mixFieldIdent(field)
          mixType(tree.tpe)

        case IsInstanceOf(expr, testType) =>
          mixTag(TagIsInstanceOf)
          mixTree(expr)
          mixType(testType)

        case AsInstanceOf(expr, tpe) =>
          mixTag(TagAsInstanceOf)
          mixTree(expr)
          mixType(tpe)

        case GetClass(expr) =>
          mixTag(TagGetClass)
          mixTree(expr)

        case JSNew(ctor, args) =>
          mixTag(TagJSNew)
          mixTree(ctor)
          mixTreeOrJSSpreads(args)

        case JSPrivateSelect(qualifier, className, field) =>
          mixTag(TagJSPrivateSelect)
          mixTree(qualifier)
          mixName(className)
          mixFieldIdent(field)

        case JSSelect(qualifier, item) =>
          mixTag(TagJSSelect)
          mixTree(qualifier)
          mixTree(item)

        case JSFunctionApply(fun, args) =>
          mixTag(TagJSFunctionApply)
          mixTree(fun)
          mixTreeOrJSSpreads(args)

        case JSMethodApply(receiver, method, args) =>
          mixTag(TagJSMethodApply)
          mixTree(receiver)
          mixTree(method)
          mixTreeOrJSSpreads(args)

        case JSSuperSelect(superClass, qualifier, item) =>
          mixTag(TagJSSuperSelect)
          mixTree(superClass)
          mixTree(qualifier)
          mixTree(item)

        case JSSuperMethodCall(superClass, receiver, method, args) =>
          mixTag(TagJSSuperMethodCall)
          mixTree(superClass)
          mixTree(receiver)
          mixTree(method)
          mixTreeOrJSSpreads(args)

        case JSSuperConstructorCall(args) =>
          mixTag(TagJSSuperConstructorCall)
          mixTreeOrJSSpreads(args)

        case JSImportCall(arg) =>
          mixTag(TagJSImportCall)
          mixTree(arg)

        case LoadJSConstructor(className) =>
          mixTag(TagLoadJSConstructor)
          mixName(className)

        case LoadJSModule(className) =>
          mixTag(TagLoadJSModule)
          mixName(className)

        case JSDelete(qualifier, item) =>
          mixTag(TagJSDelete)
          mixTree(qualifier)
          mixTree(item)

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
          fields.foreach { case (key, value) =>
            mixTree(key)
            mixTree(value)
          }

        case JSGlobalRef(name) =>
          mixTag(TagJSGlobalRef)
          mixString(name)

        case JSTypeOfGlobalRef(globalRef) =>
          mixTag(TagJSTypeOfGlobalRef)
          mixTree(globalRef)

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

        case ClassOf(typeRef) =>
          mixTag(TagClassOf)
          mixTypeRef(typeRef)

        case VarRef(ident) =>
          mixTag(TagVarRef)
          mixLocalIdent(ident)
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

        case CreateJSClass(className, captureValues) =>
          mixTag(TagCreateJSClass)
          mixName(className)
          mixTrees(captureValues)

        case Transient(value) =>
          throw new InvalidIRException(tree,
              "Cannot hash a transient IR node (its value is of class " +
              s"${value.getClass})")
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
      case PrimRef(tpe) =>
        tpe match {
          case NoType      => mixTag(TagVoidRef)
          case BooleanType => mixTag(TagBooleanRef)
          case CharType    => mixTag(TagCharRef)
          case ByteType    => mixTag(TagByteRef)
          case ShortType   => mixTag(TagShortRef)
          case IntType     => mixTag(TagIntRef)
          case LongType    => mixTag(TagLongRef)
          case FloatType   => mixTag(TagFloatRef)
          case DoubleType  => mixTag(TagDoubleRef)
          case NullType    => mixTag(TagNullRef)
          case NothingType => mixTag(TagNothingRef)
        }
      case ClassRef(className) =>
        mixTag(TagClassRef)
        mixName(className)
      case typeRef: ArrayTypeRef =>
        mixTag(TagArrayTypeRef)
        mixArrayTypeRef(typeRef)
    }

    def mixArrayTypeRef(arrayTypeRef: ArrayTypeRef): Unit = {
      mixTypeRef(arrayTypeRef.base)
      mixInt(arrayTypeRef.dimensions)
    }

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
        mixName(className)

      case ArrayType(arrayTypeRef) =>
        mixTag(TagArrayType)
        mixArrayTypeRef(arrayTypeRef)

      case RecordType(fields) =>
        mixTag(TagRecordType)
        for (RecordType.Field(name, originalName, tpe, mutable) <- fields) {
          mixName(name)
          originalName.foreach(mixString)
          mixType(tpe)
          mixBoolean(mutable)
        }
    }

    def mixLocalIdent(ident: LocalIdent): Unit = {
      mixPos(ident.pos)
      mixName(ident.name)
      ident.originalName.foreach(mixString)
    }

    def mixLabelIdent(ident: LabelIdent): Unit = {
      mixPos(ident.pos)
      mixName(ident.name)
    }

    def mixFieldIdent(ident: FieldIdent): Unit = {
      mixPos(ident.pos)
      mixName(ident.name)
      ident.originalName.foreach(mixString)
    }

    def mixMethodIdent(ident: MethodIdent): Unit = {
      mixPos(ident.pos)
      mixMethodName(ident.name)
      ident.originalName.foreach(mixString)
    }

    def mixClassIdent(ident: ClassIdent): Unit = {
      mixPos(ident.pos)
      mixName(ident.name)
      ident.originalName.foreach(mixString)
    }

    def mixName(name: Name): Unit = {
      val encoded = name.unsafeEncoded
      digestStream.writeInt(encoded.length)
      digestStream.write(encoded)
    }

    def mixMethodName(name: MethodName): Unit = {
      mixName(name.simpleName)
      mixInt(name.paramTypeRefs.size)
      for (typeRef <- name.paramTypeRefs)
        mixTypeRef(typeRef)
      mixBoolean(name.resultTypeRef.isDefined)
      for (typeRef <- name.resultTypeRef)
        mixTypeRef(typeRef)
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
