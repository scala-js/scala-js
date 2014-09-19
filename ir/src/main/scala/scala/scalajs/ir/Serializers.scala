/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.ir

import scala.annotation.switch

import java.io._
import java.net.URI

import scala.collection.mutable

import Position._
import Trees._
import Types._

object Serializers {
  def serialize(stream: OutputStream, tree: Tree): Unit = {
    new Serializer().serialize(stream, tree)
  }

  def deserialize(stream: InputStream, version: String): Tree = {
    new Deserializer(stream, version).deserialize()
  }

  // true for easier debugging (not for "production", it adds 8 bytes per node)
  private final val UseDebugMagic = false
  private final val DebugMagic = 0x3fa8ef84
  private final val PosDebugMagic = 0x65f0ec32

  private object PositionFormat {
    /* Positions are serialized incrementally as diffs wrt the last position.
     *
     * Formats are (the first byte is decomposed in bits):
     *
     *   1st byte | next bytes |  description
     *  -----------------------------------------
     *   ccccccc0 |            | Column diff (7-bit signed)
     *   llllll01 | CC         | Line diff (6-bit signed), column (8-bit unsigned)
     *   ____0011 | LL LL CC   | Line diff (16-bit signed), column (8-bit unsigned)
     *   ____0111 | 12 bytes   | File index, line, column (all 32-bit signed)
     *   11111111 |            | NoPosition (is not compared/stored in last position)
     *
     * Underscores are irrelevant and must be set to 0.
     */

    final val Format1Mask = 0x01
    final val Format1MaskValue = 0x00
    final val Format1Shift = 1

    final val Format2Mask = 0x03
    final val Format2MaskValue = 0x01
    final val Format2Shift = 2

    final val Format3Mask = 0x0f
    final val Format3MaskValue = 0x03

    final val FormatFullMask = 0x0f
    final val FormatFullMaskValue = 0x7

    final val FormatNoPositionValue = -1
  }

  private final class Serializer {
    private[this] val bufferUnderlying = new ByteArrayOutputStream
    private[this] val buffer = new DataOutputStream(bufferUnderlying)

    private[this] val files = mutable.ListBuffer.empty[URI]
    private[this] val fileIndexMap = mutable.Map.empty[URI, Int]
    private def fileToIndex(file: URI): Int =
      fileIndexMap.getOrElseUpdate(file, (files += file).size - 1)

    private[this] val strings = mutable.ListBuffer.empty[String]
    private[this] val stringIndexMap = mutable.Map.empty[String, Int]
    private def stringToIndex(str: String): Int =
      stringIndexMap.getOrElseUpdate(str, (strings += str).size - 1)

    private[this] var lastPosition: Position = Position.NoPosition

    def serialize(stream: OutputStream, tree: Tree): Unit = {
      // Write tree to buffer and record files and strings
      writeTree(tree)
      buffer.flush()

      val s = new DataOutputStream(stream)

      // Emit the files
      s.writeInt(files.size)
      files.foreach(f => s.writeUTF(f.toString))

      // Emit the strings
      s.writeInt(strings.size)
      strings.foreach(s.writeUTF)

      // Paste the buffer
      bufferUnderlying.writeTo(s)

      s.flush()
    }

    def writeTree(tree: Tree): Unit = {
      import buffer._
      writePosition(tree.pos)
      tree match {
        case EmptyTree =>
          writeByte(TagEmptyTree)

        case DocComment(text) =>
          writeByte(TagDocComment)
          writeString(text)

        case VarDef(ident, vtpe, mutable, rhs) =>
          writeByte(TagVarDef)
          writeIdent(ident); writeType(vtpe); writeBoolean(mutable); writeTree(rhs)

        case ParamDef(ident, ptpe, mutable) =>
          writeByte(TagParamDef)
          writeIdent(ident); writeType(ptpe); writeBoolean(mutable)

        case Skip() =>
          writeByte(TagSkip)

        case Block(stats) =>
          writeByte(TagBlock)
          writeTrees(stats)

        case Labeled(label, tpe, body) =>
          writeByte(TagLabeled)
          writeIdent(label); writeType(tpe); writeTree(body)

        case Assign(lhs, rhs) =>
          writeByte(TagAssign)
          writeTree(lhs); writeTree(rhs)

        case Return(expr, label) =>
          writeByte(TagReturn)
          writeTree(expr); writeOptIdent(label)

        case If(cond, thenp, elsep) =>
          writeByte(TagIf)
          writeTree(cond); writeTree(thenp); writeTree(elsep)
          writeType(tree.tpe)

        case While(cond, body, label) =>
          writeByte(TagWhile)
          writeTree(cond); writeTree(body); writeOptIdent(label)

        case DoWhile(body, cond, label) =>
          writeByte(TagDoWhile)
          writeTree(body); writeTree(cond); writeOptIdent(label)

        case Try(block, errVar, handler, finalizer) =>
          writeByte(TagTry)
          writeTree(block); writeIdent(errVar); writeTree(handler); writeTree(finalizer)
          writeType(tree.tpe)

        case Throw(expr) =>
          writeByte(TagThrow)
          writeTree(expr)

        case Break(label) =>
          writeByte(TagBreak)
          writeOptIdent(label)

        case Continue(label) =>
          writeByte(TagContinue)
          writeOptIdent(label)

        case Switch(selector, cases, default) =>
          writeByte(TagSwitch)
          writeTree(selector)
          writeInt(cases.size)
          cases foreach { caze =>
            writeTree(caze._1); writeTree(caze._2)
          }
          writeTree(default)

        case Match(selector, cases, default) =>
          writeByte(TagMatch)
          writeTree(selector)
          writeInt(cases.size)
          cases foreach { caze =>
            writeTrees(caze._1); writeTree(caze._2)
          }
          writeTree(default)
          writeType(tree.tpe)

        case Debugger() =>
          writeByte(TagDebugger)

        case New(cls, ctor, args) =>
          writeByte(TagNew)
          writeClassType(cls); writeIdent(ctor); writeTrees(args)

        case LoadModule(cls) =>
          writeByte(TagLoadModule)
          writeClassType(cls)

        case StoreModule(cls, value) =>
          writeByte(TagStoreModule)
          writeClassType(cls); writeTree(value)

        case Select(qualifier, item, mutable) =>
          writeByte(TagSelect)
          writeTree(qualifier); writeIdent(item); writeBoolean(mutable)
          writeType(tree.tpe)

        case Apply(receiver, method, args) =>
          writeByte(TagApply)
          writeTree(receiver); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case StaticApply(receiver, cls, method, args) =>
          writeByte(TagStaticApply)
          writeTree(receiver); writeClassType(cls); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case TraitImplApply(impl, method, args) =>
          writeByte(TagTraitImplApply)
          writeClassType(impl); writeIdent(method); writeTrees(args)
          writeType(tree.tpe)

        case UnaryOp(op, lhs) =>
          writeByte(TagUnaryOp)
          writeByte(op); writeTree(lhs)

        case BinaryOp(op, lhs, rhs) =>
          writeByte(TagBinaryOp)
          writeByte(op); writeTree(lhs); writeTree(rhs)

        case NewArray(tpe, lengths) =>
          writeByte(TagNewArray)
          writeArrayType(tpe); writeTrees(lengths)

        case ArrayValue(tpe, elems) =>
          writeByte(TagArrayValue)
          writeArrayType(tpe); writeTrees(elems)

        case ArrayLength(array) =>
          writeByte(TagArrayLength)
          writeTree(array)

        case ArraySelect(array, index) =>
          writeByte(TagArraySelect)
          writeTree(array); writeTree(index)
          writeType(tree.tpe)

        case RecordValue(tpe, elems) =>
          writeByte(TagRecordValue)
          writeType(tpe); writeTrees(elems)

        case IsInstanceOf(expr, cls) =>
          writeByte(TagIsInstanceOf)
          writeTree(expr); writeReferenceType(cls)

        case AsInstanceOf(expr, cls) =>
          writeByte(TagAsInstanceOf)
          writeTree(expr); writeReferenceType(cls)

        case ClassOf(cls) =>
          writeByte(TagClassOf)
          writeReferenceType(cls)

        case CallHelper(helper, args) =>
          writeByte(TagCallHelper)
          writeString(helper); writeTrees(args)
          writeType(tree.tpe)

        case JSGlobal() =>
          writeByte(TagJSGlobal)

        case JSNew(ctor, args) =>
          writeByte(TagJSNew)
          writeTree(ctor); writeTrees(args)

        case JSDotSelect(qualifier, item) =>
          writeByte(TagJSDotSelect)
          writeTree(qualifier); writeIdent(item)

        case JSBracketSelect(qualifier, item) =>
          writeByte(TagJSBracketSelect)
          writeTree(qualifier); writeTree(item)

        case JSFunctionApply(fun, args) =>
          writeByte(TagJSFunctionApply)
          writeTree(fun); writeTrees(args)

        case JSDotMethodApply(receiver, method, args) =>
          writeByte(TagJSDotMethodApply)
          writeTree(receiver); writeIdent(method); writeTrees(args)

        case JSBracketMethodApply(receiver, method, args) =>
          writeByte(TagJSBracketMethodApply)
          writeTree(receiver); writeTree(method); writeTrees(args)

        case JSApply(fun, args) =>
          writeByte(TagJSApply)
          writeTree(fun); writeTrees(args)

        case JSDelete(obj, prop) =>
          writeByte(TagJSDelete)
          writeTree(obj); writeTree(prop)

        case JSUnaryOp(op, lhs) =>
          writeByte(TagJSUnaryOp)
          writeString(op); writeTree(lhs)

        case JSBinaryOp(op, lhs, rhs) =>
          writeByte(TagJSBinaryOp)
          writeString(op); writeTree(lhs); writeTree(rhs)

        case JSArrayConstr(items) =>
          writeByte(TagJSArrayConstr)
          writeTrees(items)

        case JSObjectConstr(fields) =>
          writeByte(TagJSObjectConstr)
          writeInt(fields.size)
          fields foreach { field =>
            writePropertyName(field._1); writeTree(field._2)
          }

        // Literals

        case Undefined() =>
          writeByte(TagUndefined)

        case UndefinedParam() =>
          writeByte(TagUndefinedParam)
          writeType(tree.tpe)

        case Null() =>
          writeByte(TagNull)

        case BooleanLiteral(value) =>
          writeByte(TagBooleanLiteral)
          writeBoolean(value)

        case IntLiteral(value) =>
          writeByte(TagIntLiteral)
          writeInt(value)

        case DoubleLiteral(value) =>
          writeByte(TagDoubleLiteral)
          writeDouble(value)

        case StringLiteral(value, originalName) =>
          writeByte(TagStringLiteral)
          writeString(value); writeString(originalName.getOrElse(""))

        case VarRef(ident, mutable) =>
          writeByte(TagVarRef)
          writeIdent(ident); writeBoolean(mutable)
          writeType(tree.tpe)

        case This() =>
          writeByte(TagThis)
          writeType(tree.tpe)

        case Closure(thisType, args, resultType, body, captures) =>
          writeByte(TagClosure)
          writeType(thisType); writeTrees(args); writeType(resultType); writeTree(body)
          writeTrees(captures)

        case Function(thisType, args, resultType, body) =>
          writeByte(TagFunction)
          writeType(thisType); writeTrees(args); writeType(resultType); writeTree(body)

        case Cast(expr, tpe) =>
          writeByte(TagCast)
          writeTree(expr); writeType(tpe)

        case ClassDef(name, kind, parent, ancestors, defs) =>
          writeByte(TagClassDef)
          writeIdent(name)
          writeByte(ClassKind.toByte(kind))
          writeOptIdent(parent)
          writeIdents(ancestors)
          writeTrees(defs)

        case MethodDef(name, args, resultType, body) =>
          writeByte(TagMethodDef)
          writePropertyName(name); writeTrees(args); writeType(resultType); writeTree(body)

        case PropertyDef(name, getter, arg, setter) =>
          writeByte(TagPropertyDef)
          writePropertyName(name); writeTree(getter); writeTree(arg); writeTree(setter)

        case ConstructorExportDef(fullName, args, body) =>
          writeByte(TagConstructorExportDef)
          writeString(fullName); writeTrees(args); writeTree(body)

        case ModuleExportDef(fullName) =>
          writeByte(TagModuleExportDef)
          writeString(fullName)
      }
      if (UseDebugMagic)
        writeInt(DebugMagic)
    }

    def writeTrees(trees: List[Tree]): Unit = {
      buffer.writeInt(trees.size)
      trees.foreach(writeTree)
    }

    def writeIdent(ident: Ident): Unit = {
      writePosition(ident.pos)
      writeString(ident.name); writeString(ident.originalName.getOrElse(""))
    }

    def writeIdents(idents: List[Ident]): Unit = {
      buffer.writeInt(idents.size)
      idents.foreach(writeIdent)
    }

    def writeOptIdent(optIdent: Option[Ident]): Unit = {
      buffer.writeBoolean(optIdent.isDefined)
      optIdent.foreach(writeIdent)
    }

    def writeType(tpe: Type): Unit = {
      tpe match {
        case AnyType     => buffer.write(TagAnyType)
        case NothingType => buffer.write(TagNothingType)
        case UndefType   => buffer.write(TagUndefType)
        case BooleanType => buffer.write(TagBooleanType)
        case IntType     => buffer.write(TagIntType)
        case DoubleType  => buffer.write(TagDoubleType)
        case StringType  => buffer.write(TagStringType)
        case NullType    => buffer.write(TagNullType)
        case DynType     => buffer.write(TagDynType)
        case NoType      => buffer.write(TagNoType)

        case tpe: ClassType =>
          buffer.write(TagClassType)
          writeClassType(tpe)

        case tpe: ArrayType =>
          buffer.write(TagArrayType)
          writeArrayType(tpe)

        case RecordType(fields) =>
          buffer.write(TagRecordType)
          buffer.writeInt(fields.size)
          for (RecordType.Field(name, originalName, tpe, mutable) <- fields) {
            writeString(name)
            writeString(originalName.getOrElse(""))
            writeType(tpe)
            buffer.writeBoolean(mutable)
          }
      }
    }

    def writeClassType(tpe: ClassType): Unit =
      writeString(tpe.className)

    def writeArrayType(tpe: ArrayType): Unit = {
      writeString(tpe.baseClassName)
      buffer.writeInt(tpe.dimensions)
    }

    def writeReferenceType(tpe: ReferenceType): Unit =
      writeType(tpe)

    def writePropertyName(name: PropertyName): Unit = {
      name match {
        case name: Ident         => buffer.writeBoolean(true); writeIdent(name)
        case name: StringLiteral => buffer.writeBoolean(false); writeTree(name)
      }
    }

    def writePosition(pos: Position): Unit = {
      import buffer._
      import PositionFormat._

      def writeFull(): Unit = {
        writeByte(FormatFullMaskValue)
        writeInt(fileToIndex(pos.source))
        writeInt(pos.line)
        writeInt(pos.column)
      }

      if (pos == Position.NoPosition) {
        writeByte(FormatNoPositionValue)
      } else if (lastPosition == Position.NoPosition ||
          pos.source != lastPosition.source) {
        writeFull()
        lastPosition = pos
      } else {
        val line = pos.line
        val column = pos.column
        val lineDiff = line - lastPosition.line
        val columnDiff = column - lastPosition.column
        val columnIsByte = column >= 0 && column < 256

        if (lineDiff == 0 && columnDiff >= -64 && columnDiff < 64) {
          writeByte((columnDiff << Format1Shift) | Format1MaskValue)
        } else if (lineDiff >= -32 && lineDiff < 32 && columnIsByte) {
          writeByte((lineDiff << Format2Shift) | Format2MaskValue)
          writeByte(column)
        } else if (lineDiff >= Short.MinValue && lineDiff <= Short.MaxValue && columnIsByte) {
          writeByte(Format3MaskValue)
          writeShort(lineDiff)
          writeByte(column)
        } else {
          writeFull()
        }

        lastPosition = pos
      }

      if (UseDebugMagic)
        writeInt(PosDebugMagic)
    }

    def writeString(s: String): Unit =
      buffer.writeInt(stringToIndex(s))
  }

  private final class Deserializer(stream: InputStream, sourceVersion: String) {
    private[this] val useHacks050 = sourceVersion == "0.5.0"
    private[this] val useHacks052 = useHacks050 || sourceVersion == "0.5.2"

    private[this] val input = new DataInputStream(stream)

    private[this] val files =
      Array.fill(input.readInt())(new URI(input.readUTF()))

    private[this] val strings =
      Array.fill(input.readInt())(input.readUTF())

    private[this] var lastPosition: Position = Position.NoPosition

    def deserialize(): Tree = {
      val result = readTree()
      if (useHacks050)
        new FixClosuresTransformer().transformStat(result)
      else
        result
    }

    def readTree(): Tree = {
      import input._
      implicit val pos = readPosition()
      val tag = readByte()
      val result = (tag: @switch) match {
        case TagEmptyTree => EmptyTree

        case TagDocComment => DocComment(readString())
        case TagVarDef     => VarDef(readIdent(), readType(), readBoolean(), readTree())
        case TagParamDef   => ParamDef(readIdent(), readType(), readBoolean())

        case TagParamDefOld if useHacks052 =>
          ParamDef(readIdent(), readType(), false)

        case TagSkip     => Skip()
        case TagBlock    => Block(readTrees())
        case TagLabeled  => Labeled(readIdent(), readType(), readTree())
        case TagAssign   => Assign(readTree(), readTree())
        case TagReturn   => Return(readTree(), readOptIdent())
        case TagIf       => If(readTree(), readTree(), readTree())(readType())
        case TagWhile    => While(readTree(), readTree(), readOptIdent())
        case TagDoWhile  => DoWhile(readTree(), readTree(), readOptIdent())
        case TagTry      => Try(readTree(), readIdent(), readTree(), readTree())(readType())
        case TagThrow    => Throw(readTree())
        case TagBreak    => Break(readOptIdent())
        case TagContinue => Continue(readOptIdent())
        case TagSwitch   =>
          Switch(readTree(), List.fill(readInt()) {
            (readTree(), readTree())
          }, readTree())
        case TagMatch    =>
          Match(readTree(), List.fill(readInt()) {
            (readTrees(), readTree())
          }, readTree())(readType())
        case TagDebugger => Debugger()

        case TagNew            => New(readClassType(), readIdent(), readTrees())
        case TagLoadModule     => LoadModule(readClassType())
        case TagStoreModule    => StoreModule(readClassType(), readTree())
        case TagSelect         => Select(readTree(), readIdent(), readBoolean())(readType())
        case TagApply          => Apply(readTree(), readIdent(), readTrees())(readType())
        case TagStaticApply    => StaticApply(readTree(), readClassType(), readIdent(), readTrees())(readType())
        case TagTraitImplApply => TraitImplApply(readClassType(), readIdent(), readTrees())(readType())
        case TagUnaryOp        => UnaryOp(readByte(), readTree())
        case TagBinaryOp       => BinaryOp(readByte(), readTree(), readTree())
        case TagNewArray       => NewArray(readArrayType(), readTrees())
        case TagArrayValue     => ArrayValue(readArrayType(), readTrees())
        case TagArrayLength    => ArrayLength(readTree())
        case TagArraySelect    => ArraySelect(readTree(), readTree())(readType())
        case TagRecordValue    => RecordValue(readType().asInstanceOf[RecordType], readTrees())
        case TagIsInstanceOf   => IsInstanceOf(readTree(), readReferenceType())
        case TagAsInstanceOf   => AsInstanceOf(readTree(), readReferenceType())
        case TagClassOf        => ClassOf(readReferenceType())
        case TagCallHelper     => CallHelper(readString(), readTrees())(readType())

        case TagJSGlobal             => JSGlobal()
        case TagJSNew                => JSNew(readTree(), readTrees())
        case TagJSDotSelect          => JSDotSelect(readTree(), readIdent())
        case TagJSBracketSelect      => JSBracketSelect(readTree(), readTree())
        case TagJSFunctionApply      => JSFunctionApply(readTree(), readTrees())
        case TagJSDotMethodApply     => JSDotMethodApply(readTree(), readIdent(), readTrees())
        case TagJSBracketMethodApply => JSBracketMethodApply(readTree(), readTree(), readTrees())
        case TagJSDelete             => JSDelete(readTree(), readTree())
        case TagJSUnaryOp            => JSUnaryOp(readString(), readTree())
        case TagJSBinaryOp           => JSBinaryOp(readString(), readTree(), readTree())
        case TagJSArrayConstr        => JSArrayConstr(readTrees())
        case TagJSObjectConstr       =>
          JSObjectConstr(List.fill(readInt())((readPropertyName(), readTree())))

        // Deserialization-time fix for #804
        case TagJSApply =>
          val fun = readTree()
          val args = readTrees()
          if (useHacks050) {
            fun match {
              case CallHelper("protect", List(f)) => JSFunctionApply(f, args)
              case JSDotSelect(r, m)              => JSDotMethodApply(r, m, args)
              case JSBracketSelect(r, m)          => JSBracketMethodApply(r, m, args)
              case _                              => JSFunctionApply(fun, args)
            }
          } else {
            JSApply(fun, args)
          }

        case TagUndefined      => Undefined()
        case TagUndefinedParam => UndefinedParam()(readType())
        case TagNull           => Null()
        case TagBooleanLiteral => BooleanLiteral(readBoolean())
        case TagIntLiteral     => IntLiteral(readInt())
        case TagDoubleLiteral  => DoubleLiteral(readDouble())
        case TagStringLiteral  =>
          val value, originalName = readString()
          StringLiteral(value, if (originalName.isEmpty) None else Some(originalName))
        case TagVarRef         => VarRef(readIdent(), readBoolean())(readType())
        case TagThis           => This()(readType())
        case TagClosure        =>
          Closure(readType(), readParamDefs(), readType(), readTree(), readTrees())
        case TagFunction       => Function(readType(), readParamDefs(), readType(), readTree())
        case TagCast           => Cast(readTree(), readType())

        case TagClassDef =>
          val name = readIdent()
          val kind = ClassKind.fromByte(readByte())
          val parent = readOptIdent()
          val ancestors = readIdents()
          val defs = readTrees()
          ClassDef(name, kind, parent, ancestors, defs)

        case TagMethodDef =>
          val m = MethodDef(readPropertyName(), readParamDefs(), readType(), readTree())
          if (useHacks052 && !useHacks050)
            new FixMethodDefTransformer().fixMethodDef(m)
          else
            m
        case TagPropertyDef =>
          PropertyDef(readPropertyName(), readTree(),
              readTree().asInstanceOf[ParamDef], readTree())
        case TagConstructorExportDef =>
          ConstructorExportDef(readString(), readParamDefs(), readTree())
        case TagModuleExportDef =>
          ModuleExportDef(readString())
      }
      if (UseDebugMagic) {
        val magic = readInt()
        assert(magic == DebugMagic,
            s"Bad magic after reading a ${result.getClass}!")
      }
      result
    }

    def readTrees(): List[Tree] =
      List.fill(input.readInt())(readTree())

    def readParamDefs(): List[ParamDef] =
      readTrees().map(_.asInstanceOf[ParamDef])

    def readIdent(): Ident = {
      implicit val pos = readPosition()
      val name = readString()
      val originalName = readString()
      Ident(name, if (originalName.isEmpty) None else Some(originalName))
    }

    def readIdents(): List[Ident] =
      List.fill(input.readInt())(readIdent())

    def readOptIdent(): Option[Ident] = {
      if (input.readBoolean()) Some(readIdent())
      else None
    }

    def readType(): Type = {
      val tag = input.readByte()
      (tag: @switch) match {
        case TagAnyType     => AnyType
        case TagNothingType => NothingType
        case TagUndefType   => UndefType
        case TagBooleanType => BooleanType
        case TagIntType     => IntType
        case TagDoubleType  => DoubleType
        case TagStringType  => StringType
        case TagNullType    => NullType
        case TagDynType     => DynType
        case TagNoType      => NoType

        case TagClassType => readClassType()
        case TagArrayType => readArrayType()

        case TagRecordType =>
          RecordType(List.fill(input.readInt()) {
            val name = readString()
            val originalName = readString()
            val tpe = readType()
            val mutable = input.readBoolean()
            RecordType.Field(name,
                if (originalName.isEmpty) None else Some(originalName),
                tpe, mutable)
          })
      }
    }

    def readClassType(): ClassType =
      ClassType(readString())

    def readArrayType(): ArrayType =
      ArrayType(readString(), input.readInt())

    def readReferenceType(): ReferenceType =
      readType().asInstanceOf[ReferenceType]

    def readPropertyName(): PropertyName = {
      if (input.readBoolean()) readIdent()
      else readTree().asInstanceOf[StringLiteral]
    }

    def readPosition(): Position = {
      import input._
      import PositionFormat._

      val first = readByte()

      val result = if (first == FormatNoPositionValue) {
        Position.NoPosition
      } else {
        val result = if ((first & FormatFullMask) == FormatFullMaskValue) {
          val file = files(readInt())
          val line = readInt()
          val column = readInt()
          Position(file, line, column)
        } else {
          assert(lastPosition != NoPosition,
              "Position format error: first position must be full")
          if ((first & Format1Mask) == Format1MaskValue) {
            val columnDiff = first >> Format1Shift
            Position(lastPosition.source, lastPosition.line,
                lastPosition.column + columnDiff)
          } else if ((first & Format2Mask) == Format2MaskValue) {
            val lineDiff = first >> Format2Shift
            val column = readByte() & 0xff // unsigned
            Position(lastPosition.source,
                lastPosition.line + lineDiff, column)
          } else {
            assert((first & Format3Mask) == Format3MaskValue,
                s"Position format error: first byte $first does not match any format")
            val lineDiff = readShort()
            val column = readByte() & 0xff // unsigned
            Position(lastPosition.source,
                lastPosition.line + lineDiff, column)
          }
        }
        lastPosition = result
        result
      }

      if (UseDebugMagic) {
        val magic = readInt()
        assert(magic == PosDebugMagic,
            s"Bad magic after reading position with first byte $first")
      }

      result
    }

    def readString(): String = {
      strings(input.readInt())
    }
  }

  private class FixClosuresTransformer extends Transformers.Transformer {
    import FixClosuresTransformer._

    override def transformStat(tree: Tree): Tree =
      super.transformStat(fixClosure(tree))

    override def transformExpr(tree: Tree): Tree =
      super.transformExpr(fixClosure(tree))

    private def fixClosure(tree: Tree): Tree = {
      implicit val pos = tree.pos
      tree match {
        case JSFunctionApply(
            Function(NoType, formalCaptures, DynType,
                Function(thisType, formalArgs, resultType, body)),
            actualCaptures) =>
          Closure(thisType, formalCaptures ++ formalArgs, resultType, body,
              actualCaptures)

        case JSBracketMethodApply(
            Function(thisType, formalArgs, resultType,
                body @ PatchedBody(patch,
                    result @ Apply(receiver: This, method, args))),
            StringLiteral("bind", _), List(thisActualCapture)) =>
          // The compiler never emits a variable named "$_this"
          val thisFormalCapture = ParamDef(Ident("$_this", None), thisType, false)
          val thisCaptureArg = thisFormalCapture.ref
          val (actualArgs, actualCaptures) = args.splitAt(formalArgs.size)
          val (formalCaptures, captureArgs) = makeCaptures(actualCaptures)
          Closure(thisType, thisFormalCapture :: formalCaptures ::: formalArgs, resultType,
              patch(Apply(thisCaptureArg, method,
                  actualArgs ::: captureArgs)(result.tpe)(result.pos)),
              thisActualCapture :: actualCaptures)

        case Function(NoType, formalArgs, resultType,
            body @ PatchedBody(patch,
                result @ TraitImplApply(traitImpl, method, args))) =>
          val (thisActualCapture :: actualArgs, actualCaptures) =
            args.splitAt(formalArgs.size+1)
          val (formalCaptures, thisCaptureArg :: captureArgs) =
            makeCaptures(thisActualCapture :: actualCaptures)
          Closure(NoType, formalCaptures ::: formalArgs, resultType,
              patch(TraitImplApply(traitImpl, method,
                  thisCaptureArg :: actualArgs ::: captureArgs)(result.tpe)(result.pos)),
              thisActualCapture :: actualCaptures)

        case _: Function =>
          sys.error(s"Found unrecognized function node at ${tree.pos}:\n$tree")

        case _ =>
          tree
      }
    }

    override def transformDef(tree: Tree): Tree = tree match {
      case _: MethodDef =>
        /* We are here because useHacks050 is true. In that case, useHacks052
         * is also true.
         */
        val m = super.transformDef(tree).asInstanceOf[MethodDef]
        new FixMethodDefTransformer().fixMethodDef(m)

      case _ =>
        super.transformDef(tree)
    }
  }

  private object FixClosuresTransformer {
    private object PatchedBody {
      def unapply(body: Tree): Option[(Tree => Tree, Tree)] = {
        val stats = body match {
          case Block(stats) => stats
          case stat         => stat :: Nil
        }
        val (unboxedParams0, boxedResult) = stats.span(_.isInstanceOf[VarDef])
        val unboxedParams = unboxedParams0.asInstanceOf[List[VarDef]]
        boxedResult match {
          case List(result, undef: Undefined) =>
            Some((tree => Block(unboxedParams :+
                tree :+ undef)(tree.pos), result))
          case List(helper @ CallHelper("bC", List(result))) =>
            Some((tree => Block(unboxedParams :+
                CallHelper("bC", result)(helper.tpe)(helper.pos))(tree.pos), result))
          case List(New(cls, ctor, List(result))) =>
            Some((tree => Block(unboxedParams :+
                New(cls, ctor, List(tree))(tree.pos))(tree.pos), result))
          case List(result) =>
            Some((tree => Block(unboxedParams :+
                tree)(tree.pos), result))
          case _ =>
            None
        }
      }
    }

    private def makeCaptures(actualCaptures: List[Tree]) = {
      (actualCaptures map { c => (c: @unchecked) match {
        case VarRef(ident, _) =>
          (ParamDef(ident, c.tpe, false)(c.pos), VarRef(ident, false)(c.tpe)(c.pos))
      }}).unzip
    }
  }

  private class FixMethodDefTransformer extends Transformers.Transformer {
    private var patchedVarNames: Set[String] = Set.empty

    def fixMethodDef(tree: MethodDef): MethodDef = tree.body match {
      case body if isTailRecWhileLoop(body) =>
        val MethodDef(name, params, resultType, _) = tree
        patchedVarNames = params.map(_.name.name).toSet
        val newParams = params.map(p => p.copy(mutable = true)(p.pos))
        val newBody =
          if (resultType == NoType) transformStat(body)
          else transformExpr(body)
        MethodDef(name, newParams, resultType, newBody)(tree.pos)

      case Block(List(
          thisDef @ VarDef(thisName, thisType, _, _: This),
          body)) if isTailRecWhileLoop(body) =>
        val MethodDef(name, params, resultType, wholeBody) = tree
        patchedVarNames = (thisName.name :: params.map(_.name.name)).toSet
        val newParams = params.map(p => p.copy(mutable = true)(p.pos))
        val newThisDef = thisDef.copy(mutable = true)(thisDef.pos)
        val newBody =
          if (resultType == NoType) transformStat(body)
          else transformExpr(body)
        val newWholeBody =
          Block(newThisDef, newBody)(wholeBody.pos)
        MethodDef(name, newParams, resultType, newWholeBody)(tree.pos)

      case _ =>
        tree
    }

    private def isTailRecWhileLoop(tree: Tree): Boolean = tree match {
      case While(BooleanLiteral(true), _, Some(Ident("tailCallLoop", _))) =>
        true
      case _ =>
        false
    }

    override def transformStat(tree: Tree): Tree =
      transform(tree, isStat = true)

    override def transformExpr(tree: Tree): Tree =
      transform(tree, isStat = false)

    private def transform(tree: Tree, isStat: Boolean): Tree = tree match {
      case VarRef(ident @ Ident(name, _), false) if patchedVarNames.contains(name) =>
        VarRef(ident, true)(tree.tpe)(tree.pos)

      case Closure(thisType, params, resultType, body, captures) =>
        Closure(thisType, params, resultType, body,
            captures.map(transformExpr))(tree.pos)

      case _: Function =>
        sys.error("Unexpected Function in FixMethodDefTransformer at "+
            s"${tree.pos}:\n$tree")

      case _ =>
        if (isStat) super.transformStat(tree)
        else super.transformExpr(tree)
    }
  }

  // Tags for Trees

  private final val TagEmptyTree = 1

  private final val TagDocComment = TagEmptyTree + 1
  private final val TagVarDef = TagDocComment + 1
  private final val TagParamDefOld = TagVarDef + 1

  private final val TagSkip = TagParamDefOld + 1
  private final val TagBlock = TagSkip + 1
  private final val TagLabeled = TagBlock + 1
  private final val TagAssign = TagLabeled + 1
  private final val TagReturn = TagAssign + 1
  private final val TagIf = TagReturn + 1
  private final val TagWhile = TagIf + 1
  private final val TagDoWhile = TagWhile + 1
  private final val TagTry = TagDoWhile + 1
  private final val TagThrow = TagTry + 1
  private final val TagBreak = TagThrow + 1
  private final val TagContinue = TagBreak + 1
  private final val TagSwitch = TagContinue + 1
  private final val TagMatch = TagSwitch + 1
  private final val TagDebugger = TagMatch + 1

  private final val TagNew = TagDebugger + 1
  private final val TagLoadModule = TagNew + 1
  private final val TagStoreModule = TagLoadModule + 1
  private final val TagSelect = TagStoreModule + 1
  private final val TagApply = TagSelect + 1
  private final val TagStaticApply = TagApply + 1
  private final val TagTraitImplApply = TagStaticApply + 1
  private final val TagUnaryOp = TagTraitImplApply + 1
  private final val TagBinaryOp = TagUnaryOp + 1
  private final val TagNewArray = TagBinaryOp + 1
  private final val TagArrayValue = TagNewArray + 1
  private final val TagArrayLength = TagArrayValue + 1
  private final val TagArraySelect = TagArrayLength + 1
  private final val TagIsInstanceOf = TagArraySelect + 1
  private final val TagAsInstanceOf = TagIsInstanceOf + 1
  private final val TagClassOf = TagAsInstanceOf + 1
  private final val TagCallHelper = TagClassOf + 1

  private final val TagJSGlobal = TagCallHelper + 1
  private final val TagJSNew = TagJSGlobal + 1
  private final val TagJSDotSelect = TagJSNew + 1
  private final val TagJSBracketSelect = TagJSDotSelect + 1
  private final val TagJSApply = TagJSBracketSelect + 1
  private final val TagJSDelete = TagJSApply + 1
  private final val TagJSUnaryOp = TagJSDelete + 1
  private final val TagJSBinaryOp = TagJSUnaryOp + 1
  private final val TagJSArrayConstr = TagJSBinaryOp + 1
  private final val TagJSObjectConstr = TagJSArrayConstr + 1

  private final val TagUndefined = TagJSObjectConstr + 1
  private final val TagUndefinedParam = TagUndefined + 1
  private final val TagNull = TagUndefinedParam + 1
  private final val TagBooleanLiteral = TagNull + 1
  private final val TagIntLiteral = TagBooleanLiteral + 1
  private final val TagDoubleLiteral = TagIntLiteral + 1
  private final val TagStringLiteral = TagDoubleLiteral + 1
  private final val TagVarRef = TagStringLiteral + 1
  private final val TagThis = TagVarRef + 1
  private final val TagFunction = TagThis + 1
  private final val TagCast = TagFunction + 1

  private final val TagClassDef = TagCast + 1
  private final val TagMethodDef = TagClassDef + 1
  private final val TagPropertyDef = TagMethodDef + 1
  private final val TagConstructorExportDef = TagPropertyDef + 1
  private final val TagModuleExportDef = TagConstructorExportDef + 1

  private final val TagJSFunctionApply = TagModuleExportDef + 1
  private final val TagJSDotMethodApply = TagJSFunctionApply + 1
  private final val TagJSBracketMethodApply = TagJSDotMethodApply + 1

  private final val TagClosure = TagJSBracketMethodApply + 1

  private final val TagParamDef = TagClosure + 1

  private final val TagRecordValue = TagParamDef + 1

  // Tags for Types

  private final val TagAnyType = 1
  private final val TagNothingType = TagAnyType + 1
  private final val TagUndefType = TagNothingType + 1
  private final val TagBooleanType = TagUndefType + 1
  private final val TagIntType = TagBooleanType + 1
  private final val TagDoubleType = TagIntType + 1
  private final val TagStringType = TagDoubleType + 1
  private final val TagNullType = TagStringType + 1
  private final val TagClassType = TagNullType + 1
  private final val TagArrayType = TagClassType + 1
  private final val TagDynType = TagArrayType + 1
  private final val TagNoType = TagDynType + 1
  private final val TagRecordType = TagNoType + 1
}
