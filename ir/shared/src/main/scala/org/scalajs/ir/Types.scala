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

import scala.annotation.tailrec

import Names._
import Trees._

object Types {

  /** Type of a term (expression or statement) in the IR.
   *
   *  There is a many-to-one relationship from [[TypeRef]]s to `Type`s,
   *  because `java.lang.Object` and JS types all collapse to [[AnyType]].
   *
   *  In fact, there are two `Type`s that do not have any real equivalent in
   *  type refs: [[StringType]] and [[UndefType]], as they refer to the
   *  non-null variants of `java.lang.String` and `java.lang.Void`,
   *  respectively.
   */
  abstract sealed class Type {
    def show(): String = {
      val writer = new java.io.StringWriter
      val printer = new Printers.IRTreePrinter(writer)
      printer.print(this)
      writer.toString()
    }
  }

  sealed abstract class PrimType extends Type

  sealed abstract class PrimTypeWithRef extends PrimType {
    def primRef: PrimRef = this match {
      case NoType      => VoidRef
      case BooleanType => BooleanRef
      case CharType    => CharRef
      case ByteType    => ByteRef
      case ShortType   => ShortRef
      case IntType     => IntRef
      case LongType    => LongRef
      case FloatType   => FloatRef
      case DoubleType  => DoubleRef
      case NullType    => NullRef
      case NothingType => NothingRef
    }
  }

  /** Any type (the top type of this type system).
   *  A variable of this type can contain any value, including `undefined`
   *  and `null` and any JS value. This type supports a very limited set
   *  of Scala operations, the ones common to all values. Basically only
   *  reference equality tests and instance tests. It also supports all
   *  JavaScript operations, since all Scala objects are also genuine
   *  JavaScript objects.
   *  The type java.lang.Object in the back-end maps to [[AnyType]] because it
   *  can hold JS values (not only instances of Scala.js classes).
   */
  case object AnyType extends Type

  // Can't link to Nothing - #1969
  /** Nothing type (the bottom type of this type system).
   *  Expressions from which one can never come back are typed as `Nothing`.
   *  For example, `throw` and `return`.
   */
  case object NothingType extends PrimTypeWithRef

  /** The type of `undefined`. */
  case object UndefType extends PrimType

  /** Boolean type.
   *  It does not accept `null` nor `undefined`.
   */
  case object BooleanType extends PrimTypeWithRef

  /** `Char` type, a 16-bit UTF-16 code unit.
   *  It does not accept `null` nor `undefined`.
   */
  case object CharType extends PrimTypeWithRef

  /** 8-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object ByteType extends PrimTypeWithRef

  /** 16-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object ShortType extends PrimTypeWithRef

  /** 32-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object IntType extends PrimTypeWithRef

  /** 64-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object LongType extends PrimTypeWithRef

  /** Float type (32-bit).
   *  It does not accept `null` nor `undefined`.
   */
  case object FloatType extends PrimTypeWithRef

  /** Double type (64-bit).
   *  It does not accept `null` nor `undefined`.
   */
  case object DoubleType extends PrimTypeWithRef

  /** String type.
   *  It does not accept `null` nor `undefined`.
   */
  case object StringType extends PrimType

  /** The type of `null`.
   *  It does not accept `undefined`.
   *  The null type is a subtype of all class types and array types.
   */
  case object NullType extends PrimTypeWithRef

  /** Class (or interface) type. */
  final case class ClassType(className: ClassName) extends Type

  /** Array type. */
  final case class ArrayType(arrayTypeRef: ArrayTypeRef) extends Type

  /** Closure type.
   *
   *  This is the type of a `TypedClosure`. Parameters and result are
   *  statically typed according to the `closureTypeRef` components.
   *
   *  Closure types are nullable. `Null()` is a valid value of a closure type.
   *  This is unfortunately required for a `ClosureType` to have a default
   *  value, which in turn is required to be used as the type of a field.
   *
   *  Closure types are non-variant in both parameter and result types.
   *
   *  Closure types are *not* subtypes of `AnyType`. That statically prevents
   *  them from going into JavaScript code or in any other universal context.
   *  They do not support type tests nor casts.
   *
   *  The following subtyping relationships hold for any closure type `CT`:
   *  {{{
   *  nothing <: null <: CT <: void
   *  }}}
   */
  final case class ClosureType(paramTypes: List[Type], resultType: Type) extends Type

  /** Record type.
   *  Used by the optimizer to inline classes as records with multiple fields.
   *  They are desugared as several local variables by JSDesugaring.
   *  Record types cannot cross method boundaries, so they cannot appear as
   *  the type of fields or parameters, nor as result types of methods.
   *  The compiler itself never generates record types.
   */
  final case class RecordType(fields: List[RecordType.Field]) extends Type {
    def findField(name: SimpleFieldName): RecordType.Field =
      fields.find(_.name == name).get
  }

  object RecordType {
    final case class Field(name: SimpleFieldName, originalName: OriginalName,
        tpe: Type, mutable: Boolean)
  }

  /** No type. */
  case object NoType extends PrimTypeWithRef

  /** Type reference (allowed for classOf[], is/asInstanceOf[]).
   *
   *  A `TypeRef` has exactly the same level of precision as a JVM type.
   *  There is a one-to-one relationship between a `TypeRef` and an instance of
   *  `java.lang.Class` at run-time. This means that:
   *
   *  - All primitive types have their `TypeRef` (including `scala.Byte` and
   *    `scala.Short`), and they are different from their boxed versions.
   *  - JS types are not erased to `any`
   *  - Array types are like on the JVM
   *
   *  A `TypeRef` therefore uniquely identifies a `classOf[T]`. It is also the
   *  type refs that are used in method signatures, and which therefore dictate
   *  JVM/IR overloading.
   */
  sealed abstract class TypeRef extends Comparable[TypeRef] {
    final def compareTo(that: TypeRef): Int = this match {
      case thiz: PrimRef =>
        that match {
          case that: PrimRef => Character.compare(thiz.charCode, that.charCode)
          case _             => -1
        }
      case thiz: ClassRef =>
        that match {
          case that: ClassRef => thiz.className.compareTo(that.className)
          case _: PrimRef     => 1
          case _              => -1
        }
      case thiz: ArrayTypeRef =>
        that match {
          case that: ArrayTypeRef =>
            if (thiz.dimensions != that.dimensions) thiz.dimensions - that.dimensions
            else thiz.base.compareTo(that.base)
          case _: ClosureTypeRef =>
            -1
          case _ =>
            1
        }
      case thiz: ClosureTypeRef =>
        that match {
          case that: ClosureTypeRef =>
            import Ordering.Implicits._
            implicit val typeRefOrdering: Ordering[TypeRef] =
              Ordering.comparatorToOrdering(java.util.Comparator.naturalOrder())
            val cmp = implicitly[Ordering[List[TypeRef]]]
              .compare(thiz.paramTypeRefs, that.paramTypeRefs)
            if (cmp != 0) cmp
            else thiz.resultTypeRef.compareTo(that.resultTypeRef)
          case _ =>
            1
        }
    }

    def show(): String = {
      val writer = new java.io.StringWriter
      val printer = new Printers.IRTreePrinter(writer)
      printer.print(this)
      writer.toString()
    }

    def displayName: String
  }

  sealed abstract class NonArrayTypeRef extends TypeRef

  /** Primitive type reference. */
  final case class PrimRef private[ir] (tpe: PrimTypeWithRef)
      extends NonArrayTypeRef {

    /** The display name of this primitive type.
     *
     *  For all primitive types except `NullType` and `NothingType`, this is
     *  specified by the IR spec in the sense that it is the result of
     *  `classOf[Prim].getName()`.
     *
     *  For `NullType` and `NothingType`, the names are `"null"` and
     *  `"nothing"`, respectively.
     */
    val displayName: String = tpe match {
      case NoType      => "void"
      case BooleanType => "boolean"
      case CharType    => "char"
      case ByteType    => "byte"
      case ShortType   => "short"
      case IntType     => "int"
      case LongType    => "long"
      case FloatType   => "float"
      case DoubleType  => "double"
      case NullType    => "null"
      case NothingType => "nothing"
    }

    /** The char code of this primitive type.
     *
     *  For all primitive types except `NullType` and `NothingType`, this is
     *  specified by the IR spec in the sense that it is visible in the result
     *  of `classOf[Array[Prim]].getName()` (e.g., that is `"[I"` for
     *  `Array[Int]`).
     *
     *  For `NullType` and `NothingType`, the char codes are `'N'` and `'E'`,
     *  respectively.
     */
    val charCode: Char = tpe match {
      case NoType      => 'V'
      case BooleanType => 'Z'
      case CharType    => 'C'
      case ByteType    => 'B'
      case ShortType   => 'S'
      case IntType     => 'I'
      case LongType    => 'J'
      case FloatType   => 'F'
      case DoubleType  => 'D'
      case NullType    => 'N'
      case NothingType => 'E'
    }
  }

  /* @unchecked for the initialization checker of Scala 3
   * When we get here, `NoType` is not yet considered fully initialized because
   * its method `primRef` can access `VoidRef`. Since the constructor of
   * `PrimRef` pattern-matches on its `tpe`, which is `NoType`, this is flagged
   * by the init checker, although our usage is safe given that we do not call
   * `primRef`. The same reasoning applies to the other primitive types.
   * In the future, we may want to rearrange the initialization sequence of
   * this file to avoid this issue.
   */
  final val VoidRef = PrimRef(NoType: @unchecked)
  final val BooleanRef = PrimRef(BooleanType: @unchecked)
  final val CharRef = PrimRef(CharType: @unchecked)
  final val ByteRef = PrimRef(ByteType: @unchecked)
  final val ShortRef = PrimRef(ShortType: @unchecked)
  final val IntRef = PrimRef(IntType: @unchecked)
  final val LongRef = PrimRef(LongType: @unchecked)
  final val FloatRef = PrimRef(FloatType: @unchecked)
  final val DoubleRef = PrimRef(DoubleType: @unchecked)
  final val NullRef = PrimRef(NullType: @unchecked)
  final val NothingRef = PrimRef(NothingType: @unchecked)

  /** Class (or interface) type. */
  final case class ClassRef(className: ClassName) extends NonArrayTypeRef {
    def displayName: String = className.nameString
  }

  /** Array type. */
  final case class ArrayTypeRef(base: NonArrayTypeRef, dimensions: Int)
      extends TypeRef {

    def displayName: String = "[" * dimensions + base.displayName
  }

  object ArrayTypeRef {
    def of(innerType: TypeRef): ArrayTypeRef = innerType match {
      case innerType: NonArrayTypeRef => ArrayTypeRef(innerType, 1)
      case ArrayTypeRef(base, dim)    => ArrayTypeRef(base, dim + 1)
      case innerType: ClosureTypeRef  => throw new IllegalArgumentException(innerType.toString())
    }
  }

  /** Closure type. */
  final case class ClosureTypeRef(paramTypeRefs: List[TypeRef], resultTypeRef: TypeRef)
      extends TypeRef {

    def displayName: String = {
      paramTypeRefs.map(_.displayName)
        .mkString("(", ",", ")" + resultTypeRef.displayName)
    }
  }

  /** Generates a literal zero of the given type. */
  def zeroOf(tpe: Type)(implicit pos: Position): Tree = tpe match {
    case BooleanType => BooleanLiteral(false)
    case CharType    => CharLiteral('\u0000')
    case ByteType    => ByteLiteral(0)
    case ShortType   => ShortLiteral(0)
    case IntType     => IntLiteral(0)
    case LongType    => LongLiteral(0L)
    case FloatType   => FloatLiteral(0.0f)
    case DoubleType  => DoubleLiteral(0.0)
    case StringType  => StringLiteral("")
    case UndefType   => Undefined()

    case NullType | AnyType | _:ClassType | _:ArrayType | _:ClosureType => Null()

    case tpe: RecordType =>
      RecordValue(tpe, tpe.fields.map(f => zeroOf(f.tpe)))

    case NothingType | NoType =>
      throw new IllegalArgumentException(s"cannot generate a zero for $tpe")
  }

  val BoxedClassToPrimType: Map[ClassName, PrimType] = Map(
    BoxedUnitClass -> UndefType,
    BoxedBooleanClass -> BooleanType,
    BoxedCharacterClass -> CharType,
    BoxedByteClass -> ByteType,
    BoxedShortClass -> ShortType,
    BoxedIntegerClass -> IntType,
    BoxedLongClass -> LongType,
    BoxedFloatClass -> FloatType,
    BoxedDoubleClass -> DoubleType,
    BoxedStringClass -> StringType
  )

  val PrimTypeToBoxedClass: Map[PrimType, ClassName] =
    BoxedClassToPrimType.map(_.swap)

  /** Tests whether a type `lhs` is a subtype of `rhs` (or equal).
   *  @param isSubclass A function testing whether a class/interface is a
   *                    subclass of another class/interface.
   */
  def isSubtype(lhs: Type, rhs: Type)(
      isSubclass: (ClassName, ClassName) => Boolean): Boolean = {
    (lhs == rhs) ||
    ((lhs, rhs) match {
      // nothing and void are sub- and supertype of all types, including ClosureType's
      case (NothingType, _) => true
      case (_, NoType)      => true

      // any is supertype of all types, except void and closure types
      case (_, AnyType) => lhs != NoType && !lhs.isInstanceOf[ClosureType]

      case (ClassType(lhsClass), ClassType(rhsClass)) =>
        isSubclass(lhsClass, rhsClass)

      case (NullType, _:ClassType | _:ArrayType | _:ClosureType) => true

      case (primType: PrimType, ClassType(rhsClass)) =>
        val lhsClass = PrimTypeToBoxedClass.getOrElse(primType, {
          throw new AssertionError(s"unreachable case for isSubtype($lhs, $rhs)")
        })
        isSubclass(lhsClass, rhsClass)

      case (ArrayType(ArrayTypeRef(lhsBase, lhsDims)),
          ArrayType(ArrayTypeRef(rhsBase, rhsDims))) =>
        if (lhsDims < rhsDims) {
          false // because Array[A] </: Array[Array[A]]
        } else if (lhsDims > rhsDims) {
          rhsBase match {
            case ClassRef(ObjectClass) =>
              true // because Array[Array[A]] <: Array[Object]
            case _ =>
              false
          }
        } else { // lhsDims == rhsDims
          // lhsBase must be <: rhsBase
          (lhsBase, rhsBase) match {
            case (ClassRef(lhsBaseName), ClassRef(rhsBaseName)) =>
              /* All things must be considered subclasses of Object for this
               * purpose, even JS types and interfaces, which do not have
               * Object in their ancestors.
               */
              rhsBaseName == ObjectClass || isSubclass(lhsBaseName, rhsBaseName)
            case _ =>
              lhsBase eq rhsBase
          }
        }

      case (ArrayType(_), ClassType(className)) =>
        AncestorsOfPseudoArrayClass.contains(className)

      case _ =>
        false
    })
  }
}
