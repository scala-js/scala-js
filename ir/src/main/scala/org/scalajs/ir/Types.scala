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

  /** Record type.
   *  Used by the optimizer to inline classes as records with multiple fields.
   *  They are desugared as several local variables by JSDesugaring.
   *  Record types cannot cross method boundaries, so they cannot appear as
   *  the type of fields or parameters, nor as result types of methods.
   *  The compiler itself never generates record types.
   */
  final case class RecordType(fields: List[RecordType.Field]) extends Type {
    def findField(name: FieldName): RecordType.Field =
      fields.find(_.name == name).get
  }

  object RecordType {
    final case class Field(name: FieldName, originalName: OriginalName, tpe: Type, mutable: Boolean)
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
  sealed abstract class TypeRef {
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
  final case class PrimRef private[ir] (tpe: PrimTypeWithRef) extends NonArrayTypeRef {

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

  final val VoidRef = PrimRef(NoType)
  final val BooleanRef = PrimRef(BooleanType)
  final val CharRef = PrimRef(CharType)
  final val ByteRef = PrimRef(ByteType)
  final val ShortRef = PrimRef(ShortType)
  final val IntRef = PrimRef(IntType)
  final val LongRef = PrimRef(LongType)
  final val FloatRef = PrimRef(FloatType)
  final val DoubleRef = PrimRef(DoubleType)
  final val NullRef = PrimRef(NullType)
  final val NothingRef = PrimRef(NothingType)

  /** Class (or interface) type. */
  final case class ClassRef(className: ClassName) extends NonArrayTypeRef {
    def displayName: String = className.nameString
  }

  /** Array type. */
  final case class ArrayTypeRef(base: NonArrayTypeRef, dimensions: Int) extends TypeRef {

    def displayName: String = "[" * dimensions + base.displayName
  }

  object ArrayTypeRef {
    def of(innerType: TypeRef): ArrayTypeRef = innerType match {
      case innerType: NonArrayTypeRef => ArrayTypeRef(innerType, 1)
      case ArrayTypeRef(base, dim)    => ArrayTypeRef(base, dim + 1)
    }
  }

  /** Generates a literal zero of the given type. */
  def zeroOf(tpe: Type)(implicit pos: Position): Literal = tpe match {
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
    case _           => Null()
  }

  /** Tests whether a type `lhs` is a subtype of `rhs` (or equal).
   *  [[NoType]] is never a subtype or supertype of anything (including
   *  itself). All other types are subtypes of themselves.
   *  @param isSubclass A function testing whether a class/interface is a
   *                    subclass of another class/interface.
   */
  def isSubtype(lhs: Type, rhs: Type)(isSubclass: (ClassName, ClassName) => Boolean): Boolean = {

    (lhs != NoType && rhs != NoType) && {
      (lhs == rhs) ||
      ((lhs, rhs) match {
        case (_, AnyType)     => true
        case (NothingType, _) => true

        case (ClassType(lhsClass), ClassType(rhsClass)) =>
          isSubclass(lhsClass, rhsClass)

        case (NullType, ClassType(_)) => true
        case (NullType, ArrayType(_)) => true

        case (UndefType, ClassType(className)) =>
          isSubclass(BoxedUnitClass, className)
        case (BooleanType, ClassType(className)) =>
          isSubclass(BoxedBooleanClass, className)
        case (CharType, ClassType(className)) =>
          isSubclass(BoxedCharacterClass, className)
        case (ByteType, ClassType(className)) =>
          isSubclass(BoxedByteClass, className)
        case (ShortType, ClassType(className)) =>
          isSubclass(BoxedShortClass, className)
        case (IntType, ClassType(className)) =>
          isSubclass(BoxedIntegerClass, className)
        case (LongType, ClassType(className)) =>
          isSubclass(BoxedLongClass, className)
        case (FloatType, ClassType(className)) =>
          isSubclass(BoxedFloatClass, className)
        case (DoubleType, ClassType(className)) =>
          isSubclass(BoxedDoubleClass, className)
        case (StringType, ClassType(className)) =>
          isSubclass(BoxedStringClass, className)

        case (ArrayType(ArrayTypeRef(lhsBase, lhsDims)), ArrayType(ArrayTypeRef(rhsBase,
                        rhsDims))) =>
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
}
