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

import Trees._

object Types {

  private val AncestorsOfPseudoArrayClass =
    Set(Definitions.ObjectClass, "Ljava_io_Serializable", "jl_Cloneable")

  /** Type of a term (expression or statement) in the IR.
   *
   *  There is a many-to-one relationship from [[TypeRef]]s to `Type`s,
   *  because `java.lang.Object` and raw JS types all collapse to [[AnyType]].
   *
   *  In fact, there are two `Type`s that do not have any real equivalent in
   *  type refs: [[StringType]] and [[UndefType]], as they refer to the
   *  non-null variants of `java.lang.String` and `scala.runtime.BoxedUnit`,
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

  /** Any type (the top type of this type system).
   *  A variable of this type can contain any value, including `undefined`
   *  and `null` and any raw JS value. This type supports a very limited set
   *  of Scala operations, the ones common to all values. Basically only
   *  reference equality tests and instance tests. It also supports all
   *  JavaScript operations, since all Scala objects are also genuine
   *  JavaScript objects.
   *  The type java.lang.Object in the back-end maps to [[AnyType]] because it
   *  can hold raw JS values (not only instances of Scala.js classes).
   */
  case object AnyType extends Type

  // Can't link to Nothing - #1969
  /** Nothing type (the bottom type of this type system).
   *  Expressions from which one can never come back are typed as `Nothing`.
   *  For example, `throw` and `return`.
   */
  case object NothingType extends Type

  /** The type of `undefined`. */
  case object UndefType extends Type

  /** Boolean type.
   *  It does not accept `null` nor `undefined`.
   */
  case object BooleanType extends Type

  /** `Char` type, a 16-bit UTF-16 code unit.
   *  It does not accept `null` nor `undefined`.
   */
  case object CharType extends Type

  /** 8-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object ByteType extends Type

  /** 16-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object ShortType extends Type

  /** 32-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object IntType extends Type

  /** 64-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object LongType extends Type

  /** Float type (32-bit).
   *  It does not accept `null` nor `undefined`.
   */
  case object FloatType extends Type

  /** Double type (64-bit).
   *  It does not accept `null` nor `undefined`.
   */
  case object DoubleType extends Type

  /** String type.
   *  It does not accept `null` nor `undefined`.
   */
  case object StringType extends Type

  /** The type of `null`.
   *  It does not accept `undefined`.
   *  The null type is a subtype of all class types and array types.
   */
  case object NullType extends Type

  /** Class (or interface) type. */
  final case class ClassType(className: String) extends Type

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
    def findField(name: String): RecordType.Field =
      fields.find(_.name == name).get
  }

  object RecordType {
    final case class Field(name: String, originalName: Option[String],
        tpe: Type, mutable: Boolean)
  }

  /** No type. */
  case object NoType extends Type

  /** Type reference (allowed for classOf[], is/asInstanceOf[]).
   *
   *  A `TypeRef` has exactly the same level of precision as a JVM type.
   *  There is a one-to-one relationship between a `TypeRef` and an instance of
   *  `java.lang.Class` at run-time. This means that:
   *
   *  - All primitive types have their `TypeRef` (including `scala.Byte` and
   *    `scala.Short`), and they are different from their boxed versions.
   *  - Raw JS types are not erased to `any`
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
  }

  /** Class (or interface) type. */
  final case class ClassRef(className: String) extends TypeRef

  /** Array type. */
  final case class ArrayTypeRef(baseClassName: String, dimensions: Int)
      extends TypeRef

  object ArrayTypeRef {
    def of(innerType: TypeRef): ArrayTypeRef = innerType match {
      case ClassRef(className)          => ArrayTypeRef(className, 1)
      case ArrayTypeRef(className, dim) => ArrayTypeRef(className, dim + 1)
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
  def isSubtype(lhs: Type, rhs: Type)(
      isSubclass: (String, String) => Boolean): Boolean = {
    import Definitions._

    (lhs != NoType && rhs != NoType) && {
      (lhs == rhs) ||
      ((lhs, rhs) match {
        case (_, AnyType)     => true
        case (NothingType, _) => true

        case (ClassType(lhsClass), ClassType(rhsClass)) =>
          isSubclass(lhsClass, rhsClass)

        case (NullType, ClassType(_)) => true
        case (NullType, ArrayType(_)) => true

        case (UndefType, ClassType(cls)) =>
          isSubclass(BoxedUnitClass, cls)
        case (BooleanType, ClassType(cls)) =>
          isSubclass(BoxedBooleanClass, cls)
        case (CharType, ClassType(cls)) =>
          isSubclass(BoxedCharacterClass, cls)
        case (ByteType, ClassType(cls)) =>
          isSubclass(BoxedByteClass, cls)
        case (ShortType, ClassType(cls)) =>
          isSubclass(BoxedShortClass, cls)
        case (IntType, ClassType(cls)) =>
          isSubclass(BoxedIntegerClass, cls)
        case (LongType, ClassType(cls)) =>
          isSubclass(BoxedLongClass, cls)
        case (FloatType, ClassType(cls)) =>
          isSubclass(BoxedFloatClass, cls)
        case (DoubleType, ClassType(cls)) =>
          isSubclass(BoxedDoubleClass, cls)
        case (StringType, ClassType(cls)) =>
          isSubclass(BoxedStringClass, cls)

        case (ArrayType(ArrayTypeRef(lhsBase, lhsDims)),
            ArrayType(ArrayTypeRef(rhsBase, rhsDims))) =>
          if (lhsDims < rhsDims) {
            false // because Array[A] </: Array[Array[A]]
          } else if (lhsDims > rhsDims) {
            rhsBase == ObjectClass // because Array[Array[A]] <: Array[Object]
          } else { // lhsDims == rhsDims
            // lhsBase must be <: rhsBase
            if (PrimitiveClasses(lhsBase) || PrimitiveClasses(rhsBase)) {
              lhsBase == rhsBase
            } else {
              /* All things must be considered subclasses of Object for this
               * purpose, even raw JS types and interfaces, which do not have
               * Object in their ancestors.
               */
              rhsBase == ObjectClass || isSubclass(lhsBase, rhsBase)
            }
          }

        case (ArrayType(_), ClassType(cls)) =>
          AncestorsOfPseudoArrayClass.contains(cls)

        case _ =>
          false
      })
    }
  }
}
