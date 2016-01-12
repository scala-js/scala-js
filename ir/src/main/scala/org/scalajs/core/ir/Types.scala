/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js IR                **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.ir

import scala.annotation.tailrec

import Trees._

object Types {

  /** Type of an term (expression or statement) in the IR.
   *
   *  There is a many-to-one relationship from [[ReferenceType]]s to types,
   *  because:
   *
   *  - `scala.Byte`, `scala.Short` and `scala.Int` collapse to [[IntType]]
   *  - `java.lang.Object` and raw JS types all collapse to [[AnyType]]
   *
   *  In fact, there are two `Type`s that do not have any real equivalent in
   *  reference types: [[StringType]] and [[UndefType]], as they refer to the
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

  /** Reference types (allowed for classOf[], is/asInstanceOf[]).
   *
   *  A `ReferenceType` has exactly the same level of precision as a JVM type.
   *  There is a one-to-one relationship between a `ReferenceType` and an
   *  instance of `java.lang.Class` at run-time. This means that:
   *
   *  - All primitive types have their reference type (including `scala.Byte`
   *    and `scala.Short`), and they are different from their boxed versions.
   *  - Raw JS types are not erased to `any`
   *  - Array types are like on the JVM
   *
   *  A `ReferenceType` therefore uniquely identifies a `classOf[T]`. It is
   *  also the reference types that are used in method signatures, and which
   *  therefore dictate JVM/IR overloading.
   */
  sealed trait ReferenceType

  /** Class (or interface) type. */
  final case class ClassType(className: String) extends Type with ReferenceType

  /** Array type. */
  final case class ArrayType(baseClassName: String, dimensions: Int)
      extends Type with ReferenceType

  object ArrayType {
    def apply(innerType: ReferenceType): ArrayType = innerType match {
      case ClassType(className)      => ArrayType(className, 1)
      case ArrayType(className, dim) => ArrayType(className, dim + 1)
    }
  }

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

  /** Generates a literal zero of the given type. */
  def zeroOf(tpe: Type)(implicit pos: Position): Literal = tpe match {
    case BooleanType => BooleanLiteral(false)
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

        case (NullType, ClassType(_))    => true
        case (NullType, ArrayType(_, _)) => true

        case (UndefType, ClassType(cls)) =>
          isSubclass(BoxedUnitClass, cls)
        case (BooleanType, ClassType(cls)) =>
          isSubclass(BoxedBooleanClass, cls)
        case (IntType, ClassType(cls)) =>
          isSubclass(BoxedIntegerClass, cls) ||
          cls == BoxedByteClass ||
          cls == BoxedShortClass ||
          cls == BoxedDoubleClass
        case (LongType, ClassType(cls)) =>
          isSubclass(BoxedLongClass, cls)
        case (FloatType, ClassType(cls)) =>
          isSubclass(BoxedFloatClass, cls) ||
          cls == BoxedDoubleClass
        case (DoubleType, ClassType(cls)) =>
          isSubclass(BoxedDoubleClass, cls)
        case (StringType, ClassType(cls)) =>
          isSubclass(StringClass, cls)

        case (IntType, DoubleType)   => true
        case (FloatType, DoubleType) => true

        case (ArrayType(lhsBase, lhsDims), ArrayType(rhsBase, rhsDims)) =>
          if (lhsDims < rhsDims) {
            false // because Array[A] </: Array[Array[A]]
          } else if (lhsDims > rhsDims) {
            rhsBase == ObjectClass // because Array[Array[A]] <: Array[Object]
          } else { // lhsDims == rhsDims
            // lhsBase must be <: rhsBase
            if (isPrimitiveClass(lhsBase) || isPrimitiveClass(rhsBase)) {
              lhsBase == rhsBase
            } else {
              /* All things must be considered subclasses of Object for this
               * purpose, even raw JS types and interfaces, which do not have
               * Object in their ancestors.
               */
              rhsBase == ObjectClass || isSubclass(lhsBase, rhsBase)
            }
          }

        case (ArrayType(_, _), ClassType(cls)) =>
          AncestorsOfPseudoArrayClass.contains(cls)

        case _ =>
          false
      })
    }
  }
}
