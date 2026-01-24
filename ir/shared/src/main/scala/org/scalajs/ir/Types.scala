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

    /** Is `null` an admissible value of this type? */
    def isNullable: Boolean = this match {
      case AnyType | NullType          => true
      case ClassType(_, nullable, _)   => nullable
      case ArrayType(_, nullable, _)   => nullable
      case ClosureType(_, _, nullable) => nullable
      case _                           => false
    }

    /** A type that accepts the same values as this type except `null`, unless
     *  this type is `VoidType`.
     *
     *  If `this` is `VoidType`, returns this type.
     *
     *  For all other types `tpe`, `tpe.toNonNullable.isNullable` is `false`.
     */
    def toNonNullable: Type
  }

  sealed abstract class PrimType extends Type {
    final def toNonNullable: PrimType = this match {
      case NullType => NothingType
      case _        => this
    }
  }

  /* Each PrimTypeWithRef creates its corresponding `PrimRef`. Therefore, it
   * takes the parameters that need to be passed to the `PrimRef` constructor.
   * This little dance ensures proper initialization safety between
   * `PrimTypeWithRef`s and `PrimRef`s.
   */
  sealed abstract class PrimTypeWithRef(primRefCharCode: Char, primRefDisplayName: String)
      extends PrimType {
    val primRef: PrimRef = new PrimRef(this, primRefCharCode, primRefDisplayName)
  }

  /** Any type.
   *
   *  This is the supertype of all value types that can be passed to JavaScript
   *  code. Record types are the canonical counter-example: they are not
   *  subtypes of `any` because their values cannot be given to JavaScript.
   *
   *  This type supports a very limited set of Scala operations, the ones
   *  common to all values. Basically only reference equality tests and
   *  instance tests. It also supports all JavaScript operations, since all
   *  Scala objects are also genuine JavaScript values.
   *
   *  The type java.lang.Object in the back-end maps to [[AnyType]] because it
   *  can hold JS values (not only instances of Scala.js classes).
   */
  case object AnyType extends Type {
    def toNonNullable: AnyNotNullType.type = AnyNotNullType
  }

  /** Any type except `null`. */
  case object AnyNotNullType extends Type {
    def toNonNullable: this.type = this
  }

  // Can't link to Nothing - #1969
  /** Nothing type (the bottom type of this type system).
   *  Expressions from which one can never come back are typed as `Nothing`.
   *  For example, `throw` and `return`.
   */
  case object NothingType extends PrimTypeWithRef('E', "nothing")

  /** The type of `undefined`. */
  case object UndefType extends PrimType

  /** Boolean type.
   *  It does not accept `null` nor `undefined`.
   */
  case object BooleanType extends PrimTypeWithRef('Z', "boolean")

  /** `Char` type, a 16-bit UTF-16 code unit.
   *  It does not accept `null` nor `undefined`.
   */
  case object CharType extends PrimTypeWithRef('C', "char")

  /** 8-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object ByteType extends PrimTypeWithRef('B', "byte")

  /** 16-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object ShortType extends PrimTypeWithRef('S', "short")

  /** 32-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object IntType extends PrimTypeWithRef('I', "int")

  /** 64-bit signed integer type.
   *  It does not accept `null` nor `undefined`.
   */
  case object LongType extends PrimTypeWithRef('J', "long")

  /** Float type (32-bit).
   *  It does not accept `null` nor `undefined`.
   */
  case object FloatType extends PrimTypeWithRef('F', "float")

  /** Double type (64-bit).
   *  It does not accept `null` nor `undefined`.
   */
  case object DoubleType extends PrimTypeWithRef('D', "double")

  /** String type.
   *  It does not accept `null` nor `undefined`.
   */
  case object StringType extends PrimType

  /** The type of `null`.
   *  It does not accept `undefined`.
   *  The null type is a subtype of all class types and array types.
   */
  case object NullType extends PrimTypeWithRef('N', "null")

  /** Class (or interface) type.
   *
   *  A value of `ClassType(cls, nullable, exact)` can be:
   *
   *  - an instance of `className` itself,
   *  - an instance of a subclass of `className`, if `exact = false`, or
   *  - `null`, if `nullable = true`.
   *
   *  `nullable` and `exact` are not mutually exclusive. A value of
   *  `ClassType(cls, nullable = true, exact = true)` can hold an instance of
   *  `cls` or the value `null`, but not an instance of a subclass of `cls`.
   *
   *  Exact interface types are not prohibited, but are necessarily uninhabited
   *  except if they are nullable. Similarly, non-nullable exact hijacked class
   *  types are uninhabited.
   *
   *  If a class is final, it does not automatically allow all its `ClassType`s
   *  to be exact. Removing the `final` modifier on a class is considered a
   *  backward binary compatible change (unlike adding it). If we allowed exact
   *  types for every reference to a `final` class, that would break that
   *  property: removing the `final` modifier would invalidate all exact
   *  references to that class in other IR files.
   */
  final case class ClassType(className: ClassName, nullable: Boolean, exact: Boolean) extends Type {
    def toNullable: ClassType = ClassType(className, nullable = true, exact)

    def toNonNullable: ClassType = ClassType(className, nullable = false, exact)

    def toExact: ClassType = ClassType(className, nullable, exact = true)

    def toNonExact: ClassType = ClassType(className, nullable, exact = false)
  }

  /** Array type.
   *
   *  Although the array type itself may be non-nullable, the *elements* of an
   *  array are always nullable for non-primitive types. This is unavoidable,
   *  since arrays can be created with their elements initialized with the zero
   *  of the element type.
   *
   *  See [[ClassType]] for a discussion of `nullable` and `exact` (recall that
   *  array types are covariant).
   */
  final case class ArrayType(arrayTypeRef: ArrayTypeRef, nullable: Boolean, exact: Boolean)
      extends Type {
    def toNullable: ArrayType = ArrayType(arrayTypeRef, nullable = true, exact)

    def toNonNullable: ArrayType = ArrayType(arrayTypeRef, nullable = false, exact)

    def toExact: ArrayType = ArrayType(arrayTypeRef, nullable, exact = true)

    def toNonExact: ArrayType = ArrayType(arrayTypeRef, nullable, exact = false)
  }

  /** Closure type.
   *
   *  This is the type of a typed closure. Parameters and result are
   *  statically typed according to the `closureTypeRef` components.
   *
   *  Closure types may be nullable. `Null()` is a valid value of a nullable
   *  closure type. This is unfortunately required to have default values of
   *  closure types, which in turn is required to be used as the type of a
   *  field.
   *
   *  Closure types are non-variant in both parameter and result types.
   *
   *  Closure types are *not* subtypes of `AnyType`. That statically prevents
   *  them from going into JavaScript code or in any other universal context.
   *  They do not support type tests nor casts.
   *
   *  The following subtyping relationships hold for any closure type `CT`:
   *  {{{
   *  nothing <: CT <: void
   *  }}}
   *  For a nullable closure type `CT`, additionally the following subtyping
   *  relationship holds:
   *  {{{
   *  null <: CT
   *  }}}
   */
  final case class ClosureType(paramTypes: List[Type], resultType: Type,
      nullable: Boolean)
      extends Type {
    def toNonNullable: ClosureType =
      ClosureType(paramTypes, resultType, nullable = false)
  }

  /** Record type.
   *
   *  Used by the optimizer to inline classes as records with multiple fields.
   *  They are desugared as several local variables by JSDesugaring.
   *  Record types cannot cross method boundaries, so they cannot appear as
   *  the type of fields or parameters, nor as result types of methods.
   *  The compiler itself never generates record types.
   *
   *  Record types currently do not feature any form of subtyping. For R1 to be
   *  a subtype of R2, it must have the same fields, in the same order, with
   *  equivalent types.
   *
   *  Record types are not subtypes of `any`. As such, they can never be passed
   *  to JavaScript.
   */
  final case class RecordType(fields: List[RecordType.Field]) extends Type {
    def findField(name: SimpleFieldName): RecordType.Field =
      fields.find(_.name == name).get

    def toNonNullable: this.type = this
  }

  object RecordType {
    final case class Field(name: SimpleFieldName, originalName: OriginalName,
        tpe: Type, mutable: Boolean)
  }

  /** Void type, the top of type of our type system. */
  case object VoidType extends PrimTypeWithRef('V', "void")

  @deprecated("Use VoidType instead", since = "1.18.0")
  lazy val NoType: VoidType.type = VoidType

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
          case _: TransientTypeRef =>
            -1
          case _ =>
            1
        }
      case thiz: TransientTypeRef =>
        that match {
          case that: TransientTypeRef => thiz.name.compareTo(that.name)
          case _                      => 1
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

  // scalastyle:off equals.hash.code
  // PrimRef uses reference equality, but has a stable hashCode() method

  /** Primitive type reference. */
  final class PrimRef private[Types] (val tpe: PrimTypeWithRef,
      charCodeInit: Char, displayNameInit: String) // "Init" variants so we can have good Scaladoc on the val's
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
    val displayName: String = displayNameInit

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
    val charCode: Char = charCodeInit

    // Stable hash code, corresponding to reference equality
    override def hashCode(): Int = charCode.##
  }

  // scalastyle:on equals.hash.code

  object PrimRef {
    def unapply(typeRef: PrimRef): Some[PrimTypeWithRef] =
      Some(typeRef.tpe)
  }

  final val VoidRef = VoidType.primRef
  final val BooleanRef = BooleanType.primRef
  final val CharRef = CharType.primRef
  final val ByteRef = ByteType.primRef
  final val ShortRef = ShortType.primRef
  final val IntRef = IntType.primRef
  final val LongRef = LongType.primRef
  final val FloatRef = FloatType.primRef
  final val DoubleRef = DoubleType.primRef
  final val NullRef = NullType.primRef
  final val NothingRef = NothingType.primRef

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
      case innerType: NonArrayTypeRef  => ArrayTypeRef(innerType, 1)
      case ArrayTypeRef(base, dim)     => ArrayTypeRef(base, dim + 1)
      case innerType: TransientTypeRef => throw new IllegalArgumentException(innerType.toString())
    }
  }

  /** Transient TypeRef to store any type as a method parameter or result type.
   *
   *  `TransientTypeRef` cannot be serialized. It is only used in the linker to
   *  support some of its desugarings and/or optimizations.
   *
   *  `TransientTypeRef`s cannot be used for methods in the `Public` namespace.
   *
   *  The `name` is used for equality, hashing, and sorting. It is assumed that
   *  all occurrences of a `TransientTypeRef` with the same `name` associated
   *  to an enclosing method namespace (enclosing class, member namespace and
   *  simple method name) have the same `tpe`.
   */
  final case class TransientTypeRef(name: LabelName)(val tpe: Type) extends TypeRef {
    def displayName: String = name.nameString
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

    case NullType | AnyType | ClassType(_, true, _) | ArrayType(_, true, _) |
        ClosureType(_, _, true) =>
      Null()

    case tpe: RecordType =>
      RecordValue(tpe, tpe.fields.map(f => zeroOf(f.tpe)))

    case NothingType | VoidType | ClassType(_, false, _) | ArrayType(_, false, _) |
        ClosureType(_, _, false) | AnyNotNullType =>
      throw new IllegalArgumentException(s"cannot generate a zero for $tpe")
  }

  /** Tests whether a type `lhs` is a subtype of `rhs` (or equal).
   *  @param isSubclass A function testing whether a class/interface is a
   *                    subclass of another class/interface.
   */
  def isSubtype(lhs: Type, rhs: Type)(
      isSubclass: (ClassName, ClassName) => Boolean): Boolean = {

    /* It is fine to use WellKnownNames here because nothing in `Names` nor
     * `Types` calls `isSubtype`. So this code path is not reached during their
     * initialization.
     */
    import WellKnownNames.{AncestorsOfPseudoArrayClass, ObjectClass, PrimTypeToBoxedClass}

    def isSubnullable(lhs: Boolean, rhs: Boolean): Boolean =
      rhs || !lhs

    (lhs, rhs) match {
      case _ if lhs == rhs  => true
      case (NothingType, _) => true
      case (_, VoidType)    => true
      case (VoidType, _)    => false

      case (NullType, _) => rhs.isNullable

      case (ClosureType(lhsParamTypes, lhsResultType, lhsNullable),
              ClosureType(rhsParamTypes, rhsResultType, rhsNullable)) =>
        isSubnullable(lhsNullable, rhsNullable) &&
        lhsParamTypes == rhsParamTypes &&
        lhsResultType == rhsResultType

      case (_: ClosureType, _) => false
      case (_, _: ClosureType) => false

      case (_: RecordType, _) => false
      case (_, _: RecordType) => false

      case (_, AnyType)        => true
      case (_, AnyNotNullType) => !lhs.isNullable

      case (ClassType(lhsClass, lhsNullable, lhsExact),
              ClassType(rhsClass, rhsNullable, rhsExact)) =>
        isSubnullable(lhsNullable, rhsNullable) && {
          if (rhsExact)
            lhsExact && lhsClass == rhsClass
          else
            isSubclass(lhsClass, rhsClass)
        }

      case (primType: PrimType, ClassType(rhsClass, _, false)) =>
        val lhsClass = PrimTypeToBoxedClass.getOrElse(primType, {
          throw new AssertionError(s"unreachable case for isSubtype($lhs, $rhs)")
        })
        isSubclass(lhsClass, rhsClass)

      case (ArrayType(ArrayTypeRef(lhsBase, lhsDims), lhsNullable, lhsExact),
              ArrayType(ArrayTypeRef(rhsBase, rhsDims), rhsNullable, rhsExact)) =>
        isSubnullable(lhsNullable, rhsNullable) && {
          if (rhsExact) {
            lhsExact && lhsBase == rhsBase && lhsDims == rhsDims
          } else if (lhsDims < rhsDims) {
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
        }

      case (ArrayType(_, lhsNullable, _), ClassType(className, rhsNullable, false)) =>
        isSubnullable(lhsNullable, rhsNullable) &&
        AncestorsOfPseudoArrayClass.contains(className)

      case _ =>
        false
    }
  }
}
