/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package org.scalajs.core.compiler

import scala.tools.nsc._

import org.scalajs.core.ir
import ir.{Definitions, Types}

/** Glue representation of types as seen from the IR but still with a
 *  reference to the Symbols.
 *
 *  @author Sébastien Doeraene
 */
trait TypeKinds extends SubComponent { this: GenJSCode =>
  import global._
  import jsAddons._
  import definitions._

  lazy val ObjectReference = REFERENCE(definitions.ObjectClass)

  lazy val VoidKind    = VOID
  lazy val BooleanKind = BOOL
  lazy val CharKind    = INT(CharClass)
  lazy val ByteKind    = INT(ByteClass)
  lazy val ShortKind   = INT(ShortClass)
  lazy val IntKind     = INT(IntClass)
  lazy val LongKind    = LONG
  lazy val FloatKind   = FLOAT(FloatClass)
  lazy val DoubleKind  = FLOAT(DoubleClass)

  /** TypeKinds for Scala primitive types. */
  lazy val primitiveTypeMap: Map[Symbol, TypeKind] = {
    import definitions._
    Map(
      UnitClass    -> VoidKind,
      BooleanClass -> BooleanKind,
      CharClass    -> CharKind,
      ByteClass    -> ByteKind,
      ShortClass   -> ShortKind,
      IntClass     -> IntKind,
      LongClass    -> LongKind,
      FloatClass   -> FloatKind,
      DoubleClass  -> DoubleKind
    )
  }

  /** Glue representation of types as seen from the IR but still with a
   *  reference to the Symbols.
   */
  sealed abstract class TypeKind {
    def isReferenceType: Boolean = false
    def isArrayType: Boolean = false
    def isValueType: Boolean = false

    def toIRType: Types.Type
    def toReferenceType: Types.ReferenceType
  }

  sealed abstract class TypeKindButArray extends TypeKind {
    protected def typeSymbol: Symbol

    override def toReferenceType: Types.ClassType =
      Types.ClassType(encodeClassFullName(typeSymbol))
  }

  /** The void, for trees that can only appear in statement position. */
  case object VOID extends TypeKindButArray {
    protected def typeSymbol: Symbol = UnitClass
    def toIRType: Types.NoType.type = Types.NoType
  }

  sealed abstract class ValueTypeKind extends TypeKindButArray {
    override def isValueType: Boolean = true

    val primitiveCharCode: Char = typeSymbol match {
      case BooleanClass  => 'Z'
      case CharClass     => 'C'
      case ByteClass     => 'B'
      case ShortClass    => 'S'
      case IntClass      => 'I'
      case LongClass     => 'J'
      case FloatClass    => 'F'
      case DoubleClass   => 'D'
      case x => abort("Unknown primitive type: " + x.fullName)
    }
  }

  /** Integer number (Byte, Short, Char or Int). */
  case class INT private[TypeKinds] (typeSymbol: Symbol) extends ValueTypeKind {
    def toIRType: Types.IntType.type = Types.IntType
  }

  /** Long */
  case object LONG extends ValueTypeKind {
    protected def typeSymbol = definitions.LongClass
    def toIRType: Types.LongType.type = Types.LongType
  }

  /** Floating-point number (Float or Double). */
  case class FLOAT private[TypeKinds] (typeSymbol: Symbol) extends ValueTypeKind {
    def toIRType: Types.Type =
      if (typeSymbol == FloatClass) Types.FloatType
      else Types.DoubleType
  }

  /** Boolean */
  case object BOOL extends ValueTypeKind {
    protected def typeSymbol = definitions.BooleanClass
    def toIRType: Types.BooleanType.type = Types.BooleanType
  }

  /** Nothing */
  case object NOTHING extends TypeKindButArray {
    protected def typeSymbol: Symbol = definitions.NothingClass
    def toIRType: Types.NothingType.type = Types.NothingType
    override def toReferenceType: Types.ClassType =
      Types.ClassType(Definitions.RuntimeNothingClass)
  }

  /** Null */
  case object NULL extends TypeKindButArray {
    protected def typeSymbol: Symbol = definitions.NullClass
    def toIRType: Types.NullType.type = Types.NullType
    override def toReferenceType: Types.ClassType =
      Types.ClassType(Definitions.RuntimeNullClass)
  }

  /** An object */
  case class REFERENCE private[TypeKinds] (typeSymbol: Symbol) extends TypeKindButArray {
    override def toString(): String = "REFERENCE(" + typeSymbol.fullName + ")"
    override def isReferenceType: Boolean = true

    def toIRType: Types.Type = encodeClassType(typeSymbol)
  }

  /** An array */
  case class ARRAY private[TypeKinds] (elem: TypeKind) extends TypeKind {
    override def toString(): String = "ARRAY[" + elem + "]"
    override def isArrayType: Boolean = true

    def dimensions: Int = elem match {
      case a: ARRAY => a.dimensions + 1
      case _        => 1
    }

    override def toIRType: Types.ArrayType = toReferenceType

    override def toReferenceType: Types.ArrayType = {
      Types.ArrayType(
          elementKind.toReferenceType.className,
          dimensions)
    }

    /** The ultimate element type of this array. */
    def elementKind: TypeKindButArray = elem match {
      case a: ARRAY            => a.elementKind
      case k: TypeKindButArray => k
    }
  }

  ////////////////// Conversions //////////////////////////////

  def toIRType(t: Type): Types.Type =
    toTypeKind(t).toIRType

  def toReferenceType(t: Type): Types.ReferenceType =
    toTypeKind(t).toReferenceType

  // The following code is a hard copy-and-paste from backend.icode.TypeKinds

  /** Return the TypeKind of the given type
   *
   *  Call to .normalize fixes #3003 (follow type aliases). Otherwise,
   *  arrayOrClassType below would return ObjectReference.
   */
  def toTypeKind(t: Type): TypeKind = t.normalize match {
    case ThisType(ArrayClass)            => ObjectReference
    case ThisType(sym)                   => newReference(sym)
    case SingleType(_, sym)              => primitiveOrRefType(sym)
    case ConstantType(_)                 => toTypeKind(t.underlying)
    case TypeRef(_, sym, args)           => primitiveOrClassType(sym, args)
    case ClassInfoType(_, _, ArrayClass) => abort("ClassInfoType to ArrayClass!")
    case ClassInfoType(_, _, sym)        => primitiveOrRefType(sym)

    // !!! Iulian says types which make no sense after erasure should not reach here,
    // which includes the ExistentialType, AnnotatedType, RefinedType.  I don't know
    // if the first two cases exist because they do or as a defensive measure, but
    // at the time I added it, RefinedTypes were indeed reaching here.
    // !!! Removed in JavaScript backend because I do not know what to do with lub
    //case ExistentialType(_, t)           => toTypeKind(t)
    // Apparently, this case does occur (see pos/CustomGlobal.scala)
    case t: AnnotatedType                => toTypeKind(t.underlying)
    //case RefinedType(parents, _)         => parents map toTypeKind reduceLeft lub

    /* This case is not in scalac. We need it for the test
     * run/valueclasses-classtag-existential. I have no idea how icode does
     * not fail this test: we do everything the same as icode up to here.
     */
    case tpe: ErasedValueType            => newReference(tpe.valueClazz)

    // For sure WildcardTypes shouldn't reach here either, but when
    // debugging such situations this may come in handy.
    // case WildcardType                    => REFERENCE(ObjectClass)
    case norm => abort(
      "Unknown type: %s, %s [%s, %s] TypeRef? %s".format(
        t, norm, t.getClass, norm.getClass, t.isInstanceOf[TypeRef]
      )
    )
  }

  /** Return the type kind of a class, possibly an array type.
   */
  private def arrayOrClassType(sym: Symbol, targs: List[Type]) = sym match {
    case ArrayClass       => ARRAY(toTypeKind(targs.head))
    case _ if sym.isClass => newReference(sym)
    case _                =>
      assert(sym.isType, sym) // it must be compiling Array[a]
      ObjectReference
  }

  /** Interfaces have to be handled delicately to avoid introducing
   *  spurious errors, but if we treat them all as AnyRef we lose too
   *  much information.
   */
  private def newReference(sym: Symbol): TypeKind = sym match {
    case NothingClass => NOTHING
    case NullClass    => NULL
    case _ =>
      // Can't call .toInterface (at this phase) or we trip an assertion.
      // See PackratParser#grow for a method which fails with an apparent mismatch
      // between "object PackratParsers$class" and "trait PackratParsers"
      if (sym.isImplClass) {
        // pos/spec-List.scala is the sole failure if we don't check for NoSymbol
        val traitSym = sym.owner.info.decl(tpnme.interfaceName(sym.name))
        if (traitSym != NoSymbol)
          REFERENCE(traitSym)
        else
          REFERENCE(sym)
      } else {
        REFERENCE(sym)
      }
  }

  private def primitiveOrRefType(sym: Symbol) =
    primitiveTypeMap.getOrElse(sym, newReference(sym))
  private def primitiveOrClassType(sym: Symbol, targs: List[Type]) =
    primitiveTypeMap.getOrElse(sym, arrayOrClassType(sym, targs))
}
