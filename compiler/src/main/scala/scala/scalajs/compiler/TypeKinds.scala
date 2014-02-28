/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

/** Types as their are viewed by JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait TypeKinds extends SubComponent { this: GenJSCode =>
  import global._
  import jsAddons._

  import definitions.{ UnitClass, BooleanClass, CharClass, ByteClass,
    ShortClass, IntClass, LongClass, FloatClass, DoubleClass, ArrayClass,
    AnyRefClass, ObjectClass, NullClass, NothingClass, arrayType,
    RuntimeNullClass, RuntimeNothingClass }

  lazy val ObjectReference = REFERENCE(definitions.ObjectClass)

  lazy val UndefinedKind = UNDEFINED
  //lazy val NullKind = NULL
  lazy val BooleanKind = BOOL
  lazy val CharKind = INT(CharClass)
  lazy val ByteKind = INT(ByteClass)
  lazy val ShortKind = INT(ShortClass)
  lazy val IntKind = INT(IntClass)
  lazy val LongKind = INT(LongClass)
  lazy val FloatKind = FLOAT(FloatClass)
  lazy val DoubleKind = FLOAT(DoubleClass)
  lazy val RefKind = ObjectReference

  lazy val primitiveKinds = List(UndefinedKind, /*NullKind,*/ BooleanKind,
      CharKind, ByteKind, ShortKind, IntKind, LongKind, FloatKind, DoubleKind)

  /** A map from scala primitive Types to OzCode TypeKinds */
  lazy val primitiveTypeMap: Map[Symbol, TypeKind] = {
    import definitions._
    Map(
      UnitClass     -> UndefinedKind,
      BooleanClass  -> BooleanKind,
      CharClass     -> CharKind,
      ByteClass     -> ByteKind,
      ShortClass    -> ShortKind,
      IntClass      -> IntKind,
      LongClass     -> LongKind,
      FloatClass    -> FloatKind,
      DoubleClass   -> DoubleKind
    )
  }

  /**
   * This class represents a type kind. Type kinds represent the types that
   * JavaScript knows.
   */
  sealed abstract class TypeKind {
    def isReferenceType = false
    def isArrayType = false
    def isValueType = false

    def dimensions: Int = 0

    override def toString = {
      this.getClass.getName stripSuffix "$" dropWhile (_ != '$') drop 1
    }

    def toType: Type
  }

  sealed abstract class ValueTypeKind(cls: Symbol) extends TypeKind {
    override def isValueType = true

    def toType = cls.tpe

    val primitiveCharCode = cls match {
      case UnitClass     => "V"
      case BooleanClass  => "Z"
      case CharClass     => "C"
      case ByteClass     => "B"
      case ShortClass    => "S"
      case IntClass      => "I"
      case LongClass     => "J"
      case FloatClass    => "F"
      case DoubleClass   => "D"
      case x => abort("Unknown primitive type: " + x.fullName)
    }
  }

  /** The undefined value */
  case object UNDEFINED extends ValueTypeKind(definitions.UnitClass) {}

  /** The null value */
  //case object NULL extends ValueTypeKind(definitions.NullClass) {}

  /** Int */
  case class INT(cls: Symbol) extends ValueTypeKind(cls) {}

  /** Float */
  case class FLOAT(cls: Symbol) extends ValueTypeKind(cls) {}

  /** Boolean */
  case object BOOL extends ValueTypeKind(definitions.BooleanClass) {}

  /** An object */
  case class REFERENCE(cls: Symbol) extends TypeKind {
    override def toString = "REFERENCE(" + cls.fullName + ")"
    override def isReferenceType = true

    def toType = cls.tpe
  }

  /** An array */
  case class ARRAY(elem: TypeKind) extends TypeKind {
    override def toString = "ARRAY[" + elem + "]"
    override def isArrayType = true
    override def dimensions = elem.dimensions + 1

    def toType = arrayType(elem.toType)

    /** The ultimate element type of this array. */
    def elementKind: TypeKind = elem match {
      case a : ARRAY => a.elementKind
      case k => k
    }
  }

  ////////////////// Conversions //////////////////////////////

  // The following code is a hard copy-and-paste from backend.icode.TypeKinds

  /** Return the TypeKind of the given type
   *
   *  Call to .normalize fixes #3003 (follow type aliases). Otherwise,
   *  arrayOrClassType below would return ObjectReference.
   */
  def toTypeKind(t: Type): TypeKind = t.normalize match {
    case ThisType(ArrayClass)            => ObjectReference
    case ThisType(sym)                   => REFERENCE(sym)
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
    case AnnotatedType(_, t, _)          => toTypeKind(t)
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
  private def newReference(sym: Symbol): TypeKind = {
    // Can't call .toInterface (at this phase) or we trip an assertion.
    // See PackratParser#grow for a method which fails with an apparent mismatch
    // between "object PackratParsers$class" and "trait PackratParsers"
    if (sym.isImplClass) {
      // pos/spec-List.scala is the sole failure if we don't check for NoSymbol
      val traitSym = sym.owner.info.decl(tpnme.interfaceName(sym.name))
      if (traitSym != NoSymbol)
        return REFERENCE(traitSym)
    }
    REFERENCE(sym)
  }

  private def primitiveOrRefType(sym: Symbol) =
    primitiveTypeMap.getOrElse(sym, newReference(sym))
  private def primitiveOrClassType(sym: Symbol, targs: List[Type]) =
    primitiveTypeMap.getOrElse(sym, arrayOrClassType(sym, targs))

  /**
   * Extractor object for Scala runtime mapped types
   *
   * These are types that are mapped to a different type at runtime.
   * Currently scala.Nothing and scala.Null
   */
  object ScalaRTMapped {
    def unapply(cls: Symbol) = cls match {
      case NullClass    => Some(RuntimeNullClass)
      case NothingClass => Some(RuntimeNothingClass)
      case _ => None
    }
  }
}
