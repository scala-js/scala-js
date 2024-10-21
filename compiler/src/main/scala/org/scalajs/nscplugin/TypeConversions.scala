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

package org.scalajs.nscplugin

import scala.tools.nsc._

import org.scalajs.ir.Types

/** Conversions from scalac `Type`s to the IR `Type`s and `TypeRef`s. */
trait TypeConversions[G <: Global with Singleton] extends SubComponent {
  this: GenJSCode[G] =>

  import global._
  import definitions._

  private lazy val primitiveIRTypeMap: Map[Symbol, Types.Type] = {
    Map(
        UnitClass    -> Types.VoidType,
        BooleanClass -> Types.BooleanType,
        CharClass    -> Types.CharType,
        ByteClass    -> Types.ByteType,
        ShortClass   -> Types.ShortType,
        IntClass     -> Types.IntType,
        LongClass    -> Types.LongType,
        FloatClass   -> Types.FloatType,
        DoubleClass  -> Types.DoubleType,
        NothingClass -> Types.NothingType,
        NullClass    -> Types.NullType
    )
  }

  private lazy val primitiveRefMap: Map[Symbol, Types.NonArrayTypeRef] = {
    Map(
        UnitClass    -> Types.VoidRef,
        BooleanClass -> Types.BooleanRef,
        CharClass    -> Types.CharRef,
        ByteClass    -> Types.ByteRef,
        ShortClass   -> Types.ShortRef,
        IntClass     -> Types.IntRef,
        LongClass    -> Types.LongRef,
        FloatClass   -> Types.FloatRef,
        DoubleClass  -> Types.DoubleRef,
        NothingClass -> Types.ClassRef(encodeClassName(RuntimeNothingClass)),
        NullClass    -> Types.ClassRef(encodeClassName(RuntimeNullClass))
    )
  }

  def toIRType(t: Type): Types.Type = {
    val (base, arrayDepth) = convert(t)
    if (arrayDepth == 0)
      primitiveIRTypeMap.getOrElse(base, encodeClassType(base))
    else
      Types.ArrayType(makeArrayTypeRef(base, arrayDepth), nullable = true)
  }

  def toTypeRef(t: Type): Types.TypeRef = {
    val (base, arrayDepth) = convert(t)
    if (arrayDepth == 0)
      makeNonArrayTypeRef(base)
    else
      makeArrayTypeRef(base, arrayDepth)
  }

  private def makeNonArrayTypeRef(sym: Symbol): Types.NonArrayTypeRef =
    primitiveRefMap.getOrElse(sym, Types.ClassRef(encodeClassName(sym)))

  private def makeArrayTypeRef(base: Symbol, depth: Int): Types.ArrayTypeRef =
    Types.ArrayTypeRef(makeNonArrayTypeRef(base), depth)

  // The following code was modeled after backend.icode.TypeKinds.toTypeKind

  /** Converts the given `Type` to a Scala.js `Type` or `TypeRef`, according to
   *  the given `ConversionFinisher`.
   *
   *  @param t
   *    The `Type` to convert
   *  @return
   *    The base symbol type, and the array depth. If the array depth is 0, it
   *    means that the base symbol itself is the result.
   */
  /* The call to .normalize fixes #3003 (follow type aliases). Otherwise,
   * convertMaybeArray below would return ObjectReference.
   */
  private def convert(t: Type): (Symbol, Int) = t.normalize match {
    case ThisType(ArrayClass)            => (ObjectClass, 0)
    case ThisType(sym)                   => (sym, 0)
    case SingleType(_, sym)              => (sym, 0)
    case ConstantType(_)                 => convert(t.underlying)
    case TypeRef(_, sym, args)           => convertMaybeArray(sym, args)
    case ClassInfoType(_, _, ArrayClass) => abort("ClassInfoType to ArrayClass!")
    case ClassInfoType(_, _, sym)        => (sym, 0)

    // !!! Iulian says types which make no sense after erasure should not reach here,
    // which includes the ExistentialType, AnnotatedType, RefinedType.  I don't know
    // if the first two cases exist because they do or as a defensive measure, but
    // at the time I added it, RefinedTypes were indeed reaching here.
    // !!! Removed in JavaScript backend because I do not know what to do with lub
    //case ExistentialType(_, t)           => toTypeKind(t)
    // Apparently, this case does occur (see pos/CustomGlobal.scala)
    case t: AnnotatedType                => convert(t.underlying)
    //case RefinedType(parents, _)         => parents map toTypeKind reduceLeft lub

    /* This case is not in scalac. We need it for the test
     * run/valueclasses-classtag-existential. I have no idea how icode does
     * not fail this test: we do everything the same as icode up to here.
     */
    case tpe: ErasedValueType            => (tpe.valueClazz, 0)

    // For sure WildcardTypes shouldn't reach here either, but when
    // debugging such situations this may come in handy.
    // case WildcardType                    => (ObjectClass, 0)
    case norm => abort(
      "Unknown type: %s, %s [%s, %s] TypeRef? %s".format(
        t, norm, t.getClass, norm.getClass, t.isInstanceOf[TypeRef]
      )
    )
  }

  /** Convert a type ref, possibly an array type. */
  private def convertMaybeArray(sym: Symbol,
      targs: List[Type]): (Symbol, Int) = sym match {
    case ArrayClass =>
      val convertedArg = convert(targs.head)
      (convertedArg._1, convertedArg._2 + 1)
    case _ if sym.isClass =>
      (sym, 0)
    case _ =>
      assert(sym.isType, sym) // it must be compiling Array[a]
      (ObjectClass, 0)
  }
}
