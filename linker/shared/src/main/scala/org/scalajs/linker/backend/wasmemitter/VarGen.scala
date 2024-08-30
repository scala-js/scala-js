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

package org.scalajs.linker.backend.wasmemitter

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly.Identitities._

/** Manages generation of non-local IDs.
 *
 *  `LocalID`s and `LabelID`s are directly managed by `FunctionBuilder` instead.
 */
object VarGen {

  object genGlobalID {
    final case class forImportedModule(moduleName: String) extends GlobalID
    final case class forModuleInstance(className: ClassName) extends GlobalID
    final case class forModuleInitFlag(className: ClassName) extends GlobalID
    final case class forJSClassValue(className: ClassName) extends GlobalID

    final case class forVTable(typeRef: NonArrayTypeRef) extends GlobalID

    object forVTable {
      def apply(className: ClassName): forVTable =
        forVTable(ClassRef(className))
    }

    final case class forITable(className: ClassName) extends GlobalID
    final case class forStaticField(fieldName: FieldName) extends GlobalID
    final case class forJSPrivateField(fieldName: FieldName) extends GlobalID

    case object bZeroChar extends GlobalID
    case object bZeroLong extends GlobalID
    case object stringLiteralCache extends GlobalID
    case object arrayClassITable extends GlobalID
    case object lastIDHashCode extends GlobalID

    /** A `GlobalID` for a JS helper global.
     *
     *  Its `toString()` is guaranteed to correspond to the import name of the helper.
     */
    sealed abstract class JSHelperGlobalID extends GlobalID

    case object jsLinkingInfo extends JSHelperGlobalID
    case object undef extends JSHelperGlobalID
    case object bFalse extends JSHelperGlobalID
    case object bZero extends JSHelperGlobalID
    case object emptyString extends JSHelperGlobalID
    case object idHashCodeMap extends JSHelperGlobalID
  }

  object genFunctionID {
    final case class forMethod(namespace: MemberNamespace,
        className: ClassName, methodName: MethodName)
        extends FunctionID

    final case class forTableEntry(className: ClassName, methodName: MethodName)
        extends FunctionID

    final case class forExport(exportedName: String) extends FunctionID
    final case class forTopLevelExportSetter(exportedName: String) extends FunctionID

    final case class loadModule(className: ClassName) extends FunctionID
    final case class newDefault(className: ClassName) extends FunctionID
    final case class instanceTest(className: ClassName) extends FunctionID
    final case class clone(className: ClassName) extends FunctionID
    final case class cloneArray(arrayBaseRef: NonArrayTypeRef) extends FunctionID

    final case class asInstance(targetTpe: Type) extends FunctionID

    final case class arrayGet(baseRef: NonArrayTypeRef) extends FunctionID

    def arrayGetFor(arrayTypeRef: ArrayTypeRef): arrayGet = arrayTypeRef match {
      case ArrayTypeRef(base: PrimRef, 1) => arrayGet(base)
      case _                              => arrayGet(ClassRef(ObjectClass))
    }

    final case class arraySet(baseRef: NonArrayTypeRef) extends FunctionID

    def arraySetFor(arrayTypeRef: ArrayTypeRef): arraySet = arrayTypeRef match {
      case ArrayTypeRef(base: PrimRef, 1) => arraySet(base)
      case _                              => arraySet(ClassRef(ObjectClass))
    }

    final case class isJSClassInstance(className: ClassName) extends FunctionID
    final case class loadJSClass(className: ClassName) extends FunctionID
    final case class createJSClassOf(className: ClassName) extends FunctionID
    final case class preSuperStats(className: ClassName) extends FunctionID
    final case class superArgs(className: ClassName) extends FunctionID
    final case class postSuperStats(className: ClassName) extends FunctionID

    case object start extends FunctionID

    // JS helpers

    /** A `FunctionID` for a JS helper function.
     *
     *  Its `toString()` is guaranteed to correspond to the import name of the helper.
     */
    sealed abstract class JSHelperFunctionID extends FunctionID

    case object is extends JSHelperFunctionID

    case object isUndef extends JSHelperFunctionID

    final case class box(primRef: PrimRef) extends JSHelperFunctionID {
      override def toString(): String = "b" + primRef.charCode
    }

    final case class unbox(primRef: PrimRef) extends JSHelperFunctionID {
      override def toString(): String = "u" + primRef.charCode
    }

    final case class typeTest(primRef: PrimRef) extends JSHelperFunctionID {
      override def toString(): String = "t" + primRef.charCode
    }

    case object fmod extends JSHelperFunctionID

    case object closure extends JSHelperFunctionID
    case object closureThis extends JSHelperFunctionID
    case object closureRest extends JSHelperFunctionID
    case object closureThisRest extends JSHelperFunctionID

    case object makeExportedDef extends JSHelperFunctionID
    case object makeExportedDefRest extends JSHelperFunctionID

    case object stringLength extends JSHelperFunctionID
    case object stringCharAt extends JSHelperFunctionID
    case object jsValueToString extends JSHelperFunctionID // for actual toString() call
    case object jsValueToStringForConcat extends JSHelperFunctionID
    case object booleanToString extends JSHelperFunctionID
    case object charToString extends JSHelperFunctionID
    case object intToString extends JSHelperFunctionID
    case object longToString extends JSHelperFunctionID
    case object doubleToString extends JSHelperFunctionID
    case object stringConcat extends JSHelperFunctionID
    case object isString extends JSHelperFunctionID

    case object jsValueType extends JSHelperFunctionID
    case object jsValueDescription extends JSHelperFunctionID
    case object bigintHashCode extends JSHelperFunctionID
    case object symbolDescription extends JSHelperFunctionID
    case object idHashCodeGet extends JSHelperFunctionID
    case object idHashCodeSet extends JSHelperFunctionID

    case object jsGlobalRefGet extends JSHelperFunctionID
    case object jsGlobalRefSet extends JSHelperFunctionID
    case object jsGlobalRefTypeof extends JSHelperFunctionID
    case object jsNewArray extends JSHelperFunctionID
    case object jsArrayPush extends JSHelperFunctionID
    case object jsArraySpreadPush extends JSHelperFunctionID
    case object jsNewObject extends JSHelperFunctionID
    case object jsObjectPush extends JSHelperFunctionID
    case object jsSelect extends JSHelperFunctionID
    case object jsSelectSet extends JSHelperFunctionID
    case object jsNew extends JSHelperFunctionID
    case object jsFunctionApply extends JSHelperFunctionID
    case object jsMethodApply extends JSHelperFunctionID
    case object jsImportCall extends JSHelperFunctionID
    case object jsImportMeta extends JSHelperFunctionID
    case object jsDelete extends JSHelperFunctionID
    case object jsForInSimple extends JSHelperFunctionID
    case object jsIsTruthy extends JSHelperFunctionID

    final case class jsUnaryOp(name: String) extends JSHelperFunctionID {
      override def toString(): String = name
    }

    val jsUnaryOps: Map[JSUnaryOp.Code, jsUnaryOp] = {
      Map(
        JSUnaryOp.+ -> jsUnaryOp("jsUnaryPlus"),
        JSUnaryOp.- -> jsUnaryOp("jsUnaryMinus"),
        JSUnaryOp.~ -> jsUnaryOp("jsUnaryTilde"),
        JSUnaryOp.! -> jsUnaryOp("jsUnaryBang"),
        JSUnaryOp.typeof -> jsUnaryOp("jsUnaryTypeof")
      )
    }

    final case class jsBinaryOp(name: String) extends JSHelperFunctionID {
      override def toString(): String = name
    }

    val jsBinaryOps: Map[JSBinaryOp.Code, jsBinaryOp] = {
      Map(
        JSBinaryOp.=== -> jsBinaryOp("jsStrictEquals"),
        JSBinaryOp.!== -> jsBinaryOp("jsNotStrictEquals"),
        JSBinaryOp.+ -> jsBinaryOp("jsPlus"),
        JSBinaryOp.- -> jsBinaryOp("jsMinus"),
        JSBinaryOp.* -> jsBinaryOp("jsTimes"),
        JSBinaryOp./ -> jsBinaryOp("jsDivide"),
        JSBinaryOp.% -> jsBinaryOp("jsModulus"),
        JSBinaryOp.| -> jsBinaryOp("jsBinaryOr"),
        JSBinaryOp.& -> jsBinaryOp("jsBinaryAnd"),
        JSBinaryOp.^ -> jsBinaryOp("jsBinaryXor"),
        JSBinaryOp.<< -> jsBinaryOp("jsShiftLeft"),
        JSBinaryOp.>> -> jsBinaryOp("jsArithmeticShiftRight"),
        JSBinaryOp.>>> -> jsBinaryOp("jsLogicalShiftRight"),
        JSBinaryOp.< -> jsBinaryOp("jsLessThan"),
        JSBinaryOp.<= -> jsBinaryOp("jsLessEqual"),
        JSBinaryOp.> -> jsBinaryOp("jsGreaterThan"),
        JSBinaryOp.>= -> jsBinaryOp("jsGreaterEqual"),
        JSBinaryOp.in -> jsBinaryOp("jsIn"),
        JSBinaryOp.instanceof -> jsBinaryOp("jsInstanceof"),
        JSBinaryOp.** -> jsBinaryOp("jsExponent")
      )
    }

    case object newSymbol extends JSHelperFunctionID
    case object createJSClass extends JSHelperFunctionID
    case object createJSClassRest extends JSHelperFunctionID
    case object installJSField extends JSHelperFunctionID
    case object installJSMethod extends JSHelperFunctionID
    case object installJSStaticMethod extends JSHelperFunctionID
    case object installJSProperty extends JSHelperFunctionID
    case object installJSStaticProperty extends JSHelperFunctionID
    case object jsSuperSelect extends JSHelperFunctionID
    case object jsSuperSelectSet extends JSHelperFunctionID
    case object jsSuperCall extends JSHelperFunctionID

    // Wasm internal helpers

    case object createStringFromData extends FunctionID
    case object stringLiteral extends FunctionID
    case object typeDataName extends FunctionID
    case object createClassOf extends FunctionID
    case object getClassOf extends FunctionID
    case object arrayTypeData extends FunctionID
    case object valueDescription extends FunctionID
    case object classCastException extends FunctionID
    case object asSpecificRefArray extends FunctionID
    case object throwArrayStoreException extends FunctionID
    case object throwArrayIndexOutOfBoundsException extends FunctionID
    case object throwNegativeArraySizeException extends FunctionID
    case object throwNullPointerException extends FunctionID
    case object checkedStringCharAt extends FunctionID
    case object throwModuleInitError extends FunctionID
    case object isInstanceExternal extends FunctionID
    case object isInstance extends FunctionID
    case object isAssignableFromExternal extends FunctionID
    case object isAssignableFrom extends FunctionID
    case object checkCast extends FunctionID
    case object getComponentType extends FunctionID
    case object newArrayOfThisClass extends FunctionID
    case object anyGetClass extends FunctionID
    case object anyGetClassName extends FunctionID
    case object anyGetTypeData extends FunctionID
    case object newArrayObject extends FunctionID
    case object identityHashCode extends FunctionID
    case object searchReflectiveProxy extends FunctionID

    private final case class SpecializedArrayCopyID(arrayBaseRef: NonArrayTypeRef) extends FunctionID

    def specializedArrayCopy(arrayTypeRef: ArrayTypeRef): FunctionID = {
      val baseRef = arrayTypeRef match {
        case ArrayTypeRef(baseRef: PrimRef, 1) => baseRef
        case _                                 => ClassRef(ObjectClass)
      }
      SpecializedArrayCopyID(baseRef)
    }

    case object arrayCopyCheckBounds extends FunctionID
    case object slowRefArrayCopy extends FunctionID
    case object genericArrayCopy extends FunctionID
  }

  object genFieldID {
    final case class forClassInstanceField(name: FieldName) extends FieldID
    final case class forMethodTableEntry(methodName: MethodName) extends FieldID
    final case class captureParam(i: Int) extends FieldID

    object objStruct {
      case object vtable extends FieldID
      case object itables extends FieldID
      case object arrayUnderlying extends FieldID
    }

    object reflectiveProxy {
      case object methodID extends FieldID
      case object funcRef extends FieldID
    }

    /** Fields of the typeData structs. */
    object typeData {

      /** The name data as the 3 arguments to `stringLiteral`.
       *
       *  It is only meaningful for primitives and for classes. For array types, they are all 0, as
       *  array types compute their `name` from the `name` of their component type.
       */
      case object nameOffset extends FieldID

      /** See `nameOffset`. */
      case object nameSize extends FieldID

      /** See `nameOffset`. */
      case object nameStringIndex extends FieldID

      /** The kind of type data, an `i32`.
       *
       *  Possible values are the the `KindX` constants in `EmbeddedConstants`.
       */
      case object kind extends FieldID

      /** A bitset of special (primitive) instance types that are instances of this type, an `i32`.
       *
       *  From 0 to 5, the bits correspond to the values returned by the helper `jsValueType`. In
       *  addition, bits 6 and 7 represent `char` and `long`, respectively.
       */
      case object specialInstanceTypes extends FieldID

      /** Array of the strict ancestor classes of this class.
       *
       *  This is `null` for primitive and array types. For all other types, including JS types, it
       *  contains an array of the typeData of their ancestors that:
       *
       *  - are not themselves (hence the *strict* ancestors),
       *  - have typeData to begin with.
       */
      case object strictAncestors extends FieldID

      /** The typeData of a component of this array type, or `null` if this is not an array type.
       *
       *  For example:
       *
       *  - the `componentType` for class `Foo` is `null`,
       *  - the `componentType` for the array type `Array[Foo]` is the `typeData` of `Foo`.
       */
      case object componentType extends FieldID

      /** The name as nullable string (`anyref`), lazily initialized from the nameData.
       *
       *  This field is initialized by the `typeDataName` helper.
       *
       *  The contents of this value is specified by `java.lang.Class.getName()`. In particular, for
       *  array types, it obeys the following rules:
       *
       *  - `Array[prim]` where `prim` is a one of the primitive types with `charCode` `X` is
       *    `"[X"`, for example, `"[I"` for `Array[Int]`.
       *  - `Array[pack.Cls]` where `Cls` is a class is `"[Lpack.Cls;"`.
       *  - `Array[nestedArray]` where `nestedArray` is an array type with name `nested` is
       *    `"[nested"`, for example `"⟦I"` for `Array[Array[Int]]` and `"⟦Ljava.lang.String;"`
       *    for `Array[Array[String]]`.¹
       *
       *  ¹ We use the Unicode character `⟦` to represent two consecutive `[` characters in order
       *  not to confuse Scaladoc.
       */
      case object name extends FieldID

      /** The `classOf` value, a nullable `java.lang.Class`, lazily initialized from this typeData.
       *
       *  This field is initialized by the `createClassOf` helper.
       */
      case object classOfValue extends FieldID

      /** The typeData/vtable of an array of this type, a nullable `typeData`, lazily initialized.
       *
       *  This field is initialized by the `arrayTypeData` helper.
       *
       *  For example, once initialized,
       *
       *  - in the `typeData` of class `Foo`, it contains the `typeData` of `Array[Foo]`,
       *  - in the `typeData` of `Array[Int]`, it contains the `typeData` of `Array[Array[Int]]`.
       */
      case object arrayOf extends FieldID

      /** The function to clone the object of this type, a nullable function reference.
       *
       *  This field is initialized only with the classes that implement java.lang.Cloneable.
       */
      case object cloneFunction extends FieldID

      /** `isInstance` func ref for top-level JS classes. */
      case object isJSClassInstance extends FieldID

      /** The reflective proxies in this type, used for reflective call on the class at runtime.
       *
       *  This field contains an array of reflective proxy structs, where each struct contains the
       *  ID of the reflective proxy and a reference to the actual method implementation. Reflective
       *  call site should walk through the array to look up a method to call.
       *
       *  See `genSearchReflectivePRoxy` in `HelperFunctions`
       */
      case object reflectiveProxies extends FieldID
    }
  }

  object genTypeID {
    final case class forClass(className: ClassName) extends TypeID
    final case class captureData(index: Int) extends TypeID
    final case class forVTable(className: ClassName) extends TypeID
    final case class forITable(className: ClassName) extends TypeID
    final case class forFunction(index: Int) extends TypeID
    final case class forTableFunctionType(methodName: MethodName) extends TypeID

    val ObjectStruct = forClass(ObjectClass)
    val ClassStruct = forClass(ClassClass)
    val ThrowableStruct = forClass(ThrowableClass)
    val JSExceptionStruct = forClass(SpecialNames.JSExceptionClass)

    val ObjectVTable: TypeID = forVTable(ObjectClass)

    case object typeData extends TypeID
    case object reflectiveProxy extends TypeID

    // Array types -- they extend j.l.Object
    case object BooleanArray extends TypeID
    case object CharArray extends TypeID
    case object ByteArray extends TypeID
    case object ShortArray extends TypeID
    case object IntArray extends TypeID
    case object LongArray extends TypeID
    case object FloatArray extends TypeID
    case object DoubleArray extends TypeID
    case object ObjectArray extends TypeID

    def forArrayClass(arrayTypeRef: ArrayTypeRef): TypeID = {
      if (arrayTypeRef.dimensions > 1) {
        ObjectArray
      } else {
        arrayTypeRef.base match {
          case BooleanRef => BooleanArray
          case CharRef    => CharArray
          case ByteRef    => ByteArray
          case ShortRef   => ShortArray
          case IntRef     => IntArray
          case LongRef    => LongArray
          case FloatRef   => FloatArray
          case DoubleRef  => DoubleArray
          case _          => ObjectArray
        }
      }
    }

    case object typeDataArray extends TypeID
    case object itables extends TypeID
    case object reflectiveProxies extends TypeID

    // primitive array types, underlying the Array[T] classes
    case object i8Array extends TypeID
    case object i16Array extends TypeID
    case object i32Array extends TypeID
    case object i64Array extends TypeID
    case object f32Array extends TypeID
    case object f64Array extends TypeID
    case object anyArray extends TypeID

    def underlyingOf(arrayTypeRef: ArrayTypeRef): TypeID = {
      if (arrayTypeRef.dimensions > 1) {
        anyArray
      } else {
        arrayTypeRef.base match {
          case BooleanRef => i8Array
          case CharRef    => i16Array
          case ByteRef    => i8Array
          case ShortRef   => i16Array
          case IntRef     => i32Array
          case LongRef    => i64Array
          case FloatRef   => f32Array
          case DoubleRef  => f64Array
          case _          => anyArray
        }
      }
    }

    case object cloneFunctionType extends TypeID
    case object isJSClassInstanceFuncType extends TypeID
  }

  object genTagID {
    case object exception extends TagID
  }

  object genDataID {
    case object string extends DataID
  }

}
