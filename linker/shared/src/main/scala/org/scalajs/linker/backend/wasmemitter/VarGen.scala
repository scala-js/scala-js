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

import org.scalajs.ir.Names.{FieldName => IRFieldName, _}
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp, MemberNamespace}
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.webassembly.Identitities._

/** Manages generation of non-local IDs.
 *
 *  `LocalID`s and `LabelID`s are directly managed by `FunctionBuilder` instead.
 */
object VarGen {

  object genGlobalID {
    private final case class ImportedModuleID(moduleName: String) extends GlobalID
    private final case class ModuleInstanceID(className: ClassName) extends GlobalID
    private final case class JSClassValueID(className: ClassName) extends GlobalID
    private final case class VTableID(typeRef: NonArrayTypeRef) extends GlobalID
    private final case class ITableID(className: ClassName) extends GlobalID
    private final case class StaticFieldID(fieldName: IRFieldName) extends GlobalID
    private final case class JSPrivateFieldID(fieldName: IRFieldName) extends GlobalID

    def forImportedModule(moduleName: String): GlobalID =
      ImportedModuleID(moduleName)

    def forModuleInstance(className: ClassName): GlobalID =
      ModuleInstanceID(className)

    def forJSClassValue(className: ClassName): GlobalID =
      JSClassValueID(className)

    def forVTable(className: ClassName): GlobalID =
      forVTable(ClassRef(className))

    def forVTable(typeRef: NonArrayTypeRef): GlobalID =
      VTableID(typeRef)

    def forITable(className: ClassName): GlobalID =
      ITableID(className)

    def forStaticField(fieldName: IRFieldName): GlobalID =
      StaticFieldID(fieldName)

    def forJSPrivateField(fieldName: IRFieldName): GlobalID =
      JSPrivateFieldID(fieldName)

    /** A `GlobalID` for a JS helper global.
     *
     *  Its `toString()` is guaranteed to correspond to the import name of the helper.
     */
    sealed abstract class JSHelperGlobalID extends GlobalID

    case object undef extends JSHelperGlobalID

    case object bFalse extends JSHelperGlobalID

    case object bZero extends JSHelperGlobalID

    case object bZeroChar extends GlobalID

    case object bZeroLong extends GlobalID

    case object emptyString extends JSHelperGlobalID

    case object stringLiteralCache extends GlobalID

    case object arrayClassITable extends GlobalID

    case object lastIDHashCode extends GlobalID

    case object idHashCodeMap extends JSHelperGlobalID
  }

  object genFunctionID {
    private final case class MethodID(namespace: MemberNamespace,
        className: ClassName, methodName: MethodName)
        extends FunctionID

    private final case class TableEntryID(className: ClassName, methodName: MethodName)
        extends FunctionID

    private final case class ExportID(exportedName: String) extends FunctionID
    private final case class TopLevelExportSetterID(exportedName: String) extends FunctionID

    private final case class LoadModuleID(className: ClassName) extends FunctionID
    private final case class NewDefaultID(className: ClassName) extends FunctionID
    private final case class InstanceTestID(className: ClassName) extends FunctionID
    private final case class CloneID(className: ClassName) extends FunctionID
    private final case class CloneArrayID(arrayBaseRef: NonArrayTypeRef) extends FunctionID

    private final case class IsJSClassInstanceID(className: ClassName) extends FunctionID
    private final case class LoadJSClassID(className: ClassName) extends FunctionID
    private final case class CreateJSClassOfID(className: ClassName) extends FunctionID
    private final case class PreSuperStatsID(className: ClassName) extends FunctionID
    private final case class SuperArgsID(className: ClassName) extends FunctionID
    private final case class PostSuperStatsID(className: ClassName) extends FunctionID

    def forMethod(namespace: MemberNamespace, clazz: ClassName, method: MethodName): FunctionID =
      MethodID(namespace, clazz, method)
    def forTableEntry(clazz: ClassName, method: MethodName): FunctionID =
      TableEntryID(clazz, method)

    def forExport(exportedName: String): FunctionID =
      ExportID(exportedName)
    def forTopLevelExportSetter(exportedName: String): FunctionID =
      TopLevelExportSetterID(exportedName)

    def loadModule(clazz: ClassName): FunctionID =
      LoadModuleID(clazz)
    def newDefault(clazz: ClassName): FunctionID =
      NewDefaultID(clazz)
    def instanceTest(clazz: ClassName): FunctionID =
      InstanceTestID(clazz)
    def clone(clazz: ClassName): FunctionID =
      CloneID(clazz)
    def clone(arrayBaseRef: NonArrayTypeRef): FunctionID =
      CloneArrayID(arrayBaseRef)

    def isJSClassInstance(clazz: ClassName): FunctionID =
      IsJSClassInstanceID(clazz)
    def loadJSClass(clazz: ClassName): FunctionID =
      LoadJSClassID(clazz)
    def createJSClassOf(clazz: ClassName): FunctionID =
      CreateJSClassOfID(clazz)
    def preSuperStats(clazz: ClassName): FunctionID =
      PreSuperStatsID(clazz)
    def superArgs(clazz: ClassName): FunctionID =
      SuperArgsID(clazz)
    def postSuperStats(clazz: ClassName): FunctionID =
      PostSuperStatsID(clazz)

    case object start extends FunctionID

    // JS helpers

    /** A `FunctionID` for a JS helper function.
     *
     *  Its `toString()` is guaranteed to correspond to the import name of the helper.
     */
    sealed abstract class JSHelperFunctionID extends FunctionID

    case object is extends JSHelperFunctionID

    case object isUndef extends JSHelperFunctionID

    private final case class BoxID(primRef: PrimRef) extends JSHelperFunctionID {
      override def toString(): String = "b" + primRef.charCode
    }

    private final case class UnboxID(primRef: PrimRef) extends JSHelperFunctionID {
      override def toString(): String = "u" + primRef.charCode
    }

    private final case class TypeTestID(primRef: PrimRef) extends JSHelperFunctionID {
      override def toString(): String = "t" + primRef.charCode
    }

    def box(primRef: PrimRef): JSHelperFunctionID = BoxID(primRef)
    def unbox(primRef: PrimRef): JSHelperFunctionID = UnboxID(primRef)
    def typeTest(primRef: PrimRef): JSHelperFunctionID = TypeTestID(primRef)

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
    case object jsLinkingInfo extends JSHelperFunctionID

    private final case class JSUnaryOpHelperID(name: String) extends JSHelperFunctionID {
      override def toString(): String = name
    }

    val jsUnaryOps: Map[JSUnaryOp.Code, JSHelperFunctionID] = {
      Map(
        JSUnaryOp.+ -> JSUnaryOpHelperID("jsUnaryPlus"),
        JSUnaryOp.- -> JSUnaryOpHelperID("jsUnaryMinus"),
        JSUnaryOp.~ -> JSUnaryOpHelperID("jsUnaryTilde"),
        JSUnaryOp.! -> JSUnaryOpHelperID("jsUnaryBang"),
        JSUnaryOp.typeof -> JSUnaryOpHelperID("jsUnaryTypeof")
      )
    }

    private final case class JSBinaryOpHelperID(name: String) extends JSHelperFunctionID {
      override def toString(): String = name
    }

    val jsBinaryOps: Map[JSBinaryOp.Code, JSHelperFunctionID] = {
      Map(
        JSBinaryOp.=== -> JSBinaryOpHelperID("jsStrictEquals"),
        JSBinaryOp.!== -> JSBinaryOpHelperID("jsNotStrictEquals"),
        JSBinaryOp.+ -> JSBinaryOpHelperID("jsPlus"),
        JSBinaryOp.- -> JSBinaryOpHelperID("jsMinus"),
        JSBinaryOp.* -> JSBinaryOpHelperID("jsTimes"),
        JSBinaryOp./ -> JSBinaryOpHelperID("jsDivide"),
        JSBinaryOp.% -> JSBinaryOpHelperID("jsModulus"),
        JSBinaryOp.| -> JSBinaryOpHelperID("jsBinaryOr"),
        JSBinaryOp.& -> JSBinaryOpHelperID("jsBinaryAnd"),
        JSBinaryOp.^ -> JSBinaryOpHelperID("jsBinaryXor"),
        JSBinaryOp.<< -> JSBinaryOpHelperID("jsShiftLeft"),
        JSBinaryOp.>> -> JSBinaryOpHelperID("jsArithmeticShiftRight"),
        JSBinaryOp.>>> -> JSBinaryOpHelperID("jsLogicalShiftRight"),
        JSBinaryOp.< -> JSBinaryOpHelperID("jsLessThan"),
        JSBinaryOp.<= -> JSBinaryOpHelperID("jsLessEqual"),
        JSBinaryOp.> -> JSBinaryOpHelperID("jsGreaterThan"),
        JSBinaryOp.>= -> JSBinaryOpHelperID("jsGreaterEqual"),
        JSBinaryOp.in -> JSBinaryOpHelperID("jsIn"),
        JSBinaryOp.instanceof -> JSBinaryOpHelperID("jsInstanceof"),
        JSBinaryOp.** -> JSBinaryOpHelperID("jsExponent")
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
    case object isInstance extends FunctionID
    case object isAssignableFromExternal extends FunctionID
    case object isAssignableFrom extends FunctionID
    case object checkCast extends FunctionID
    case object getComponentType extends FunctionID
    case object newArrayOfThisClass extends FunctionID
    case object anyGetClass extends FunctionID
    case object newArrayObject extends FunctionID
    case object identityHashCode extends FunctionID
    case object searchReflectiveProxy extends FunctionID
  }

  object genFieldID {
    private final case class ClassInstanceFieldID(name: IRFieldName) extends FieldID
    private final case class MethodTableEntryID(methodName: MethodName) extends FieldID
    private final case class CaptureParamID(i: Int) extends FieldID

    def forClassInstanceField(name: IRFieldName): FieldID =
      ClassInstanceFieldID(name)

    def forMethodTableEntry(name: MethodName): FieldID =
      MethodTableEntryID(name)

    def captureParam(i: Int): FieldID =
      CaptureParamID(i)

    object objStruct {
      case object vtable extends FieldID
      case object itables extends FieldID
      case object arrayUnderlying extends FieldID
    }

    object reflectiveProxy {
      case object func_name extends FieldID
      case object func_ref extends FieldID
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
    private final case class ClassStructID(className: ClassName) extends TypeID
    private final case class CaptureDataID(index: Int) extends TypeID
    private final case class VTableID(className: ClassName) extends TypeID
    private final case class ITableID(className: ClassName) extends TypeID
    private final case class FunctionTypeID(index: Int) extends TypeID
    private final case class TableFunctionTypeID(methodName: MethodName) extends TypeID

    def forClass(name: ClassName): TypeID =
      ClassStructID(name)

    val ObjectStruct = forClass(ObjectClass)
    val ClassStruct = forClass(ClassClass)
    val ThrowableStruct = forClass(ThrowableClass)
    val JSExceptionStruct = forClass(SpecialNames.JSExceptionClass)

    def captureData(index: Int): TypeID =
      CaptureDataID(index)

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

    def forVTable(className: ClassName): TypeID =
      VTableID(className)

    val ObjectVTable: TypeID = forVTable(ObjectClass)

    def forITable(className: ClassName): TypeID =
      ITableID(className)

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

    def forFunction(idx: Int): TypeID = FunctionTypeID(idx)

    case object cloneFunctionType extends TypeID
    case object isJSClassInstanceFuncType extends TypeID

    def forTableFunctionType(methodName: MethodName): TypeID =
      TableFunctionTypeID(methodName)
  }

  object genTagID {
    case object exception extends TagID
  }

  object genDataID {
    case object string extends DataID
  }

}
