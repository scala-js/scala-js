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

import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.mutable.LinkedHashMap

import org.scalajs.ir.ClassKind
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{FieldDef, ParamDef, JSNativeLoadSpec}
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard.{CoreSpec, LinkedClass, LinkedTopLevelExport}

import org.scalajs.linker.backend.webassembly.ModuleBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._
import org.scalajs.ir.OriginalName

final class WasmContext(
    val coreSpec: CoreSpec,
    val coreLib: CoreWasmLib,
    classInfo: Map[ClassName, WasmContext.ClassInfo],
    reflectiveProxies: Map[MethodName, Int],
    val itablesLength: Int
) {
  import WasmContext._

  private val functionTypes = LinkedHashMap.empty[watpe.FunctionType, wanme.TypeID]
  private val tableFunctionTypes = mutable.HashMap.empty[MethodName, wanme.TypeID]
  private val closureDataTypes = LinkedHashMap.empty[List[Type], wanme.TypeID]

  val moduleBuilder: ModuleBuilder = {
    new ModuleBuilder(new ModuleBuilder.FunctionTypeProvider {
      def functionTypeToTypeID(sig: watpe.FunctionType): wanme.TypeID = {
        functionTypes.getOrElseUpdate(
          sig, {
            val typeID = genTypeID.forFunction(functionTypes.size)
            moduleBuilder.addRecType(typeID, NoOriginalName, sig)
            typeID
          }
        )
      }
    })
  }

  private var nextClosureDataTypeIndex: Int = 1

  private val _funcDeclarations: mutable.LinkedHashSet[wanme.FunctionID] =
    new mutable.LinkedHashSet()

  val stringPool: StringPool = new StringPool

  /** The main `rectype` containing the object model types. */
  val mainRecType: ModuleBuilder.RecTypeBuilder = new ModuleBuilder.RecTypeBuilder

  def getClassInfoOption(name: ClassName): Option[ClassInfo] =
    classInfo.get(name)

  def getClassInfo(name: ClassName): ClassInfo =
    classInfo.getOrElse(name, throw new Error(s"Class not found: $name"))

  def inferTypeFromTypeRef(typeRef: TypeRef): Type = typeRef match {
    case PrimRef(tpe) =>
      tpe
    case ClassRef(className) =>
      if (className == ObjectClass || getClassInfo(className).kind.isJSType)
        AnyType
      else
        ClassType(className, nullable = true)
    case typeRef: ArrayTypeRef =>
      ArrayType(typeRef, nullable = true)
  }

  /** Retrieves a unique identifier for a reflective proxy with the given name.
   *
   *  If no class defines a reflective proxy with the given name, returns `-1`.
   */
  def getReflectiveProxyId(name: MethodName): Int =
    reflectiveProxies.getOrElse(name, -1)

  /** Adds or reuses a function type for a table function.
   *
   *  Table function types are part of the main `rectype`, and have names derived from the
   *  `methodName`.
   */
  def tableFunctionType(methodName: MethodName): wanme.TypeID = {
    // Project all the names with the same *signatures* onto a normalized `MethodName`
    val normalizedName = MethodName(
      SpecialNames.normalizedSimpleMethodName,
      methodName.paramTypeRefs,
      methodName.resultTypeRef,
      methodName.isReflectiveProxy
    )

    tableFunctionTypes.getOrElseUpdate(
      normalizedName, {
        val typeID = genTypeID.forTableFunctionType(normalizedName)
        val regularParamTyps = normalizedName.paramTypeRefs.map { typeRef =>
          TypeTransformer.transformParamType(inferTypeFromTypeRef(typeRef))(this)
        }
        val resultType = TypeTransformer.transformResultType(
            inferTypeFromTypeRef(normalizedName.resultTypeRef))(this)
        mainRecType.addSubType(
          typeID,
          NoOriginalName,
          watpe.FunctionType(watpe.RefType.any :: regularParamTyps, resultType)
        )
        typeID
      }
    )
  }

  def getClosureDataStructType(captureParamTypes: List[Type]): wanme.TypeID = {
    closureDataTypes.getOrElseUpdate(
      captureParamTypes, {
        val fields: List[watpe.StructField] = {
          for ((tpe, i) <- captureParamTypes.zipWithIndex) yield {
            watpe.StructField(
              genFieldID.captureParam(i),
              NoOriginalName,
              TypeTransformer.transformParamType(tpe)(this),
              isMutable = false
            )
          }
        }
        val structTypeID = genTypeID.captureData(nextClosureDataTypeIndex)
        nextClosureDataTypeIndex += 1
        val structType = watpe.StructType(fields)
        moduleBuilder.addRecType(structTypeID, NoOriginalName, structType)
        structTypeID
      }
    )
  }

  def refFuncWithDeclaration(funcID: wanme.FunctionID): wa.RefFunc = {
    _funcDeclarations += funcID
    wa.RefFunc(funcID)
  }

  def addGlobal(g: wamod.Global): Unit =
    moduleBuilder.addGlobal(g)

  def getAllFuncDeclarations(): List[wanme.FunctionID] =
    _funcDeclarations.toList
}

object WasmContext {
  final class ClassInfo(
      val name: ClassName,
      val kind: ClassKind,
      val jsClassCaptures: Option[List[ParamDef]],
      val allFieldDefs: List[FieldDef],
      val classImplementsAnyInterface: Boolean,
      val hasInstances: Boolean,
      val isAbstract: Boolean,
      val hasRuntimeTypeInfo: Boolean,
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
      val staticFieldMirrors: Map[FieldName, List[String]],
      _specialInstanceTypes: Int, // should be `val` but there is a large Scaladoc for it below
      val resolvedMethodInfos: Map[MethodName, ConcreteMethodInfo],
      val tableEntries: List[MethodName],
      _itableIdx: Int
  ) {
    override def toString(): String =
      s"ClassInfo(${name.nameString})"

    /** Returns the index of this interface's itable in the classes' interface tables.
     *
     *  Only interfaces that have instances get an itable index.
     */
    def itableIdx: Int = {
      if (_itableIdx < 0) {
        val isInterface = kind == ClassKind.Interface
        if (isInterface && hasInstances) {
          // it should have received an itable idx
          throw new IllegalStateException(
              s"$this was not assigned an itable index although it needs one.")
        } else {
          throw new IllegalArgumentException(
              s"Trying to ask the itable idx for $this, which is not supposed to have one " +
              s"(isInterface = $isInterface; hasInstances = $hasInstances).")
        }
      }
      _itableIdx
    }

    /** A bitset of the `jsValueType`s corresponding to hijacked classes that extend this class.
     *
     *  This value is used for instance tests against this class. A JS value `x` is an instance of
     *  this type iff `jsValueType(x)` is a member of this bitset. Because of how a bitset works,
     *  this means testing the following formula:
     *
     *  {{{
     *  ((1 << jsValueType(x)) & specialInstanceTypes) != 0
     *  }}}
     *
     *  For example, if this class is `Comparable`, we want the bitset to contain the values for
     *  `boolean`, `string` and `number` (but not `undefined`), because `jl.Boolean`, `jl.String`
     *  and `jl.Double` implement `Comparable`.
     *
     *  This field is initialized with 0, and augmented during preprocessing by calls to
     *  `addSpecialInstanceType`.
     *
     *  This technique is used both for static `isInstanceOf` tests as well as reflective tests
     *  through `Class.isInstance`. For the latter, this value is stored in
     *  `typeData.specialInstanceTypes`. For the former, it is embedded as a constant in the
     *  generated code.
     *
     *  See the `isInstance` and `genInstanceTest` helpers.
     *
     *  Special cases: this value remains 0 for all the numeric hijacked classes except `jl.Double`,
     *  since `jsValueType(x) == JSValueTypeNumber` is not enough to deduce that
     *  `x.isInstanceOf[Int]`, for example.
     */
    val specialInstanceTypes: Int = _specialInstanceTypes

    /** Is this class an ancestor of any hijacked class?
     *
     *  This includes but is not limited to the hijacked classes themselves, as well as `jl.Object`.
     */
    def isAncestorOfHijackedClass: Boolean =
      specialInstanceTypes != 0 || kind == ClassKind.HijackedClass

    def isInterface: Boolean =
      kind == ClassKind.Interface
  }

  final class ConcreteMethodInfo(val ownerClass: ClassName, val methodName: MethodName) {
    val tableEntryID = genFunctionID.forTableEntry(ownerClass, methodName)
  }
}
