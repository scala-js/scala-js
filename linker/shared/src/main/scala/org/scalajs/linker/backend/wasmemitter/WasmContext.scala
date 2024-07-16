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
import org.scalajs.linker.standard.LinkedTopLevelExport
import org.scalajs.linker.standard.LinkedClass

import org.scalajs.linker.backend.webassembly.ModuleBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import VarGen._
import org.scalajs.ir.OriginalName

final class WasmContext(
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
        ClassType(className)
    case typeRef: ArrayTypeRef =>
      ArrayType(typeRef)
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
          TypeTransformer.transformLocalType(inferTypeFromTypeRef(typeRef))(this)
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
              TypeTransformer.transformLocalType(tpe)(this),
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
      classConcretePublicMethodNames: List[MethodName],
      val allFieldDefs: List[FieldDef],
      superClass: Option[ClassInfo],
      val classImplementsAnyInterface: Boolean,
      val hasInstances: Boolean,
      val isAbstract: Boolean,
      val hasRuntimeTypeInfo: Boolean,
      val jsNativeLoadSpec: Option[JSNativeLoadSpec],
      val jsNativeMembers: Map[MethodName, JSNativeLoadSpec],
      val staticFieldMirrors: Map[FieldName, List[String]]
  ) {
    val resolvedMethodInfos: Map[MethodName, ConcreteMethodInfo] = {
      if (kind.isClass || kind == ClassKind.HijackedClass) {
        val inherited: Map[MethodName, ConcreteMethodInfo] = superClass match {
          case Some(superClass) => superClass.resolvedMethodInfos
          case None             => Map.empty
        }

        for (methodName <- classConcretePublicMethodNames)
          inherited.get(methodName).foreach(_.markOverridden())

        classConcretePublicMethodNames.foldLeft(inherited) { (prev, methodName) =>
          prev.updated(methodName, new ConcreteMethodInfo(name, methodName))
        }
      } else {
        Map.empty
      }
    }

    private val methodsCalledDynamically = mutable.HashSet.empty[MethodName]

    /** For a class or interface, its table entries in definition order. */
    private var _tableEntries: List[MethodName] = null

    private var _itableIdx: Int = -1

    def setItableIdx(idx: Int): Unit =
      _itableIdx = idx

    /** Returns the index of this interface's itable in the classes' interface tables. */
    def itableIdx: Int = {
      if (_itableIdx < 0)
        throw new IllegalArgumentException(s"$this was not assigned an itable index.")
      _itableIdx
    }

    private var _specialInstanceTypes: Int = 0

    def addSpecialInstanceType(jsValueType: Int): Unit =
      _specialInstanceTypes |= (1 << jsValueType)

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
    def specialInstanceTypes: Int = _specialInstanceTypes

    /** Is this class an ancestor of any hijacked class?
     *
     *  This includes but is not limited to the hijacked classes themselves, as well as `jl.Object`.
     */
    def isAncestorOfHijackedClass: Boolean =
      specialInstanceTypes != 0 || kind == ClassKind.HijackedClass

    def isInterface: Boolean =
      kind == ClassKind.Interface

    def registerDynamicCall(methodName: MethodName): Unit =
      methodsCalledDynamically += methodName

    def buildMethodTable(): Unit = {
      if (_tableEntries != null)
        throw new IllegalStateException(s"Duplicate call to buildMethodTable() for $name")

      kind match {
        case ClassKind.Class | ClassKind.ModuleClass | ClassKind.HijackedClass =>
          val superTableEntries = superClass.fold[List[MethodName]](Nil)(_.tableEntries)
          val superTableEntrySet = superTableEntries.toSet

          /* When computing the table entries to add for this class, exclude:
           * - methods that are already in the super class' table entries, and
           * - methods that are effectively final, since they will always be
           *   statically resolved instead of using the table dispatch.
           */
          val newTableEntries = methodsCalledDynamically.toList
            .filter(!superTableEntrySet.contains(_))
            .filterNot(m => resolvedMethodInfos.get(m).exists(_.isEffectivelyFinal))
            .sorted // for stability

          _tableEntries = superTableEntries ::: newTableEntries

        case ClassKind.Interface =>
          _tableEntries = methodsCalledDynamically.toList.sorted // for stability

        case _ =>
          _tableEntries = Nil
      }

      methodsCalledDynamically.clear() // gc
    }

    def tableEntries: List[MethodName] = {
      if (_tableEntries == null)
        throw new IllegalStateException(s"Table not yet built for $name")
      _tableEntries
    }
  }

  final class ConcreteMethodInfo(val ownerClass: ClassName, val methodName: MethodName) {
    val tableEntryID = genFunctionID.forTableEntry(ownerClass, methodName)

    private var effectivelyFinal: Boolean = true

    private[WasmContext] def markOverridden(): Unit =
      effectivelyFinal = false

    def isEffectivelyFinal: Boolean = effectivelyFinal
  }
}
