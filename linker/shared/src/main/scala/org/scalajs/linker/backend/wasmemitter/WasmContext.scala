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
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{FieldDef, ParamDef, JSNativeLoadSpec, WasmImportedMethodDef}
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.interface.{ModuleInitializer, ModuleKind}
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard.{CoreSpec, LinkedClass, LinkedTopLevelExport}

import org.scalajs.linker.backend.emitter.{NameGen => JSNameGen}

import org.scalajs.linker.backend.webassembly.ModuleBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import org.scalajs.linker.backend.javascript.{Trees => js}

import EmbeddedConstants._
import SpecialNames._
import TypeTransformer._
import VarGen._

final class WasmContext(
    val coreSpec: CoreSpec,
    val coreLib: CoreWasmLib,
    preprocessResult: Preprocessor.Result
) {
  import WasmContext._

  val useCustomDescriptors = coreSpec.wasmFeatures.experimentalUseCustomDescriptors
  val hasJSInterop: Boolean = coreSpec.moduleKind == ModuleKind.ESModule

  val privateJSFields: Map[FieldName, String] = preprocessResult.privateJSFields
  val itablesLength: Int = preprocessResult.itablesLength

  /** Hijacked classes that we must emit as real box classes. */
  val hijackedClassesWithBoxes: Set[ClassName] = {
    if (hasJSInterop) {
      Set(
        BoxedCharacterClass,
        BoxedLongClass
      )
    } else {
      Set(
        BoxedUnitClass,
        BoxedBooleanClass,
        BoxedCharacterClass,
        BoxedLongClass,
        BoxedDoubleClass,
        BoxedStringClass
      )
    }
  }

  private val tableFunctionTypes = mutable.HashMap.empty[MethodName, wanme.TypeID]
  private val closureDataTypes = LinkedHashMap.empty[List[Type], wanme.TypeID]
  private val typedClosureTypes = LinkedHashMap.empty[ClosureType, (wanme.TypeID, wanme.TypeID)]

  val jsNameGen = new JSNameGen()

  private val customJSHelpers = mutable.ListBuffer.empty[(String, js.Function)]

  val moduleBuilder: ModuleBuilder = new ModuleBuilder

  def addCustomJSHelper(jsFunction: js.Function, wasmType: watpe.FunctionType): wanme.FunctionID = {
    val id = CustomJSHelperFunctionID(customJSHelpers.size)
    moduleBuilder.addImport(
      wamod.Import(
        CustomHelpersModule,
        id.importName,
        wamod.ImportDesc.Func(
          id,
          OriginalName(id.toString()),
          moduleBuilder.functionTypeToTypeID(wasmType)
        )
      )
    )
    customJSHelpers += ((id.importName, jsFunction))
    id
  }

  private var nextClosureDataTypeIndex: Int = 1

  private val _funcDeclarations: mutable.LinkedHashSet[wanme.FunctionID] =
    new mutable.LinkedHashSet()

  val stringType: watpe.RefType =
    if (hasJSInterop) watpe.RefType.extern // for all the JS string builtin functions
    else watpe.RefType(genTypeID.StringStruct)

  val stringPool: StringPool =
    if (hasJSInterop) new JSStringPool
    else new DataStringPool

  val constantArrayPool: ConstantArrayPool = new ConstantArrayPool

  /** The main `rectype` containing the object model types. */
  val mainRecType: ModuleBuilder.RecTypeBuilder = new ModuleBuilder.RecTypeBuilder

  def getClassInfoOption(name: ClassName): Option[ClassInfo] =
    preprocessResult.classInfos.get(name)

  def getClassInfo(name: ClassName): ClassInfo =
    preprocessResult.classInfos.getOrElse(name, throw new Error(s"Class not found: $name"))

  /** Retrieves a unique identifier for a reflective proxy with the given name.
   *
   *  If no class defines a reflective proxy with the given name, returns `-1`.
   */
  def getReflectiveProxyId(name: MethodName): Int =
    preprocessResult.reflectiveProxies.getOrElse(name, -1)

  /** Adds or reuses a function type for a table function.
   *
   *  Table function types are part of the main `rectype`, and have names derived from the
   *  `methodName`.
   */
  def tableFunctionType(methodName: MethodName): wanme.TypeID = {
    val normalizedName = normalizeTableMethodName(methodName)

    tableFunctionTypes.getOrElseUpdate(normalizedName, {
      val subType = transformTableFunctionType(normalizedName)(this)
      mainRecType.addSubType(subType)
      subType.id
    })
  }

  def getClosureDataStructType(captureParamTypes: List[Type]): wanme.TypeID = {
    closureDataTypes.getOrElseUpdate(captureParamTypes, {
      val structType = transformClosureDataStructType(captureParamTypes)(this)
      val structTypeID = genTypeID.captureData(nextClosureDataTypeIndex)
      nextClosureDataTypeIndex += 1
      moduleBuilder.addRecType(structTypeID, NoOriginalName, structType)
      structTypeID
    })
  }

  /** Generates the struct type for a `ClosureType`.
   *
   *  @return
   *    `(funTypeID, structTypeID)`, where `funTypeID` is the function type of
   *    the `ref.func`s, and `structTypeID` is the struct type that contains
   *    the capture data and the `ref.func` (i.e., the actual Wasm type of
   *    values of the given `ClosureType`).
   */
  def genTypedClosureStructType(tpe0: ClosureType): (wanme.TypeID, wanme.TypeID) = {
    // Normalize to the non-nullable variant
    val tpe = tpe0.toNonNullable

    typedClosureTypes.getOrElseUpdate(tpe, {
      val (funSubType, structSubType) = transformTypedClosureType(tpe)(this)

      mainRecType.addSubType(funSubType)
      mainRecType.addSubType(structSubType)

      (funSubType.id, structSubType.id)
    })
  }

  def refFuncWithDeclaration(funcID: wanme.FunctionID): wa.RefFunc = {
    _funcDeclarations += funcID
    wa.RefFunc(funcID)
  }

  def getVTableInstr(typeID: wanme.TypeID): wa.Instr =
    if (useCustomDescriptors) wa.RefGetDesc(typeID)
    else wa.StructGet(typeID, genFieldID.objStruct.vtable)

  def addGlobal(g: wamod.Global): Unit =
    moduleBuilder.addGlobal(g)

  def getAllCustomJSHelpers(): List[(String, js.Function)] =
    customJSHelpers.toList

  def getAllFuncDeclarations(): List[wanme.FunctionID] =
    _funcDeclarations.toList
}

object WasmContext {
  private final case class CustomJSHelperFunctionID(index: Int) extends wanme.FunctionID {
    override def toString(): String = s"customJSHelper.$index"

    val importName: String = index.toString()
  }
}
