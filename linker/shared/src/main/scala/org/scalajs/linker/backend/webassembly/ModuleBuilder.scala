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

package org.scalajs.linker.backend.webassembly

import scala.collection.mutable

import org.scalajs.ir.OriginalName

import Identitities._
import Modules._
import Types._

final class ModuleBuilder(functionSignatureProvider: ModuleBuilder.FunctionTypeProvider) {
  import ModuleBuilder._

  /** Items are `RecType | RecTypeBuilder`. */
  private val types: mutable.ListBuffer[AnyRef] = new mutable.ListBuffer()

  private val imports: mutable.ListBuffer[Import] = new mutable.ListBuffer()
  private val funcs: mutable.ListBuffer[Function] = new mutable.ListBuffer()
  private val tags: mutable.ListBuffer[Tag] = new mutable.ListBuffer()
  private val globals: mutable.ListBuffer[Global] = new mutable.ListBuffer()
  private val exports: mutable.ListBuffer[Export] = new mutable.ListBuffer()
  private var start: Option[FunctionID] = None
  private val elems: mutable.ListBuffer[Element] = new mutable.ListBuffer()
  private val datas: mutable.ListBuffer[Data] = new mutable.ListBuffer()

  def functionTypeToTypeID(sig: FunctionType): TypeID =
    functionSignatureProvider.functionTypeToTypeID(sig)

  def addRecType(recType: RecType): Unit = types += recType
  def addRecType(subType: SubType): Unit = addRecType(RecType(subType))

  def addRecType(id: TypeID, originalName: OriginalName, compositeType: CompositeType): Unit =
    addRecType(SubType(id, originalName, compositeType))

  def addRecTypeBuilder(recTypeBuilder: RecTypeBuilder): Unit =
    types += recTypeBuilder

  def addImport(imprt: Import): Unit = imports += imprt
  def addFunction(function: Function): Unit = funcs += function
  def addTag(tag: Tag): Unit = tags += tag
  def addGlobal(global: Global): Unit = globals += global
  def addExport(exprt: Export): Unit = exports += exprt
  def setStart(startFunction: FunctionID): Unit = start = Some(startFunction)
  def addElement(element: Element): Unit = elems += element
  def addData(data: Data): Unit = datas += data

  def build(): Module = {
    val builtTypes: List[RecType] = types.toList.map {
      case tpe: RecType            => tpe
      case builder: RecTypeBuilder => builder.build()
    }

    new Module(
      builtTypes,
      imports.toList,
      funcs.toList,
      tags.toList,
      globals.toList,
      exports.toList,
      start,
      elems.toList,
      datas.toList
    )
  }
}

object ModuleBuilder {
  trait FunctionTypeProvider {
    def functionTypeToTypeID(sig: FunctionType): TypeID
  }

  final class RecTypeBuilder {
    private val subTypes = mutable.ListBuffer.empty[SubType]

    def addSubType(subType: SubType): Unit =
      subTypes += subType

    def addSubType(id: TypeID, originalName: OriginalName, compositeType: CompositeType): Unit =
      addSubType(SubType(id, originalName, compositeType))

    def build(): RecType = RecType(subTypes.toList)
  }
}
