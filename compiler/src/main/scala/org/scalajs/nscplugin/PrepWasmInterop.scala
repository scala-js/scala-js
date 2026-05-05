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

import scala.collection.mutable

import scala.tools.nsc.Global

trait PrepWasmInterop[G <: Global with Singleton] { self: PrepJSInterop[G] =>

  import global._
  import definitions._
  import jsAddons._
  import jsDefinitions._

  private val wasmExportNames = mutable.Map.empty[Symbol, mutable.Map[String, Position]]

  protected def checkWasmInteropAnnotations(tree: MemberDef, shouldCheckLiterals: Boolean): Unit = {
    val sym = moduleToModuleClass(tree.symbol)
    val importAnnot = sym.getAnnotation(WasmImportAnnotation)
    val exportAnnot = sym.getAnnotation(WasmExportAnnotation)

    for (annot <- importAnnot)
      checkWasmImportAnnotation(tree, sym, annot, shouldCheckLiterals)

    for (annot <- exportAnnot)
      checkWasmExportAnnotation(tree, sym, annot, shouldCheckLiterals)
  }

  private def checkWasmImportAnnotation(tree: MemberDef, sym: Symbol, annot: AnnotationInfo,
      shouldCheckLiterals: Boolean): Unit = {
    if (shouldCheckLiterals) {
      annot.stringArg(0) match {
        case Some(moduleName) =>
          if (moduleName.isEmpty) {
            reporter.error(annot.args.head.pos,
                "The first argument to @WasmImport must be non-empty")
          }
        case None =>
          reporter.error(annot.args.head.pos,
              "The first argument to @WasmImport must be a literal string")
      }

      annot.stringArg(1) match {
        case Some(functionName) =>
          if (functionName.isEmpty) {
            reporter.error(annot.args(1).pos,
                "The second argument to @WasmImport must be non-empty")
          }
        case None =>
          reporter.error(annot.args(1).pos,
              "The second argument to @WasmImport must be a literal string")
      }
    }

    tree match {
      case tree: DefDef =>
        if (sym.isConstructor || sym.isAccessor) {
          reporter.error(tree.pos,
              "@WasmImport can only be used on methods")
        } else if (sym.isLocalToBlock) {
          reporter.error(tree.pos,
              "@WasmImport is not allowed on local definitions")
        } else if (!isWasmImportAllowedOwner(sym)) {
          reporter.error(tree.pos,
              "@WasmImport can only be used on methods in static Scala objects")
        } else if (!sym.isPublic) {
          reporter.error(tree.pos,
              "@WasmImport can only be used on public methods")
        } else if (sym.typeParams.nonEmpty) {
          reporter.error(tree.pos,
              "@WasmImport methods may not have type parameters")
        } else if (tree.vparamss.size > 1) {
          reporter.error(tree.pos,
              "@WasmImport methods may not have multiple parameter lists")
        } else if (sym.hasAnnotation(JSNativeAnnotation)) {
          reporter.error(annot.pos,
              "@WasmImport cannot be used together with @js.native")
        } else if (sym.isDeferred) {
          reporter.error(tree.pos,
              "@WasmImport can only be used on concrete methods")
        } else {
          checkRHSCallsWasmNative(tree)
          checkWasmInteropMethodType(sym, "@WasmImport")
        }
      case _ =>
        reporter.error(tree.pos,
            "@WasmImport can only be used on methods")
    }
  }

  private def checkWasmExportAnnotation(tree: MemberDef, sym: Symbol,
      annot: AnnotationInfo, shouldCheckLiterals: Boolean): Unit = {
    val exportName = annot.stringArg(0)

    if (shouldCheckLiterals) {
      exportName match {
        case Some(name) =>
          if (name.isEmpty) {
            reporter.error(annot.args.head.pos,
                "The argument to @WasmExport must be non-empty")
          }
        case None =>
          reporter.error(annot.args.head.pos,
              "The argument to @WasmExport must be a literal string")
      }
    }

    tree match {
      case tree: DefDef =>
        if (sym.isConstructor || sym.isAccessor) {
          reporter.error(tree.pos,
              "@WasmExport can only be used on methods")
        } else if (sym.isLocalToBlock) {
          reporter.error(tree.pos,
              "@WasmExport is not allowed on local definitions")
        } else if (!isWasmExportAllowedOwner(sym)) {
          reporter.error(tree.pos,
              "@WasmExport can only be used on methods in static Scala objects")
        } else if (!sym.isPublic) {
          reporter.error(tree.pos,
              "@WasmExport can only be used on public methods")
        } else if (sym.typeParams.nonEmpty) {
          reporter.error(tree.pos,
              "@WasmExport methods may not have type parameters")
        } else if (tree.vparamss.size > 1) {
          reporter.error(tree.pos,
              "@WasmExport methods may not have multiple parameter lists")
        } else if (sym.hasAnnotation(JSNativeAnnotation)) {
          reporter.error(annot.pos,
              "@WasmExport cannot be used together with @js.native")
        } else if (sym.isDeferred) {
          reporter.error(tree.pos,
              "@WasmExport can only be used on concrete methods")
        } else {
          checkWasmInteropMethodType(sym, "@WasmExport")

          for (name <- exportName) {
            val namesForOwner = wasmExportNames.getOrElseUpdate(sym.owner, mutable.Map.empty)
            namesForOwner.get(name) match {
              case Some(pos) =>
                reporter.error(annot.pos,
                    s"Duplicate @WasmExport name '$name' already used at $pos")
              case None =>
                namesForOwner.put(name, annot.pos)
            }
          }
        }

      case _ =>
        reporter.error(tree.pos,
            "@WasmExport can only be used on methods")
    }
  }

  private def isWasmImportAllowedOwner(sym: Symbol): Boolean =
    sym.owner.isModuleClass && sym.owner.isStatic && !isJSAny(sym.owner)

  private def isWasmExportAllowedOwner(sym: Symbol): Boolean =
    sym.owner.isModuleClass && sym.owner.isStatic && !isJSAny(sym.owner)

  private def checkWasmInteropMethodType(sym: Symbol, subject: String): Unit = {
    for (param <- sym.paramss.flatten) {
      if (isScalaRepeatedParamType(param.tpe)) {
        reporter.error(param.pos,
            s"$subject methods may not have repeated parameters")
      } else if (param.isParamWithDefault) {
        reporter.error(param.pos,
            s"$subject methods may not have default parameters")
      } else if (!isWasmInteropType(param.tpe, isParam = true)) {
        reporter.error(param.pos,
            "Wasm imports and exports only support primitive types and " +
            "Array types as parameters")
      }
    }
    if (!isWasmInteropType(sym.tpe.resultType, isParam = false)) {
      reporter.error(sym.pos,
          "Wasm imports and exports only support primitive types, " +
          "Array types and Unit as result types")
    }
  }

  private def isWasmInteropType(tpe: Type, isParam: Boolean): Boolean = {
    val tsym = tpe.dealias.widen.typeSymbol
    tsym == ArrayClass ||
      tsym == BooleanClass ||
      tsym == ByteClass ||
      tsym == ShortClass ||
      tsym == IntClass ||
      tsym == CharClass ||
      tsym == LongClass ||
      tsym == FloatClass ||
      tsym == DoubleClass ||
      (!isParam && tsym == UnitClass)
  }

  private def checkRHSCallsWasmNative(tree: ValOrDefDef): Unit = {
    tree.rhs match {
      case sel: Select if sel.symbol == WasmPackage_native =>
        // ok
      case _ =>
        val pos = if (tree.rhs != EmptyTree) tree.rhs.pos else tree.pos
        reporter.error(pos, "@WasmImport methods may only call wasm.native.")
    }
  }

  private def moduleToModuleClass(sym: Symbol): Symbol =
    if (sym.isModule) sym.moduleClass
    else sym
}
