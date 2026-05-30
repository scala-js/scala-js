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

import java.nio.charset.StandardCharsets.UTF_8

import scala.collection.mutable

import scala.tools.nsc.Global

trait PrepWasmInterop[G <: Global with Singleton] { self: PrepJSInterop[G] =>

  import global._
  import definitions._
  import jsAddons._
  import jsDefinitions._

  private val wasmExportNames = mutable.Map.empty[Symbol, mutable.Map[String, Position]]
  private val utf8Encoder = UTF_8.newEncoder()

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
      checkWasmImportStringArg(annot, 0)
      checkWasmImportStringArg(annot, 1)
    }

    tree match {
      case tree: DefDef =>
        if (sym.isConstructor || sym.isAccessor) {
          reporter.error(tree.pos,
              "@WasmImport can only be used on methods")
        } else if (sym.isLocalToBlock) {
          reporter.error(tree.pos,
              "@WasmImport is not allowed on local definitions")
        } else if (!isWasmInteropAllowedOwner(sym)) {
          reporter.error(tree.pos,
              "@WasmImport can only be used on methods in static Scala objects")
        } else if (!sym.isPublic) {
          reporter.error(tree.pos,
              "@WasmImport can only be used on public methods")
        } else if (sym.typeParams.nonEmpty) {
          reporter.error(tree.pos,
              "@WasmImport methods may not have type parameters")
        } else if (tree.vparamss.isEmpty) {
          reporter.error(tree.pos,
              "@WasmImport methods must have a parameter list")
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
      checkWasmExportStringArg(annot)
    }

    tree match {
      case tree: DefDef =>
        if (sym.isConstructor || sym.isAccessor) {
          reporter.error(tree.pos,
              "@WasmExport can only be used on methods")
        } else if (sym.isLocalToBlock) {
          reporter.error(tree.pos,
              "@WasmExport is not allowed on local definitions")
        } else if (!isWasmInteropAllowedOwner(sym)) {
          reporter.error(tree.pos,
              "@WasmExport can only be used on methods in static Scala objects")
        } else if (!sym.isPublic) {
          reporter.error(tree.pos,
              "@WasmExport can only be used on public methods")
        } else if (sym.typeParams.nonEmpty) {
          reporter.error(tree.pos,
              "@WasmExport methods may not have type parameters")
        } else if (tree.vparamss.isEmpty) {
          reporter.error(tree.pos,
              "@WasmExport methods must have a parameter list")
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

  private def isWasmInteropAllowedOwner(sym: Symbol): Boolean =
    sym.owner.isModuleClass && sym.owner.isStatic && !isJSAny(sym.owner)

  private def checkWasmImportStringArg(annot: AnnotationInfo, idx: Int): Unit = {
    annot.stringArg(idx) match {
      case Some(value) =>
        if (!isValidUTF16String(value)) {
          reporter.error(annot.args(idx).pos,
              "The arguments to @WasmImport must be valid UTF-16 strings")
        }
      case None =>
        reporter.error(annot.args(idx).pos,
            "The arguments to @WasmImport must be literal strings")
    }
  }

  private def checkWasmExportStringArg(annot: AnnotationInfo): Unit = {
    annot.stringArg(0) match {
      case Some(value) =>
        if (!isValidUTF16String(value)) {
          reporter.error(annot.args.head.pos,
              "The argument to @WasmExport must be a valid UTF-16 string")
        }
      case None =>
        reporter.error(annot.args.head.pos,
            "The argument to @WasmExport must be a literal string")
    }
  }

  private def isValidUTF16String(str: String): Boolean =
    utf8Encoder.canEncode(str)

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
            "Wasm imports and exports only support Int, Long, Float, Double, " +
            "arrays of Byte, Short, Int, Long, Float and Double, " +
            "and Unit as result type")
      }
    }
    if (!isWasmInteropType(sym.tpe.resultType, isParam = false)) {
      reporter.error(sym.pos,
          "Wasm imports and exports only support Int, Long, Float, Double, " +
          "arrays of Byte, Short, Int, Long, Float and Double, " +
          "and Unit as result type")
    }
  }

  private def isWasmInteropType(tpe: Type, isParam: Boolean): Boolean = {
    val tpe1 = tpe.dealias
    val tsym = tpe1.typeSymbol
    (isWasmInteropPrimitiveType(tpe1) ||
      (!isParam && tsym == UnitClass) ||
      isSupportedWasmInteropArrayType(tpe1))
  }

  private def isWasmInteropPrimitiveType(tpe: Type): Boolean = tpe match {
    case ConstantType(_) =>
      false // reject literal type: ConstantType(1).typeSymbol => IntType
    case _ =>
      wasmInteropPrimitiveTypeClasses.contains(tpe.typeSymbol)
  }

  // Byte, Short, Int, Long, Float and Double.
  private lazy val wasmInteropArrayTypeClasses: Set[Symbol] =
    ScalaNumericValueClasses.filter(_ != CharClass).toSet

  // Int, Long, Float and Double.
  private lazy val wasmInteropPrimitiveTypeClasses: Set[Symbol] =
    wasmInteropArrayTypeClasses -- Set(ByteClass, ShortClass)

  private def isWasmInteropArrayTypeClass(sym: Symbol): Boolean =
    wasmInteropArrayTypeClasses.contains(sym)

  private def isSupportedWasmInteropArrayType(tpe: Type): Boolean = {
    tpe match {
      case TypeRef(_, sym, List(arg)) if sym == ArrayClass =>
        wasmInteropArrayTypeClasses.contains(arg.dealias.typeSymbol)
      case _ =>
        false
    }
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
