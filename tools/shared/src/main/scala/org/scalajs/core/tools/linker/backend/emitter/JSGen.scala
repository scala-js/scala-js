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

package org.scalajs.core.tools.linker.backend.emitter

import scala.language.implicitConversions

import org.scalajs.core.ir
import ir._
import ir.Definitions._
import ir.Types._
import ir.{Trees => irt}

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.linker.backend.{ModuleKind, OutputMode}
import org.scalajs.core.tools.javascript.Trees._

import EmitterDefinitions._

/** Collection of tree generators that are used accross the board.
 *  This class is fully stateless.
 *
 *  Also carries around config (semantics and outputMode).
 */
private[emitter] final class JSGen(val semantics: Semantics,
    val outputMode: OutputMode, val moduleKind: ModuleKind,
    internalOptions: InternalOptions) {
  import JSGen._

  implicit def transformIdent(ident: irt.Ident): Ident =
    Ident(ident.name, ident.originalName)(ident.pos)

  def genZeroOf(tpe: Type)(implicit pos: Position): Tree = {
    tpe match {
      case BooleanType => BooleanLiteral(false)
      case IntType     => IntLiteral(0)
      case LongType    => genLongZero()
      case FloatType   => DoubleLiteral(0.0)
      case DoubleType  => DoubleLiteral(0.0)
      case StringType  => StringLiteral("")
      case UndefType   => Undefined()
      case _           => Null()
    }
  }

  def genLongZero()(implicit pos: Position): Tree = {
    genLongModuleApply(LongImpl.Zero)
  }

  def genLongModuleApply(methodName: String, args: Tree*)(
      implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(
        genLoadModule(LongImpl.RuntimeLongModuleClass) DOT methodName,
        args.toList)
  }

  def genLet(name: Ident, mutable: Boolean, rhs: Tree)(
      implicit pos: Position): LocalDef = {
    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        VarDef(name, Some(rhs))
      case OutputMode.ECMAScript6 =>
        Let(name, mutable, Some(rhs))
    }
  }

  def genEmptyMutableLet(name: Ident)(implicit pos: Position): LocalDef = {
    outputMode match {
      case OutputMode.ECMAScript51Global | OutputMode.ECMAScript51Isolated =>
        VarDef(name, rhs = None)
      case OutputMode.ECMAScript6 =>
        Let(name, mutable = true, rhs = None)
    }
  }

  def genSelectStatic(className: String, item: irt.Ident)(
      implicit pos: Position): Tree = {
    envField("t", className + "__" + item.name)
  }

  def genIsInstanceOf(expr: Tree, cls: ReferenceType)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): Tree = {
    import TreeDSL._

    cls match {
      case ClassType(className) =>
        if (!HijackedClassesAndTheirSuperClasses.contains(className) &&
            !globalKnowledge.isInterface(className)) {
          expr instanceof encodeClassVar(className)
        } else if (className == BoxedLongClass) {
          expr instanceof encodeClassVar(LongImpl.RuntimeLongClass)
        } else {
          genIsAsInstanceOf(expr, cls, test = true)
        }
      case ArrayType(_, _)  =>
        genIsAsInstanceOf(expr, cls, test = true)
    }
  }

  def genAsInstanceOf(expr: Tree, cls: ReferenceType)(
      implicit pos: Position): Tree =
    genIsAsInstanceOf(expr, cls, test = false)

  private def genIsAsInstanceOf(expr: Tree, cls: ReferenceType, test: Boolean)(
      implicit pos: Position): Tree = {
    import TreeDSL._

    cls match {
      case ClassType(className0) =>
        val className =
          if (className0 == BoxedLongClass) LongImpl.RuntimeLongClass
          else className0

        if (HijackedBoxedClasses.contains(className)) {
          if (test) {
            className match {
              case BoxedUnitClass    => expr === Undefined()
              case BoxedBooleanClass => typeof(expr) === "boolean"
              case BoxedByteClass    => genCallHelper("isByte", expr)
              case BoxedShortClass   => genCallHelper("isShort", expr)
              case BoxedIntegerClass => genCallHelper("isInt", expr)
              case BoxedFloatClass   => genCallHelper("isFloat", expr)
              case BoxedDoubleClass  => typeof(expr) === "number"
            }
          } else {
            className match {
              case BoxedUnitClass    => genCallHelper("asUnit", expr)
              case BoxedBooleanClass => genCallHelper("asBoolean", expr)
              case BoxedByteClass    => genCallHelper("asByte", expr)
              case BoxedShortClass   => genCallHelper("asShort", expr)
              case BoxedIntegerClass => genCallHelper("asInt", expr)
              case BoxedFloatClass   => genCallHelper("asFloat", expr)
              case BoxedDoubleClass  => genCallHelper("asDouble", expr)
            }
          }
        } else {
          Apply(
              envField(if (test) "is" else "as", className),
              List(expr))
        }

      case ArrayType(base, depth) =>
        Apply(
            envField(if (test) "isArrayOf" else "asArrayOf", base),
            List(expr, IntLiteral(depth)))
    }
  }

  def genCallHelper(helperName: String, args: Tree*)(
      implicit pos: Position): Tree = {
    Apply(envField(helperName), args.toList)
  }

  def encodeClassVar(className: String)(implicit pos: Position): Tree =
    envField("c", className)

  def genLoadModule(moduleClass: String)(implicit pos: Position): Tree = {
    import TreeDSL._
    Apply(envField("m", moduleClass), Nil)
  }

  def genRawJSClassConstructor(className: String)(
      implicit globalKnowledge: GlobalKnowledge, pos: Position): Tree = {

    genRawJSClassConstructor(className,
        globalKnowledge.getJSNativeLoadSpec(className))
  }

  def genRawJSClassConstructor(className: String,
      spec: Option[irt.JSNativeLoadSpec])(
      implicit pos: Position): Tree = {
    spec match {
      case None =>
        // This is a Scala.js-defined JS class, call its class value accessor
        Apply(envField("a", className), Nil)

      case Some(spec) =>
        genLoadJSFromSpec(spec)
    }
  }

  def genLoadJSFromSpec(spec: irt.JSNativeLoadSpec)(
      implicit pos: Position): Tree = {

    def pathSelection(from: Tree, path: List[String]): Tree = {
      path.foldLeft(from) {
        (prev, part) => genBracketSelect(prev, StringLiteral(part))
      }
    }

    spec match {
      case irt.JSNativeLoadSpec.Global(path) =>
        pathSelection(envField("g"), path)

      case irt.JSNativeLoadSpec.Import(module, path) =>
        val moduleValue = envModuleField(module)
        path match {
          case DefaultExportName :: rest if moduleKind == ModuleKind.CommonJSModule =>
            val defaultField = genCallHelper("moduleDefault", moduleValue)
            pathSelection(defaultField, rest)
          case _ =>
            pathSelection(moduleValue, path)
        }

      case irt.JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, globalSpec) =>
        moduleKind match {
          case ModuleKind.NoModule =>
            genLoadJSFromSpec(globalSpec)
          case ModuleKind.ESModule | ModuleKind.CommonJSModule =>
            genLoadJSFromSpec(importSpec)
        }
    }
  }

  def genArrayValue(tpe: ArrayType, elems: List[Tree])(
      implicit pos: Position): Tree = {
    genCallHelper("makeNativeArrayWrapper", genClassDataOf(tpe),
        ArrayConstr(elems))
  }

  def genClassDataOf(cls: ReferenceType)(implicit pos: Position): Tree = {
    cls match {
      case ClassType(className) =>
        envField("d", className)
      case ArrayType(base, dims) =>
        (1 to dims).foldLeft[Tree](envField("d", base)) { (prev, _) =>
          Apply(DotSelect(prev, Ident("getArrayOf")), Nil)
        }
    }
  }

  def envModuleField(module: String)(implicit pos: Position): VarRef = {
    /* This is written so that the happy path, when `module` contains only
     * valid characters, is fast.
     */

    def isValidChar(c: Char): Boolean =
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

    def containsOnlyValidChars(): Boolean = {
      // scalastyle:off return
      val len = module.length
      var i = 0
      while (i != len) {
        if (!isValidChar(module.charAt(i)))
          return false
        i += 1
      }
      true
      // scalastyle:on return
    }

    def buildValidName(): String = {
      val result = new java.lang.StringBuilder("$i_")
      val len = module.length
      var i = 0
      while (i != len) {
        val c = module.charAt(i)
        if (isValidChar(c))
          result.append(c)
        else
          result.append("$%04x".format(c.toInt))
        i += 1
      }
      result.toString()
    }

    val varName =
      if (containsOnlyValidChars()) "$i_" + module
      else buildValidName()

    VarRef(Ident(varName, Some(module)))
  }

  def envField(field: String, subField: String, origName: Option[String] = None)(
      implicit pos: Position): Tree = {
    import TreeDSL._

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        envField(field) DOT Ident(subField, origName)

      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        VarRef(Ident("$" + field + "_" + subField, origName))
    }
  }

  def envField(field: String)(implicit pos: Position): Tree = {
    import TreeDSL._

    outputMode match {
      case OutputMode.ECMAScript51Global =>
        VarRef(Ident(ScalaJSEnvironmentName)) DOT field

      case OutputMode.ECMAScript51Isolated | OutputMode.ECMAScript6 =>
        VarRef(Ident("$" + field))
    }
  }

  def genPropSelect(qual: Tree, item: PropertyName)(
      implicit pos: Position): Tree = {
    item match {
      case item: Ident         => DotSelect(qual, item)
      case item: StringLiteral => genBracketSelect(qual, item)
      case ComputedName(tree)  => genBracketSelect(qual, tree)
    }
  }

  def genBracketSelect(qual: Tree, item: Tree)(implicit pos: Position): Tree = {
    item match {
      case StringLiteral(name) if internalOptions.optimizeBracketSelects &&
          irt.isValidIdentifier(name) && name != "eval" =>
        /* We exclude "eval" because Rhino does not respect the strict mode
         * specificities of eval().
         */
        DotSelect(qual, Ident(name))
      case _ =>
        BracketSelect(qual, item)
    }
  }

  def genIdentBracketSelect(qual: Tree, item: String)(
      implicit pos: Position): Tree = {
    require(item != "eval")
    if (internalOptions.optimizeBracketSelects)
      DotSelect(qual, Ident(item))
    else
      BracketSelect(qual, StringLiteral(item))
  }
}

private object JSGen {
  private final val ScalaJSEnvironmentName = "ScalaJS"
  private final val DefaultExportName = "default"
}
