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

import scala.collection.mutable

import org.scalajs.ir.{ClassKind, OriginalName, Position, UTF8String}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

import org.scalajs.linker.backend.emitter.{NameGen => JSNameGen}

import org.scalajs.linker.backend.javascript.{Trees => js}

import org.scalajs.linker.backend.webassembly.FunctionBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import EmbeddedConstants._
import SWasmGen._
import VarGen._
import TypeTransformer._
import WasmContext._

/** Toolkit to build custom JS helpers that take Wasm inputs, while trying to
 *  optimize them directly as JavaScript trees.
 *
 *  The usage scenario for this class is as follows:
 *
 *  {{{
 *  val builder = new CustomJSHelperBuilder()
 *
 *  // Add inputs that will be evaluated on the Wasm call site, and whose
 *  // values are then available as `js.Tree`s in the JS helper:
 *  val xRef = builder.addWasmInput(watpe.Int32) {
 *    // Wasm code evaluated at call site
 *    fb += wa.LocalGet(someLocal)
 *  }
 *  ...
 *
 *  // Build the body of the helper using the above inputs
 *  val helperID = builder.build(watpe.Int32) {
 *    js.Return(js.BinaryOp(JSBinaryOp.+, xRef, js.IntLiteral(5)))
 *  }
 *
 *  // Call the helper, which consumes the declared inputs and leaves the result on the stack
 *  fb += wa.Call(helperID)
 *  }}}
 *
 *  The subclass [[CustomJSHelperBuilder.WithTreeEval]] provides even richer
 *  capabilities, by allowing to evaluate arbitrary IR `Tree`s as inputs. The
 *  builder will optimize the evaluation and transfer from Wasm to JS:
 *
 *  - Evaluate some trees entirely on the JS side instead of on the Wasm side.
 *    This is notably the case for string literals, which commonly appear as
 *    method names, and global refs/external imports, which commonly appear as
 *    method receivers.
 *  - For other trees, evaluate them on the Wasm side, but fuse their boxing
 *    operation with the Wasm-to-JS interoperability layer, by choosing the
 *    best possible Wasm type for the helper function.
 *
 *  When using the `WithTreeEval` subclass, the user must provide a concrete
 *  method to evaluate `Tree`s at call site with a given expected type. In the
 *  context of `FunctionEmitter`, that directly translates to the `genTree`
 *  method.
 *
 *  A typical scenario looks like:
 *
 *  {{{
 *  val builder = new CustomJSHelperBuilder.WithTreeEval() {
 *    protected def evalTreeAtCallSite(tree: Tree, expectedType: Type): Unit = ???
 *  }
 *
 *  // Add inputs that will be evaluated on the JS side if possible, otherwise
 *  // evaluated on the Wasm call site, and whose values are then available as
 *  // `js.Tree`s in the JS helper:
 *  val xRef = builder.addInput(someIRTree)
 *  ...
 *  }}}
 *
 *  In addition to the input management, `CustomJSHelperBuilder` provides tools
 *  for local name management, and generation of `js.ParamDef`s for inner
 *  functions.
 */
class CustomJSHelperBuilder()(implicit ctx: WasmContext, pos: Position) {
  import CustomJSHelperBuilder._

  private val usedGlobalRefs = mutable.Set.empty[String]
  private val allocatedLocalIdentResolvers = mutable.ListBuffer.empty[LocalResolver]

  private val jsParamDefs = mutable.ListBuffer.empty[js.ParamDef]
  private val wasmParamTypes = mutable.ListBuffer.empty[watpe.Type]

  def newLocalIdent(origName: String): js.DelayedIdent = {
    val resolver = new LocalResolver(origName)
    allocatedLocalIdentResolvers += resolver
    js.DelayedIdent(resolver)
  }

  def newLocalIdent(name: LocalName): js.DelayedIdent =
    newLocalIdent(ctx.jsNameGen.genName(name))

  private def addParamDef(origName: String, wasmType: watpe.Type): js.VarRef = {
    val jsParamDef = js.ParamDef(newLocalIdent(origName))
    jsParamDefs += jsParamDef
    wasmParamTypes += wasmType
    jsParamDef.ref
  }

  /** Adds an input of an arbitrary Wasm type, to be evaluated on the Wasm
   *  stack.
   *
   *  The `evalValue` must add code to the call site context to evaluate the
   *  value on the Wasm site. It is passed as by-name parameter to show
   *  intent, but is in fact called immediately.
   *
   *  @return
   *    A `js.VarRef` that can be used in the JS helper to read the input.
   */
  def addWasmInput(origName: String, wasmType: watpe.Type)(evalValue: => Unit): js.VarRef = {
    evalValue
    addParamDef(origName, wasmType)
  }

  def genGlobalRef(name: String): js.VarRef = {
    usedGlobalRefs += name
    js.VarRef(js.Ident(name))
  }

  def genJSNativeLoadSpec(jsNativeLoadSpec: JSNativeLoadSpec): js.Tree = {
    def genFollowPath(owner: js.Tree, path: List[String]): js.Tree = {
      path.foldLeft(owner) { (owner, item) =>
        js.BracketSelect.makeOptimized(owner, js.StringLiteral(item))
      }
    }

    jsNativeLoadSpec match {
      case JSNativeLoadSpec.Global(globalRef, path) =>
        genFollowPath(genGlobalRef(globalRef), path)
      case JSNativeLoadSpec.Import(module, path) =>
        genFollowPath(js.VarRef(js.Ident("imported" + JSNameGen.genModuleName(module))), path)
      case JSNativeLoadSpec.ImportWithGlobalFallback(importSpec, _) =>
        genJSNativeLoadSpec(importSpec)
    }
  }

  def genJSParamDef(param: ParamDef): js.ParamDef =
    js.ParamDef(newLocalIdent(param.name.name))

  def genJSParamDefs(params: List[ParamDef],
      restParam: Option[ParamDef]): (List[js.ParamDef], Option[js.ParamDef]) = {
    (params.map(genJSParamDef(_)), restParam.map(genJSParamDef(_)))
  }

  def build(resultType: Type)(body: js.Tree): wanme.FunctionID = {
    // Allocate names for the local ident resolvers
    val usedLocalNames = usedGlobalRefs.clone()
    for (resolver <- allocatedLocalIdentResolvers) {
      var allocatedName = resolver.origName
      var index = 0
      while (!usedLocalNames.add(allocatedName)) {
        index += 1
        allocatedName = resolver.origName + index
      }
      resolver.setResolved(allocatedName)
    }

    val helperFun = js.Function(arrow = true, jsParamDefs.toList, None, body)
    val wasmFunType = watpe.FunctionType(wasmParamTypes.toList, transformResultType(resultType))
    ctx.addCustomJSHelper(helperFun, wasmFunType)
  }
}

object CustomJSHelperBuilder {

  private final class LocalResolver(val origName: String) extends js.DelayedIdent.Resolver {
    private var resolved: Option[String] = None

    def debugString: String = origName

    def resolve(): String = {
      resolved.getOrElse {
        throw new IllegalStateException(s"Local JS ident '$origName' not resolved")
      }
    }

    def setResolved(resolved: String): Unit = {
      if (this.resolved.isDefined)
        throw new IllegalStateException(s"Local JS ident '$origName' was already resolved")
      this.resolved = Some(resolved)
    }
  }

  abstract class WithTreeEval()(implicit ctx: WasmContext, pos: Position) extends CustomJSHelperBuilder {
    /** Evaluates an arbitrary `Tree` with the given expected type and puts it
     *  on the call site's stack.
     *
     *  Concrete subclasses must implement this method to evaluate trees in
     *  their own call site context.
     *
     *  The given `tree` is guaranteed to be none of:
     *
     *  - `JSGlobalRef`
     *  - `LoadJSConstructor` for a native JS class
     *  - `LoadJSModule` for a native JS module class
     *  - `SelectJSNativeMember`
     */
    protected def evalTreeAtCallSite(tree: Tree, expectedType: Type): Unit

    def addInput(tree: TreeOrJSSpread): js.Tree = {
      tree match {
        case JSSpread(items) =>
          js.Spread(addInput(items))

        // Literals that we can directly constant-fold into the JS code
        case Undefined()           => js.Undefined()
        case Null()                => js.Null()
        case BooleanLiteral(value) => js.BooleanLiteral(value)
        case ByteLiteral(value)    => js.IntLiteral(value)
        case ShortLiteral(value)   => js.IntLiteral(value)
        case IntLiteral(value)     => js.IntLiteral(value)
        case FloatLiteral(value)   => js.DoubleLiteral(value)
        case DoubleLiteral(value)  => js.DoubleLiteral(value)
        case StringLiteral(value)  => js.StringLiteral(value)

        // Global refs and native load specs
        case JSGlobalRef(name) =>
          genGlobalRef(name)
        case LoadJSConstructor(ClassNameWithJSNativeLoadSpec(jsNativeLoadSpec)) =>
          genJSNativeLoadSpec(jsNativeLoadSpec)
        case LoadJSModule(ClassNameWithJSNativeLoadSpec(jsNativeLoadSpec)) =>
          genJSNativeLoadSpec(jsNativeLoadSpec)
        case SelectJSNativeMember(className, MethodIdent(memberName)) =>
          genJSNativeLoadSpec(ctx.getClassInfo(className).jsNativeMembers.getOrElse(memberName, {
            throw new AssertionError(
                s"Found $tree for non-existing JS native member at ${tree.pos}")
          }))

        // Other trees, which must be evaluated at the call site
        case arg: Tree =>
          def addInputAsType(expectedType: Type): js.Tree = {
            addWasmInput("x", transformParamType(expectedType)) {
              evalTreeAtCallSite(arg, expectedType)
            }
          }

          arg.tpe match {
            case BooleanType =>
              // Pass in an i32; convert to boolean in the JS code
              val input = addInputAsType(BooleanType)
              js.BinaryOp(JSBinaryOp.!==, input, js.IntLiteral(0))

            case CharType | LongType =>
              // Box on the Wasm side; pass in an anyref
              addInputAsType(AnyType)

            case argTpe =>
              // For all other types, we can rely on the Wasm-to-JS type conversions
              addInputAsType(argTpe)
          }
      }
    }
  }

  private object ClassNameWithJSNativeLoadSpec {
    def unapply(className: ClassName)(implicit ctx: WasmContext): Option[JSNativeLoadSpec] =
      ctx.getClassInfo(className).jsNativeLoadSpec
  }

}
