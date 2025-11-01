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

import scala.concurrent.{ExecutionContext, Future}

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.ClosureFlags
import org.scalajs.ir.Types._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.Position
import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.UTF8String
import org.scalajs.ir.WellKnownNames._

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.emitter.{NameGen => JSNameGen, PrivateLibHolder}

import org.scalajs.linker.backend.javascript.Printers.JSTreePrinter
import org.scalajs.linker.backend.javascript.{Trees => js}

import org.scalajs.linker.backend.wasmemitter.EmbeddedConstants._
import org.scalajs.linker.backend.webassembly.FunctionBuilder
import org.scalajs.linker.backend.webassembly.{Instructions => wa}
import org.scalajs.linker.backend.webassembly.{Modules => wamod}
import org.scalajs.linker.backend.webassembly.{Identitities => wanme}
import org.scalajs.linker.backend.webassembly.{Types => watpe}

import org.scalajs.logging.Logger

import SpecialNames._
import VarGen._
import org.scalajs.linker.backend.javascript.ByteArrayWriter

final class Emitter(config: Emitter.Config) {
  import Emitter._

  private val coreSpec = config.coreSpec

  private val loaderContent = LoaderContent.makeBytesContent(coreSpec)

  private val classEmitter = new ClassEmitter(coreSpec)

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(coreSpec)

  val injectedIRFiles: Seq[IRFile] = PrivateLibHolder.files

  def emit(module: ModuleSet.Module, globalInfo: LinkedGlobalInfo, logger: Logger): Result = {
    val (wasmModule, jsFileContentInfo) = emitWasmModule(module, globalInfo)
    val jsFileContent = buildJSFileContent(module, jsFileContentInfo)

    new Result(wasmModule, loaderContent, jsFileContent)
  }

  private def emitWasmModule(module: ModuleSet.Module,
      globalInfo: LinkedGlobalInfo): (wamod.Module, JSFileContentInfo) = {
    // Inject the derived linked classes
    val allClasses =
      DerivedClasses.deriveClasses(module.classDefs) ::: module.classDefs

    /* Sort by ancestor count so that superclasses always appear before
     * subclasses, then tie-break by name for stability.
     */
    val sortedClasses = allClasses.sortWith { (a, b) =>
      val cmp = Integer.compare(a.ancestors.size, b.ancestors.size)
      if (cmp != 0) cmp < 0
      else a.className.compareTo(b.className) < 0
    }

    val topLevelExports = module.topLevelExports
    val moduleInitializers = module.initializers.toList

    val coreLib = new CoreWasmLib(coreSpec, globalInfo)

    implicit val ctx: WasmContext =
      Preprocessor.preprocess(coreSpec, coreLib, sortedClasses, topLevelExports)

    coreLib.genPreClasses()
    sortedClasses.foreach(classEmitter.genClassDef(_))
    topLevelExports.foreach(classEmitter.genTopLevelExport(_))
    classEmitter.genArrayClasses()
    coreLib.genPostClasses()

    genStartFunction(sortedClasses, moduleInitializers, topLevelExports)

    val privateJSFields = genPrivateJSFields(sortedClasses)

    /* Gen the string pool and the declarative elements at the very end, since
     * they depend on what instructions where produced by all the preceding codegen.
     */
    val wtf16Strings = ctx.stringPool.genPool()
    genDeclarativeElements()

    // Likewise, gen the constant array pool at the end
    for (data <- ctx.constantArrayPool.genPool())
      ctx.moduleBuilder.addData(data)

    val wasmModule = ctx.moduleBuilder.build()

    val jsFileContentInfo = new JSFileContentInfo(
      privateJSFields = privateJSFields,
      customJSHelpers = ctx.getAllCustomJSHelpers(),
      wtf16Strings = wtf16Strings
    )

    (wasmModule, jsFileContentInfo)
  }

  private def genStartFunction(
      sortedClasses: List[LinkedClass],
      moduleInitializers: List[ModuleInitializer.Initializer],
      topLevelExportDefs: List[LinkedTopLevelExport]
  )(implicit ctx: WasmContext): Unit = {
    import org.scalajs.ir.Trees._

    implicit val pos = Position.NoPosition

    val fb =
      new FunctionBuilder(ctx.moduleBuilder, genFunctionID.start, OriginalName("start"), pos)

    // Configure the JS prototypes

    if (ctx.useCustomDescriptors)
      genConfigureJSPrototypes(fb, sortedClasses)

    // Emit the static initializers

    for (clazz <- sortedClasses if clazz.hasStaticInitializer) {
      val funcID = genFunctionID.forMethod(
        MemberNamespace.StaticConstructor,
        clazz.className,
        StaticInitializerName
      )
      fb += wa.Call(funcID)
    }

    // Initialize the top-level exports that require it

    for (tle <- topLevelExportDefs) {
      // Load the (initial) exported value on the stack
      tle.tree match {
        case TopLevelJSClassExportDef(_, exportName) =>
          fb += wa.Call(genFunctionID.loadJSClass(tle.owningClass))
        case TopLevelModuleExportDef(_, exportName) =>
          fb += wa.Call(genFunctionID.loadModule(tle.owningClass))
        case TopLevelMethodExportDef(_, methodDef) =>
          genTopLevelExportedFun(fb, tle.exportName, methodDef)
        case TopLevelFieldExportDef(_, _, fieldIdent) =>
          /* Usually redundant, but necessary if the static field is never
           * explicitly set and keeps its default (zero) value instead. In that
           * case this initial call is required to publish that zero value (as
           * opposed to the default `undefined` value of the JS `let`).
           */
          fb += wa.GlobalGet(genGlobalID.forStaticField(fieldIdent.name))
      }

      // Call the export setter
      fb += wa.Call(genFunctionID.forTopLevelExportSetter(tle.exportName))
    }

    // Emit the module initializers

    moduleInitializers.foreach { init =>
      def genCallStatic(className: ClassName, methodName: MethodName): Unit = {
        val funcID = genFunctionID.forMethod(MemberNamespace.PublicStatic, className, methodName)
        fb += wa.Call(funcID)
      }

      ModuleInitializerImpl.fromInitializer(init) match {
        case ModuleInitializerImpl.MainMethodWithArgs(className, encodedMainMethodName, args) =>
          val stringArrayTypeRef = ArrayTypeRef(ClassRef(BoxedStringClass), 1)
          SWasmGen.genArrayValue(fb, stringArrayTypeRef, args.size) {
            for (arg <- args) {
              fb += ctx.stringPool.getConstantStringInstr(arg)
              fb += wa.AnyConvertExtern
            }
          }
          genCallStatic(className, encodedMainMethodName)

        case ModuleInitializerImpl.VoidMainMethod(className, encodedMainMethodName) =>
          genCallStatic(className, encodedMainMethodName)
      }
    }

    // Finish the start function

    fb.buildAndAddToModule()
    ctx.moduleBuilder.setStart(genFunctionID.start)
  }

  private def genConfigureJSPrototypes(fb: FunctionBuilder, sortedClasses: List[LinkedClass])(
      implicit ctx: WasmContext): Unit = {

    import org.scalajs.ir.Trees._

    /* TODO Most of this should be done with the configureAll builtin.
     * Eventually, only methods with varargs should be handled via JS helpers.
     */

    for (clazz <- sortedClasses) {
      val className = clazz.className
      val classInfo = ctx.getClassInfo(className)

      if (classInfo.jsPrototypeHolder.contains(className)) {
        implicit val pos = clazz.pos

        val helperBuilder = new CustomJSHelperBuilder()
        val statsBuilder = List.newBuilder[js.Tree]

        val objectRef = helperBuilder.genGlobalRef("Object")
        val errorRef = helperBuilder.genGlobalRef("Error")
        val webAssemblyRef = helperBuilder.genGlobalRef("WebAssembly")

        def addHackToRecoverProtoFromDescriptorOptions(protoRef: js.VarRef): Unit = {
          // TODO Get rid of this when we get rid of the DescriptorOptions legacy support
          statsBuilder += js.If(js.DotSelect(webAssemblyRef, js.Ident("DescriptorOptions")), {
            js.Assign(protoRef, js.DotSelect(protoRef, js.Ident("scalaJSUnderlyingProto")))
          })
        }

        val protoRef = helperBuilder.addWasmInput("proto", watpe.RefType.extern) {
          fb += wa.GlobalGet(genGlobalID.forJSPrototype(className))
        }
        addHackToRecoverProtoFromDescriptorOptions(protoRef)

        // Set up the super prototype
        for {
          superClass <- clazz.superClass
          superProtoHolder <- ctx.getClassInfo(superClass.name).jsPrototypeHolder
        } {
          val superProtoRef = if (className == ThrowableClass) {
            js.DotSelect(errorRef, js.Ident("prototype"))
          } else {
            val superProtoRef = helperBuilder.addWasmInput("superProto", watpe.RefType.extern) {
              fb += wa.GlobalGet(genGlobalID.forJSPrototype(superProtoHolder))
            }
            addHackToRecoverProtoFromDescriptorOptions(superProtoRef)
            superProtoRef
          }
          statsBuilder += js.Apply(
            js.DotSelect(objectRef, js.Ident("setPrototypeOf")),
            List(protoRef, superProtoRef)
          )
        }

        // Set up the members
        for (exportedMember <- clazz.exportedMembers) {
          exportedMember match {
            case JSMethodDef(_, nameTree, params, restParam, _) =>
              val StringLiteral(name) = nameTree: @unchecked
              val fRef = helperBuilder.addWasmInput("f", watpe.RefType.func) {
                fb += ctx.refFuncWithDeclaration(genFunctionID.forExportedMethod(className, name))
              }
              val (argsParamDefs, restParamDef) = helperBuilder.genJSParamDefs(params, restParam)
              val jsFunction = js.Function(ClosureFlags.function, argsParamDefs, restParamDef, {
                js.Return(js.Apply(fRef,
                    js.This() :: argsParamDefs.map(_.ref) ::: restParamDef.map(_.ref).toList))
              })
              statsBuilder += js.Assign(
                js.BracketSelect(protoRef, js.StringLiteral(name)),
                jsFunction
              )

            case JSPropertyDef(_, nameTree, getterBodyOpt, setterArgAndBodyOpt) =>
              val StringLiteral(name) = nameTree: @unchecked

              val optGetterFun = getterBodyOpt.map { _ =>
                val fRef = helperBuilder.addWasmInput("f", watpe.RefType.func) {
                  fb += ctx.refFuncWithDeclaration(
                      genFunctionID.forExportedPropGetter(className, name))
                }
                js.Function(ClosureFlags.function, Nil, None, {
                  js.Return(js.Apply(fRef, List(js.This())))
                })
              }

              val optSetterFun = setterArgAndBodyOpt.map { case (param, _) =>
                val fRef = helperBuilder.addWasmInput("f", watpe.RefType.func) {
                  fb += ctx.refFuncWithDeclaration(
                      genFunctionID.forExportedPropSetter(className, name))
                }
                val argParamDef = helperBuilder.genJSParamDef(param)
                js.Function(ClosureFlags.function, List(argParamDef), None, {
                  js.Return(js.Apply(fRef, List(js.This(), argParamDef.ref)))
                })
              }

              val descriptor = js.ObjectConstr(
                optGetterFun.map(js.Ident("get") -> _).toList :::
                optSetterFun.map(js.Ident("set") -> _).toList :::
                List(js.Ident("configurable") -> js.BooleanLiteral(true))
              )

              statsBuilder += js.Apply(
                js.DotSelect(objectRef, js.Ident("defineProperty")),
                List(protoRef, js.StringLiteral(name), descriptor)
              )
          }
        }

        val helperID = helperBuilder.build(VoidType)(js.Block(statsBuilder.result()))
        fb += wa.Call(helperID)
      }
    }
  }

  private def genTopLevelExportedFun(fb: FunctionBuilder, exportName: String,
      methodDef: org.scalajs.ir.Trees.JSMethodDef)(
      implicit ctx: WasmContext): Unit = {

    import org.scalajs.ir.Trees._

    val JSMethodDef(_, _, params, restParam, _) = methodDef

    implicit val pos = methodDef.pos

    val builder = new CustomJSHelperBuilder()

    val fRef = builder.addWasmInput("f", watpe.RefType.func) {
      fb += ctx.refFuncWithDeclaration(genFunctionID.forExport(exportName))
    }

    val helperID = builder.build(AnyNotNullType) {
      js.Return {
        val (argsParamDefs, restParamDef) = builder.genJSParamDefs(params, restParam)
        // Exported defs must be `function`s although they do not use their `this`
        js.Function(ClosureFlags.function, argsParamDefs, restParamDef, {
          js.Return(js.Apply(
            fRef,
            argsParamDefs.map(_.ref) ::: restParamDef.map(_.ref).toList
          ))
        })
      }
    }

    fb += wa.Call(helperID)
  }

  private def genPrivateJSFields(sortedClasses: List[LinkedClass])(
      implicit ctx: WasmContext): List[(String, FieldName)] = {
    import org.scalajs.ir.Trees._

    val privateJSFieldGetterTypeID = ctx.moduleBuilder.functionTypeToTypeID(
        watpe.FunctionType(List(watpe.RefType.anyref), List(watpe.RefType.anyref)))
    val privateJSFieldSetterTypeID = ctx.moduleBuilder.functionTypeToTypeID(
        watpe.FunctionType(List(watpe.RefType.anyref, watpe.RefType.anyref), Nil))

    val setSuffix = UTF8String("_set")

    for {
      clazz <- sortedClasses
      if clazz.kind.isJSClass
      FieldDef(flags, FieldIdent(fieldName), origName, _) <- clazz.fields
      if !flags.namespace.isStatic
    } yield {
      val varName = ctx.privateJSFields(fieldName)

      val origName1 = origName.orElse(fieldName)
      ctx.moduleBuilder.addImport(wamod.Import(
        PrivateJSFieldGetters,
        varName,
        wamod.ImportDesc.Func(
          genFunctionID.forPrivateJSFieldGetter(fieldName),
          origName1,
          privateJSFieldGetterTypeID
        )
      ))
      ctx.moduleBuilder.addImport(wamod.Import(
        PrivateJSFieldSetters,
        varName,
        wamod.ImportDesc.Func(
          genFunctionID.forPrivateJSFieldSetter(fieldName),
          OriginalName(origName1.get ++ setSuffix),
          privateJSFieldSetterTypeID
        )
      ))

      varName -> fieldName
    }
  }

  private def genDeclarativeElements()(implicit ctx: WasmContext): Unit = {
    // Aggregated Elements

    val funcDeclarations = ctx.getAllFuncDeclarations()

    if (funcDeclarations.nonEmpty) {
      /* Functions that are referred to with `ref.func` in the Code section
       * must be declared ahead of time in one of the earlier sections
       * (otherwise the module does not validate). It can be the Global section
       * if they are meaningful there (which is why `ref.func` in the vtables
       * work out of the box). In the absence of any other specific place, an
       * Element section with the declarative mode is the recommended way to
       * introduce these declarations.
       */
      val exprs = funcDeclarations.map { funcID =>
        wa.Expr(List(wa.RefFunc(funcID)))
      }
      ctx.moduleBuilder.addElement(
        wamod.Element(watpe.RefType.funcref, exprs, wamod.Element.Mode.Declarative)
      )
    }
  }

  private def buildJSFileContent(module: ModuleSet.Module,
      info: JSFileContentInfo): Array[Byte] = {

    implicit val noPos = Position.NoPosition

    // Sort for stability
    val importedModules = module.externalDependencies.toList.sorted

    // External imports

    val moduleImports = for (moduleName <- importedModules) yield {
      val importIdent = js.Ident("imported" + JSNameGen.genModuleName(moduleName))
      val moduleNameStr = js.StringLiteral(moduleName)
      js.ImportNamespace(importIdent, moduleNameStr)
    }

    // Exports

    val (exportDecls, exportSettersItems) = (for {
      exportName <- module.topLevelExports.map(_.exportName)
    } yield {
      val ident = js.Ident(s"exported$exportName")
      val decl = js.Let(ident, mutable = true, None)
      val exportStat = js.Export(List(ident -> js.ExportName(exportName)))
      val xParam = js.ParamDef(js.Ident("x"))
      val setterFun = js.Function(ClosureFlags.arrow, List(xParam), None, {
        js.Assign(js.VarRef(ident), xParam.ref)
      })
      val setterItem = js.StringLiteral(exportName) -> setterFun
      (List(decl, exportStat), setterItem)
    }).unzip

    val exportSettersDict = js.ObjectConstr(exportSettersItems)

    // JS private field symbols and the accompanying getters and setters

    val (privateJSFieldDecls, privateJSFieldGetterItems, privateJSFieldSetterItems) = {
      (for ((varName, fieldName) <- info.privateJSFields) yield {
        val symbolValue = {
          val args =
            if (coreSpec.semantics.productionMode) Nil
            else js.StringLiteral(fieldName.nameString) :: Nil
          js.Apply(js.VarRef(js.Ident("Symbol")), args)
        }

        val varIdent = js.Ident(varName)
        val importName = js.StringLiteral(varName)
        val qualParamDef = js.ParamDef(js.Ident("qual"))
        val valueParamDef = js.ParamDef(js.Ident("value"))

        val varDef = js.VarDef(varIdent, Some(symbolValue))
        val getterItem = importName -> js.Function(ClosureFlags.arrow, List(qualParamDef), None, {
          js.Return(js.BracketSelect(qualParamDef.ref, js.VarRef(varIdent)))
        })
        val setterItem = {
          importName -> js.Function(ClosureFlags.arrow, List(qualParamDef, valueParamDef), None, {
            js.Assign(js.BracketSelect(qualParamDef.ref, js.VarRef(varIdent)), valueParamDef.ref)
          })
        }

        (varDef, getterItem, setterItem)
      }).unzip3
    }
    val privateJSFieldGettersDict = js.ObjectConstr(privateJSFieldGetterItems)
    val privateJSFieldSettersDict = js.ObjectConstr(privateJSFieldSetterItems)

    // Custom JS helpers

    val customJSHelpersItems = for ((importName, jsFunction) <- info.customJSHelpers) yield {
      js.StringLiteral(importName) -> jsFunction
    }
    val customJSHelpersDict = js.ObjectConstr(customJSHelpersItems)

    // WTF-16 string constants

    val wtf16StringsItems = for ((importName, str) <- info.wtf16Strings) yield {
      js.StringLiteral(importName) -> js.StringLiteral(str)
    }
    val wtf16StringsDict = js.ObjectConstr(wtf16StringsItems)

    // Overall structure of the result

    val loadFunIdent = js.Ident("__load")
    val loaderImport = js.Import(
      List(js.ExportName("load") -> loadFunIdent),
      js.StringLiteral(config.loaderModuleName)
    )

    val loadCall = js.Apply(
      js.VarRef(loadFunIdent),
      List(
        js.StringLiteral(config.internalWasmFileURIPattern(module.id)),
        exportSettersDict,
        privateJSFieldGettersDict,
        privateJSFieldSettersDict,
        customJSHelpersDict,
        wtf16StringsDict
      )
    )

    val fullTree = (
      moduleImports :::
        loaderImport ::
        privateJSFieldDecls :::
        exportDecls.flatten :::
        js.Await(loadCall) ::
        Nil
    )

    val writer = new ByteArrayWriter
    val printer = new JSTreePrinter(writer)
    fullTree.foreach(printer.printStat(_))
    writer.toByteArray()
  }
}

object Emitter {

  /** Configuration for the Emitter. */
  final class Config private (
      val coreSpec: CoreSpec,
      val loaderModuleName: String,
      val internalWasmFileURIPattern: ModuleID => String
  ) {
    private def this(coreSpec: CoreSpec, loaderModuleName: String) = {
      this(
        coreSpec,
        loaderModuleName,
        internalWasmFileURIPattern = { moduleID => s"./${moduleID.id}.wasm" }
      )
    }

    def withInternalWasmFileURIPattern(
        internalWasmFileURIPattern: ModuleID => String): Config = {
      copy(internalWasmFileURIPattern = internalWasmFileURIPattern)
    }

    private def copy(
        coreSpec: CoreSpec = coreSpec,
        loaderModuleName: String = loaderModuleName,
        internalWasmFileURIPattern: ModuleID => String = internalWasmFileURIPattern
    ): Config = {
      new Config(
        coreSpec,
        loaderModuleName,
        internalWasmFileURIPattern
      )
    }
  }

  object Config {
    def apply(coreSpec: CoreSpec, loaderModuleName: String): Config =
      new Config(coreSpec, loaderModuleName)
  }

  private final class JSFileContentInfo(
      /** Private JS fields for which we need symbols: pairs of `(importName/identName, fieldName)`. */
      val privateJSFields: List[(String, FieldName)],
      /** Custom JS helpers to generate: pairs of `(importName, jsFunction)`. */
      val customJSHelpers: List[(String, js.Function)],
      /** WTF-16 string constants: pairs of `(importName, stringValue)`. */
      val wtf16Strings: List[(String, String)]
  )

  final class Result(
      val wasmModule: wamod.Module,
      val loaderContent: Array[Byte],
      val jsFileContent: Array[Byte]
  )

  /** Builds the symbol requirements of our back-end.
   *
   *  The symbol requirements tell the LinkerFrontend that we need these symbols to always be
   *  reachable, even if no "user-land" IR requires them. They are roots for the reachability
   *  analysis, together with module initializers and top-level exports. If we don't do this, the
   *  linker frontend will dead-code eliminate our box classes.
   */
  private def symbolRequirements(coreSpec: CoreSpec): SymbolRequirement = {
    import coreSpec.semantics._
    import CheckedBehavior._
    import SpecialNames._

    val factory = SymbolRequirement.factory("emitter")
    import factory._

    def cond(p: Boolean)(v: => SymbolRequirement): SymbolRequirement =
      if (p) v else none()

    def isAnyFatal(behaviors: CheckedBehavior*): Boolean =
      behaviors.contains(Fatal)

    multiple(
      cond(asInstanceOfs != Unchecked) {
        instantiateClass(ClassCastExceptionClass, StringArgConstructorName)
      },

      cond(arrayIndexOutOfBounds != Unchecked) {
        instantiateClass(ArrayIndexOutOfBoundsExceptionClass,
            StringArgConstructorName)
      },

      cond(arrayStores != Unchecked) {
        instantiateClass(ArrayStoreExceptionClass,
            StringArgConstructorName)
      },

      cond(negativeArraySizes != Unchecked) {
        instantiateClass(NegativeArraySizeExceptionClass,
            StringArgConstructorName)
      },

      cond(nullPointers != Unchecked) {
        instantiateClass(NullPointerExceptionClass, NoArgConstructorName)
      },

      cond(stringIndexOutOfBounds != Unchecked) {
        instantiateClass(StringIndexOutOfBoundsExceptionClass,
            IntArgConstructorName)
      },

      cond(isAnyFatal(asInstanceOfs, arrayIndexOutOfBounds, arrayStores,
          negativeArraySizes, nullPointers, stringIndexOutOfBounds)) {
        instantiateClass(UndefinedBehaviorErrorClass,
            ThrowableArgConsructorName)
      },

      cond(moduleInit == Fatal) {
        instantiateClass(UndefinedBehaviorErrorClass,
            StringArgConstructorName)
      },

      // TODO Ideally we should not require these, but rather adapt to their absence
      instantiateClass(ClassClass, NoArgConstructorName),
      instantiateClass(JSExceptionClass, AnyArgConstructorName),
      instantiateClass(IllegalArgumentExceptionClass, NoArgConstructorName),

      // See genIdentityHashCode in HelperFunctions
      callMethodStatically(BoxedDoubleClass, hashCodeMethodName),
      callMethodStatically(BoxedStringClass, hashCodeMethodName),

      // Implementation of Float_% and Double_%
      callStaticMethod(WasmRuntimeClass, fmodfMethodName),
      callStaticMethod(WasmRuntimeClass, fmoddMethodName)
    )
  }

}
