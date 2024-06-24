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
import org.scalajs.ir.Types._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.Position

import org.scalajs.linker.interface._
import org.scalajs.linker.interface.unstable._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.emitter.PrivateLibHolder

import org.scalajs.linker.backend.javascript.Printers.JSTreePrinter
import org.scalajs.linker.backend.javascript.{Trees => js}

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

  private val coreLib = new CoreWasmLib(coreSpec)
  private val classEmitter = new ClassEmitter(coreSpec)

  val symbolRequirements: SymbolRequirement =
    Emitter.symbolRequirements(coreSpec)

  val injectedIRFiles: Seq[IRFile] = PrivateLibHolder.files

  def emit(module: ModuleSet.Module, logger: Logger): Result = {
    val wasmModule = emitWasmModule(module)
    val loaderContent = LoaderContent.bytesContent
    val jsFileContent = buildJSFileContent(module)

    new Result(wasmModule, loaderContent, jsFileContent)
  }

  private def emitWasmModule(module: ModuleSet.Module): wamod.Module = {
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

    implicit val ctx: WasmContext =
      Preprocessor.preprocess(coreSpec, coreLib, sortedClasses, topLevelExports)

    coreLib.genPreClasses()
    genExternalModuleImports(module)
    sortedClasses.foreach(classEmitter.genClassDef(_))
    topLevelExports.foreach(classEmitter.genTopLevelExport(_))
    coreLib.genPostClasses()

    genStartFunction(sortedClasses, moduleInitializers, topLevelExports)

    /* Gen the string pool and the declarative elements at the very end, since
     * they depend on what instructions where produced by all the preceding codegen.
     */
    ctx.stringPool.genPool()
    genDeclarativeElements()

    ctx.moduleBuilder.build()
  }

  private def genExternalModuleImports(module: ModuleSet.Module)(
      implicit ctx: WasmContext): Unit = {
    // Sort for stability
    val allImportedModules = module.externalDependencies.toList.sorted

    // Gen imports of external modules on the Wasm side
    for (moduleName <- allImportedModules) {
      val id = genGlobalID.forImportedModule(moduleName)
      val origName = OriginalName("import." + moduleName)
      ctx.moduleBuilder.addImport(
        wamod.Import(
          "__scalaJSImports",
          moduleName,
          wamod.ImportDesc.Global(id, origName, isMutable = false, watpe.RefType.anyref)
        )
      )
    }
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

    // Initialize itables

    def genInitClassITable(classITableGlobalID: wanme.GlobalID,
        classInfoForResolving: WasmContext.ClassInfo, ancestors: List[ClassName]): Unit = {
      val resolvedMethodInfos = classInfoForResolving.resolvedMethodInfos

      for {
        ancestor <- ancestors
        // Use getClassInfoOption in case the reachability analysis got rid of those interfaces
        interfaceInfo <- ctx.getClassInfoOption(ancestor)
        if interfaceInfo.isInterface
      } {
        fb += wa.GlobalGet(classITableGlobalID)
        fb += wa.I32Const(interfaceInfo.itableIdx)

        for (method <- interfaceInfo.tableEntries)
          fb += ctx.refFuncWithDeclaration(resolvedMethodInfos(method).tableEntryID)
        fb += wa.StructNew(genTypeID.forITable(ancestor))
        fb += wa.ArraySet(genTypeID.itables)
      }
    }

    // For all concrete, normal classes
    for (clazz <- sortedClasses if clazz.kind.isClass && clazz.hasDirectInstances) {
      val className = clazz.className
      val classInfo = ctx.getClassInfo(className)
      if (classInfo.classImplementsAnyInterface)
        genInitClassITable(genGlobalID.forITable(className), classInfo, clazz.ancestors)
    }

    // For array classes
    genInitClassITable(genGlobalID.arrayClassITable, ctx.getClassInfo(ObjectClass),
        List(SerializableClass, CloneableClass))

    // Initialize the JS private field symbols

    for (clazz <- sortedClasses if clazz.kind.isJSClass) {
      for (fieldDef <- clazz.fields) {
        fieldDef match {
          case FieldDef(flags, name, _, _) if !flags.namespace.isStatic =>
            fb += wa.Call(genFunctionID.newSymbol)
            fb += wa.GlobalSet(genGlobalID.forJSPrivateField(name.name))
          case _ =>
            ()
        }
      }
    }

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
          fb += ctx.refFuncWithDeclaration(genFunctionID.forExport(tle.exportName))
          if (methodDef.restParam.isDefined) {
            fb += wa.I32Const(methodDef.args.size)
            fb += wa.Call(genFunctionID.makeExportedDefRest)
          } else {
            fb += wa.Call(genFunctionID.makeExportedDef)
          }
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
            args.foreach(arg => fb ++= ctx.stringPool.getConstantStringInstr(arg))
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

  private def buildJSFileContent(module: ModuleSet.Module): Array[Byte] = {
    implicit val noPos = Position.NoPosition

    // Sort for stability
    val importedModules = module.externalDependencies.toList.sorted

    val (moduleImports, importedModulesItems) = (for {
      (moduleName, idx) <- importedModules.zipWithIndex
    } yield {
      val importIdent = js.Ident(s"imported$idx")
      val moduleNameStr = js.StringLiteral(moduleName)
      val moduleImport = js.ImportNamespace(importIdent, moduleNameStr)
      val item = moduleNameStr -> js.VarRef(importIdent)
      (moduleImport, item)
    }).unzip

    val importedModulesDict = js.ObjectConstr(importedModulesItems)

    val (exportDecls, exportSettersItems) = (for {
      exportName <- module.topLevelExports.map(_.exportName)
    } yield {
      val ident = js.Ident(s"exported$exportName")
      val decl = js.Let(ident, mutable = true, None)
      val exportStat = js.Export(List(ident -> js.ExportName(exportName)))
      val xParam = js.ParamDef(js.Ident("x"))
      val setterFun = js.Function(arrow = true, List(xParam), None, {
        js.Assign(js.VarRef(ident), xParam.ref)
      })
      val setterItem = js.StringLiteral(exportName) -> setterFun
      (List(decl, exportStat), setterItem)
    }).unzip

    val exportSettersDict = js.ObjectConstr(exportSettersItems)

    val loadFunIdent = js.Ident("__load")
    val loaderImport = js.Import(
      List(js.ExportName("load") -> loadFunIdent),
      js.StringLiteral(config.loaderModuleName)
    )

    val loadCall = js.Apply(
      js.VarRef(loadFunIdent),
      List(
        js.StringLiteral(config.internalWasmFileURIPattern(module.id)),
        importedModulesDict,
        exportSettersDict
      )
    )

    val fullTree = (
      moduleImports :::
      loaderImport ::
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

      cond(isAnyFatal(asInstanceOfs, arrayIndexOutOfBounds, arrayStores,
          negativeArraySizes, nullPointers, stringIndexOutOfBounds)) {
        instantiateClass(UndefinedBehaviorErrorClass,
            ThrowableArgConsructorName)
      },

      // TODO Ideally we should not require these, but rather adapt to their absence
      instantiateClass(ClassClass, AnyArgConstructorName),
      instantiateClass(JSExceptionClass, AnyArgConstructorName),

      // See genIdentityHashCode in HelperFunctions
      callMethodStatically(BoxedDoubleClass, hashCodeMethodName),
      callMethodStatically(BoxedStringClass, hashCodeMethodName)
    )
  }

}
