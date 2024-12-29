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

package org.scalajs.linker.frontend

import scala.collection.mutable
import scala.concurrent._

import org.scalajs.logging._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.checker._
import org.scalajs.linker.analyzer._

import org.scalajs.ir
import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.{ClassDef, MethodDef}
import org.scalajs.ir.Version

import Analysis._

/** Links the information from [[interface.IRFile IRFile]]s into
 *  [[standard.LinkedClass LinkedClass]]es. Does a dead code elimination pass.
 */
final class BaseLinker(config: CommonPhaseConfig, checkIR: Boolean) {
  import BaseLinker._

  private val irLoader = new FileIRLoader
  private val analyzer =
    new Analyzer(config, initial = true, checkIR = checkIR, failOnError = true, irLoader)
  private val desugarTransformer = new DesugarTransformer(config.coreSpec)
  private val desugaredClassCaches =
    new java.util.concurrent.ConcurrentHashMap[ClassName, DesugaredClassCache]
  private val methodSynthesizer = new MethodSynthesizer(irLoader)

  def link(irInput: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer], logger: Logger,
      symbolRequirements: SymbolRequirement)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {

    val result = for {
      _ <- irLoader.update(irInput)
      analysis <- logger.timeFuture("Linker: Compute reachability") {
        analyzer.computeReachability(moduleInitializers, symbolRequirements, logger)
      }
      linkResult <- logger.timeFuture("Linker: Assemble LinkedClasses") {
        assemble(moduleInitializers, analysis)
      }
    } yield {
      if (checkIR) {
        logger.time("Linker: Check IR") {
          val errorCount = IRChecker.check(linkResult, logger)
          if (errorCount != 0) {
            throw new LinkingException(
                s"There were $errorCount IR checking errors.")
          }
        }
      }

      linkResult
    }

    result.andThen { case _ =>
      irLoader.cleanAfterRun()
    }
  }

  private def assemble(moduleInitializers: Seq[ModuleInitializer],
      analysis: Analysis)(implicit ec: ExecutionContext): Future[LinkingUnit] = {

    /* Remove stale entries from the desugared class cache.
     * Note that we do not remove individual stale *methods* inside a live
     * class. This has the potential for memory leaks if methods requiring
     * desugaring keep being added and removed across runs.
     * It does not seem worth guarding against, though.
     */
    desugaredClassCaches.keySet().removeIf { (className: ClassName) =>
      !analysis.classInfos.contains(className)
    }

    def assembleClass(info: ClassInfo) = {
      val version = irLoader.irFileVersion(info.className)
      val syntheticMethods = methodSynthesizer.synthesizeMembers(info, analysis)

      for {
        classDef <- irLoader.loadClassDef(info.className)
        syntheticMethods <- syntheticMethods
      } yield {
        val desugaredClassCache = desugaredClassCaches.computeIfAbsent(info.className, {
          (className: ClassName) =>
            new DesugaredClassCache(desugarTransformer)
        })
        BaseLinker.linkClassDef(classDef, version, syntheticMethods, analysis,
            Some(desugaredClassCache))
      }
    }

    for {
      assembled <- Future.traverse(analysis.classInfos.values)(assembleClass)
    } yield {
      val (linkedClassDefs, linkedTopLevelExports) = assembled.unzip

      val globalInfo = new LinkedGlobalInfo(
        analysis.isClassSuperClassUsed
      )

      new LinkingUnit(config.coreSpec, linkedClassDefs.toList,
          linkedTopLevelExports.flatten.toList,
          moduleInitializers.toList,
          globalInfo)
    }
  }
}

private[frontend] object BaseLinker {

  private final class DesugarTransformer(coreSpec: CoreSpec)
      extends ir.Transformers.ClassTransformer {

    import ir.Trees._

    override def transform(tree: Tree): Tree = {
      tree match {
        case prop: LinkTimeProperty =>
          coreSpec.linkTimeProperties.transformLinkTimeProperty(prop)

        case _ =>
          super.transform(tree)
      }
    }

    /* Transfer Version from old members to transformed members.
     * We can do this because the transformation only depends on the
     * `coreSpec`, which is immutable.
     */

    override def transformMethodDef(methodDef: MethodDef): MethodDef = {
      val newMethodDef = super.transformMethodDef(methodDef)
      newMethodDef.copy()(newMethodDef.optimizerHints, methodDef.version)(newMethodDef.pos)
    }

    override def transformJSConstructorDef(jsConstructor: JSConstructorDef): JSConstructorDef = {
      val newJSConstructor = super.transformJSConstructorDef(jsConstructor)
      newJSConstructor.copy()(newJSConstructor.optimizerHints, jsConstructor.version)(
          newJSConstructor.pos)
    }

    override def transformJSMethodDef(jsMethodDef: JSMethodDef): JSMethodDef = {
      val newJSMethodDef = super.transformJSMethodDef(jsMethodDef)
      newJSMethodDef.copy()(newJSMethodDef.optimizerHints, jsMethodDef.version)(
          newJSMethodDef.pos)
    }

    override def transformJSPropertyDef(jsPropertyDef: JSPropertyDef): JSPropertyDef = {
      val newJSPropertyDef = super.transformJSPropertyDef(jsPropertyDef)
      newJSPropertyDef.copy()(jsPropertyDef.version)(newJSPropertyDef.pos)
    }
  }

  private final class DesugaredClassCache(desugarTransformer: DesugarTransformer) {
    import ir.Trees._

    private val methodCache = mutable.Map.empty[(MemberNamespace, MethodName), MethodDef]

    def desugarMethod(method: MethodDef): MethodDef = {
      if (method.version == Version.Unversioned) {
        desugarTransformer.transformMethodDef(method)
      } else {
        val key = (method.flags.namespace, method.methodName)
        methodCache.get(key) match {
          case Some(desugared) if desugared.version.sameVersion(method.version) =>
            desugared
          case _ =>
            val desugared = desugarTransformer.transformMethodDef(method)
            methodCache(key) = desugared
            desugared
        }
      }
    }

    def desugarJSConstructor(jsConstructor: Option[JSConstructorDef]): Option[JSConstructorDef] = {
      // We do not actually cache the desugaring of JS members
      jsConstructor.map(desugarTransformer.transformJSConstructorDef(_))
    }

    def desugarJSMethodProps(jsMethodProps: List[JSMethodPropDef]): List[JSMethodPropDef] = {
      // We do not actually cache the desugaring of JS members
      jsMethodProps.map(desugarTransformer.transformJSMethodPropDef(_))
    }

    def desugarTopLevelExport(topLevelExport: TopLevelExportDef): TopLevelExportDef = {
      // We do not actually cache top-level exports
      desugarTransformer.transformTopLevelExportDef(topLevelExport)
    }
  }

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private[frontend] def refineClassDef(classDef: ClassDef, version: Version,
      analysis: Analysis): (LinkedClass, List[LinkedTopLevelExport]) = {
    linkClassDef(classDef, version, syntheticMethodDefs = Nil, analysis,
        desugaredClassCache = None)
  }

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private[frontend] def linkClassDef(classDef: ClassDef, version: Version,
      syntheticMethodDefs: List[MethodDef], analysis: Analysis,
      desugaredClassCache: Option[DesugaredClassCache]): (LinkedClass, List[LinkedTopLevelExport]) = {
    import ir.Trees._

    def requireDesugaredClassCache(): DesugaredClassCache = {
      desugaredClassCache.getOrElse {
        throw new AssertionError(
            s"Unexpected desugaring needed in refiner in class ${classDef.className.nameString}")
      }
    }

    val classInfo = analysis.classInfos(classDef.className)

    val fields = classDef.fields.filter {
      case field: FieldDef =>
        if (field.flags.namespace.isStatic)
          classInfo.staticFieldsRead(field.name.name) || classInfo.staticFieldsWritten(field.name.name)
        else if (classInfo.kind.isJSClass || classInfo.isAnySubclassInstantiated)
          classInfo.fieldsRead(field.name.name) || classInfo.fieldsWritten(field.name.name)
        else
          false

      case field: JSFieldDef =>
        classInfo.isAnySubclassInstantiated
    }

    val methods: List[MethodDef] = classDef.methods.iterator
      .map(m => m -> classInfo.methodInfos(m.flags.namespace)(m.methodName))
      .filter(_._2.isReachable)
      .map { case (m, info) =>
        assert(m.body.isDefined,
            s"The abstract method ${classDef.name.name}.${m.methodName} is reachable.")
        if (!info.needsDesugaring)
          m
        else
          requireDesugaredClassCache().desugarMethod(m)
      }
      .toList

    val (jsConstructor, jsMethodProps) = if (classInfo.isAnySubclassInstantiated) {
      val anyJSMemberNeedsDesugaring = classInfo.anyJSMemberNeedsDesugaring

      if (!anyJSMemberNeedsDesugaring) {
        (classDef.jsConstructor, classDef.jsMethodProps)
      } else {
        val cache = requireDesugaredClassCache()
        (cache.desugarJSConstructor(classDef.jsConstructor),
            cache.desugarJSMethodProps(classDef.jsMethodProps))
      }
    } else {
      (None, Nil)
    }

    val jsNativeMembers = classDef.jsNativeMembers
      .filter(m => classInfo.jsNativeMembersUsed.contains(m.name.name))

    val allMethods = methods ++ syntheticMethodDefs

    val ancestors = classInfo.ancestors.map(_.className)

    val linkedClass = new LinkedClass(
        classDef.name,
        classDef.kind,
        classDef.jsClassCaptures,
        classDef.superClass,
        classDef.interfaces,
        classDef.jsSuperClass,
        classDef.jsNativeLoadSpec,
        fields,
        allMethods,
        jsConstructor,
        jsMethodProps,
        jsNativeMembers,
        classDef.optimizerHints,
        classDef.pos,
        ancestors.toList,
        hasInstances = classInfo.isAnySubclassInstantiated,
        hasDirectInstances = classInfo.isInstantiated,
        hasInstanceTests = classInfo.areInstanceTestsUsed,
        hasRuntimeTypeInfo = classInfo.isDataAccessed,
        fieldsRead = classInfo.fieldsRead.toSet,
        staticFieldsRead = classInfo.staticFieldsRead.toSet,
        staticDependencies = classInfo.staticDependencies.toSet,
        externalDependencies = classInfo.externalDependencies.toSet,
        dynamicDependencies = classInfo.dynamicDependencies.toSet,
        version)

    val linkedTopLevelExports = for {
      topLevelExport <- classDef.topLevelExportDefs
    } yield {
      val infos = analysis.topLevelExportInfos(
        (ModuleID(topLevelExport.moduleID), topLevelExport.topLevelExportName))
      val desugared =
        if (!infos.needsDesugaring) topLevelExport
        else requireDesugaredClassCache().desugarTopLevelExport(topLevelExport)
      new LinkedTopLevelExport(classDef.className, topLevelExport,
          infos.staticDependencies.toSet, infos.externalDependencies.toSet)
    }

    (linkedClass, linkedTopLevelExports)
  }
}
