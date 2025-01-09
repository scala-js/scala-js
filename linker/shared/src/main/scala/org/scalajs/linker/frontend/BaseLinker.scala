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
import org.scalajs.ir.Trees._
import org.scalajs.ir.Version

import Analysis._

/** Links the information from [[interface.IRFile IRFile]]s into
 *  [[standard.LinkedClass LinkedClass]]es. Does a dead code elimination pass.
 */
final class BaseLinker(config: CommonPhaseConfig, checkIRFor: Option[CheckingPhase]) {
  import BaseLinker._

  private val irLoader = new FileIRLoader
  private val analyzer =
    new Analyzer(config, initial = true, checkIRFor = checkIRFor, failOnError = true, irLoader)
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
      for (nextPhase <- checkIRFor) {
        logger.time("Linker: Check IR") {
          val errorCount = IRChecker.check(linkResult, logger, nextPhase)
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
    def assembleClass(info: ClassInfo) = {
      val version = irLoader.irFileVersion(info.className)
      val syntheticMethods = methodSynthesizer.synthesizeMembers(info, analysis)

      for {
        classDef <- irLoader.loadClassDef(info.className)
        syntheticMethods <- syntheticMethods
      } yield {
        BaseLinker.linkClassDef(classDef, version, syntheticMethods, analysis)
      }
    }

    for {
      assembled <- Future.traverse(analysis.classInfos.values)(assembleClass)
    } yield {
      val (linkedClassDefs, linkedTopLevelExports) = assembled.unzip

      val globalInfo = new LinkedGlobalInfo(
        analysis.isClassSuperClassUsed
      )

      new LinkingUnit(linkedClassDefs.toList,
          linkedTopLevelExports.flatten.toList,
          moduleInitializers.toList,
          globalInfo)
    }
  }
}

private[frontend] object BaseLinker {

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private[frontend] def refineClassDef(classDef: ClassDef, version: Version,
      analysis: Analysis): (LinkedClass, List[LinkedTopLevelExport]) = {
    linkClassDef(classDef, version, syntheticMethodDefs = Nil, analysis)
  }

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private[frontend] def linkClassDef(classDef: ClassDef, version: Version,
      syntheticMethodDefs: List[MethodDef],
      analysis: Analysis): (LinkedClass, List[LinkedTopLevelExport]) = {
    import ir.Trees._

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
          markNeedsDesugaring(m)
      }
      .toList

    val (jsConstructor, jsMethodProps) = if (classInfo.isAnySubclassInstantiated) {
      val anyJSMemberNeedsDesugaring = classInfo.anyJSMemberNeedsDesugaring

      if (!anyJSMemberNeedsDesugaring) {
        (classDef.jsConstructor, classDef.jsMethodProps)
      } else {
        (classDef.jsConstructor.map(markNeedsDesugaring(_)),
            classDef.jsMethodProps.map(markNeedsDesugaring(_)))
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
      val maybeMarked =
        if (!infos.needsDesugaring) topLevelExport
        else markNeedsDesugaring(topLevelExport)
      new LinkedTopLevelExport(classDef.className, maybeMarked,
          infos.staticDependencies.toSet, infos.externalDependencies.toSet)
    }

    (linkedClass, linkedTopLevelExports)
  }

  private def markNeedsDesugaring(methodDef: MethodDef): MethodDef = {
    import methodDef._
    MethodDef(flags, name, originalName, args, resultType,
        Some(makeDesugarNode(body.get)))(
        optimizerHints, version)(
        pos)
  }

  private def markNeedsDesugaring(jsConstructorDef: JSConstructorDef): JSConstructorDef = {
    import jsConstructorDef._
    val newBody = JSConstructorBody(makeDesugarNode(Skip()(pos)) :: body.beforeSuper,
        body.superCall, body.afterSuper)
    JSConstructorDef(flags, args, restParam, newBody)(optimizerHints, version)(pos)
  }

  private def markNeedsDesugaring(jsMethodPropDef: JSMethodPropDef): JSMethodPropDef = {
    jsMethodPropDef match {
      case jsMethodDef: JSMethodDef =>
        import jsMethodDef._
        JSMethodDef(flags, name, args, restParam, makeDesugarNode(body))(
            optimizerHints, version)(pos)

      case jsPropDef: JSPropertyDef =>
        import jsPropDef._
        JSPropertyDef(flags, name,
            getterBody.map(makeDesugarNode(_)),
            setterArgAndBody.map(t => (t._1 -> makeDesugarNode(t._2))))(
            version)
    }
  }

  private def markNeedsDesugaring(exportDef: TopLevelExportDef): TopLevelExportDef = {
    exportDef match {
      case TopLevelMethodExportDef(exportName, jsMethodDef) =>
        TopLevelMethodExportDef(exportName,
            markNeedsDesugaring(jsMethodDef).asInstanceOf[JSMethodDef])(
            exportDef.pos)
      case _ =>
        exportDef
    }
  }

  private def makeDesugarNode(body: Tree): Tree =
    Transient(Desugarer.Transients.Desugar(body))(body.pos)
}
