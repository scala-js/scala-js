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

import scala.concurrent._
import scala.util.Try

import org.scalajs.logging._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.checker._
import org.scalajs.linker.analyzer._

import org.scalajs.ir
import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.Trees.{ClassDef, MethodDef}
import org.scalajs.ir.Version

import Analysis._

/** Links the information from [[interface.IRFile IRFile]]s into
 *  [[standard.LinkedClass LinkedClass]]es. Does a dead code elimination pass.
 */
final class BaseLinker(config: CommonPhaseConfig, checkIR: Boolean) {
  import BaseLinker._

  private val irLoader = new IRLoader(checkIR)
  private val methodSynthesizer = new MethodSynthesizer(irLoader)

  def link(irInput: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer], logger: Logger,
      symbolRequirements: SymbolRequirement)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {

    val result = for {
      _ <- irLoader.update(irInput, logger)
      analysis <- logger.timeFuture("Linker: Compute reachability") {
        analyze(moduleInitializers, symbolRequirements, logger)
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

    result.andThen { case _ => irLoader.cleanAfterRun() }
  }

  private def analyze(moduleInitializers: Seq[ModuleInitializer],
      symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ec: ExecutionContext): Future[Analysis] = {
    def reportErrors(errors: scala.collection.Seq[Analysis.Error]) = {
      require(errors.nonEmpty)

      val maxDisplayErrors = {
        val propName = "org.scalajs.linker.maxlinkingerrors"
        Try(System.getProperty(propName, "20").toInt).getOrElse(20).max(1)
      }

      errors
        .take(maxDisplayErrors)
        .foreach(logError(_, logger, Level.Error))

      val skipped = errors.size - maxDisplayErrors
      if (skipped > 0)
        logger.log(Level.Error, s"Not showing $skipped more linking errors")

      throw new LinkingException("There were linking errors")
    }

    for {
      analysis <- Analyzer.computeReachability(config, moduleInitializers,
          symbolRequirements, allowAddingSyntheticMethods = true,
          checkAbstractReachability = true, irLoader)
    } yield {
      if (analysis.errors.nonEmpty) {
        reportErrors(analysis.errors)
      }

      analysis
    }
  }

  private def assemble(moduleInitializers: Seq[ModuleInitializer],
      analysis: Analysis)(implicit ec: ExecutionContext): Future[LinkingUnit] = {
    def assembleClass(info: ClassInfo) = {
      val className = info.className
      val classAndVersion = irLoader.loadClassDefAndVersion(className)
      val syntheticMethods = methodSynthesizer.synthesizeMembers(info, analysis)

      for {
        (classDef, version) <- classAndVersion
        syntheticMethods <- syntheticMethods
      } yield {
        val linkedClass = linkedClassDef(classDef, version, syntheticMethods, info)
        val linkedTopLevelExports = for {
          topLevelExport <- classDef.topLevelExportDefs
        } yield {
          val infos = analysis.topLevelExportInfos(
              (ModuleID(topLevelExport.moduleID), topLevelExport.topLevelExportName))
          new LinkedTopLevelExport(className, topLevelExport,
              infos.staticDependencies.toSet, infos.externalDependencies.toSet)
        }

        (linkedClass, linkedTopLevelExports)
      }
    }

    for {
      assembled <- Future.traverse(analysis.classInfos.values)(assembleClass)
    } yield {
      val (linkedClassDefs, linkedTopLevelExports) = assembled.unzip

      new LinkingUnit(config.coreSpec, linkedClassDefs.toList,
          linkedTopLevelExports.flatten.toList,
          moduleInitializers.toList)
    }
  }

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private def linkedClassDef(classDef: ClassDef, version: Version,
      syntheticMethodDefs: Iterator[MethodDef],
      analyzerInfo: ClassInfo): LinkedClass = {
    import ir.Trees._

    val fields = classDef.fields.filter(isFieldDefNeeded(analyzerInfo, _))

    val methods = classDef.methods.filter { m =>
      val methodInfo =
        analyzerInfo.methodInfos(m.flags.namespace)(m.methodName)

      val reachable = methodInfo.isReachable
      assert(m.body.isDefined || !reachable,
          s"The abstract method ${classDef.name.name}.${m.methodName} " +
          "is reachable.")

      reachable
    }

    val jsConstructor =
      if (analyzerInfo.isAnySubclassInstantiated) classDef.jsConstructor
      else None

    val jsMethodProps =
      if (analyzerInfo.isAnySubclassInstantiated) classDef.jsMethodProps
      else Nil

    val jsNativeMembers = classDef.jsNativeMembers
      .filter(m => analyzerInfo.jsNativeMembersUsed.contains(m.name.name))

    val allMethods = methods ++ syntheticMethodDefs

    val kind =
      if (analyzerInfo.isModuleAccessed) classDef.kind
      else classDef.kind.withoutModuleAccessor

    val ancestors = analyzerInfo.ancestors.map(_.className)

    new LinkedClass(
        classDef.name,
        kind,
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
        hasInstances = analyzerInfo.isAnySubclassInstantiated,
        hasInstanceTests = analyzerInfo.areInstanceTestsUsed,
        hasRuntimeTypeInfo = analyzerInfo.isDataAccessed,
        fieldsRead = analyzerInfo.fieldsRead.toSet,
        staticFieldsRead = analyzerInfo.staticFieldsRead.toSet,
        staticDependencies = analyzerInfo.staticDependencies.toSet,
        externalDependencies = analyzerInfo.externalDependencies.toSet,
        dynamicDependencies = analyzerInfo.dynamicDependencies.toSet,
        version)
  }
}

private[frontend] object BaseLinker {
  private[frontend] def isFieldDefNeeded(classInfo: ClassInfo,
      field: ir.Trees.AnyFieldDef): Boolean = {
    import ir.Trees._

    field match {
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
  }
}
