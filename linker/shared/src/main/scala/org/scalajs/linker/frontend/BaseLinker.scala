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
import org.scalajs.linker.checker._
import org.scalajs.linker.analyzer._

import org.scalajs.ir
import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.Trees.{ClassDef, MethodDef}
import org.scalajs.ir.Hashers

import Analysis._

/** Links the information from [[interface.IRFile IRFile]]s into
 *  [[standard.LinkedClass LinkedClass]]es. Does a dead code elimination pass.
 */
final class BaseLinker(config: CommonPhaseConfig) {
  import BaseLinker._

  private val irLoader = new IRLoader
  private val methodSynthesizer = new MethodSynthesizer(irLoader)

  def link(irInput: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer], logger: Logger,
      symbolRequirements: SymbolRequirement, checkIR: Boolean)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {

    val allSymbolRequirements = {
      symbolRequirements ++
      SymbolRequirement.fromModuleInitializer(moduleInitializers)
    }

    val result = for {
      _ <- irLoader.update(irInput)
      analysis <- logger.timeFuture("Linker: Compute reachability") {
        analyze(allSymbolRequirements, logger)
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

  private def analyze(symbolRequirements: SymbolRequirement, logger: Logger)(
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
      analysis <- Analyzer.computeReachability(config, symbolRequirements,
          allowAddingSyntheticMethods = true, checkAbstractReachability = true,
          irLoader)
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
        val linkedTopLevelExports =
          classDef.topLevelExportDefs.map(new LinkedTopLevelExport(className, _))
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
  private def linkedClassDef(classDef: ClassDef, version: Option[String],
      syntheticMethodDefs: Iterator[MethodDef],
      analyzerInfo: ClassInfo): LinkedClass = {
    import ir.Trees._

    val fields = List.newBuilder[AnyFieldDef]
    val methods = List.newBuilder[Versioned[MethodDef]]
    val jsNativeMembers = List.newBuilder[JSNativeMemberDef]
    val exportedMembers = List.newBuilder[Versioned[JSMethodPropDef]]

    def linkedMethod(m: MethodDef) = {
      val version = m.hash.map(Hashers.hashAsVersion(_))
      new Versioned(m, version)
    }

    classDef.memberDefs.foreach {
      case field: AnyFieldDef =>
        if (isFieldDefNeeded(analyzerInfo, field))
          fields += field

      case m: MethodDef =>
        val methodInfo =
          analyzerInfo.methodInfos(m.flags.namespace)(m.methodName)

        /* Synthetic methods take precedence over non-synthetic ones.
         * This happens if a class has its own and foreign static initializers.
         */
        val isSynthetic =
          methodInfo.syntheticKind != MethodSyntheticKind.None

        if (methodInfo.isReachable && !isSynthetic) {
          assert(m.body.isDefined,
              s"The abstract method ${classDef.name.name}.${m.methodName} " +
              "is reachable.")
          methods += linkedMethod(m)
        }

      case m: JSMethodDef =>
        if (analyzerInfo.isAnySubclassInstantiated) {
          val version = m.hash.map(Hashers.hashAsVersion(_))
          exportedMembers += new Versioned(m, version)
        }

      case m: JSPropertyDef =>
        if (analyzerInfo.isAnySubclassInstantiated)
          exportedMembers += new Versioned(m, None)

      case m: JSNativeMemberDef =>
        if (analyzerInfo.jsNativeMembersUsed.contains(m.name.name))
          jsNativeMembers += m
    }

    methods ++= syntheticMethodDefs.map(linkedMethod)

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
        fields.result(),
        methods.result(),
        exportedMembers.result(),
        jsNativeMembers.result(),
        classDef.optimizerHints,
        classDef.pos,
        ancestors.toList,
        hasInstances = analyzerInfo.isAnySubclassInstantiated,
        hasInstanceTests = analyzerInfo.areInstanceTestsUsed,
        hasRuntimeTypeInfo = analyzerInfo.isDataAccessed,
        version)
  }
}

private[frontend] object BaseLinker {
  private[frontend] def isFieldDefNeeded(classInfo: ClassInfo,
      field: ir.Trees.AnyFieldDef): Boolean = {
    import ir.Trees._

    field match {
      case field: FieldDef =>
        if (field.flags.namespace.isStatic) classInfo.isAnyStaticFieldUsed
        else if (classInfo.kind.isJSType) classInfo.isAnyPrivateJSFieldUsed
        else classInfo.isAnySubclassInstantiated

      case field: JSFieldDef =>
        classInfo.isAnySubclassInstantiated
    }
  }
}
