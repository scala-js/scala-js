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
import scala.util.Try

import org.scalajs.logging._
import org.scalajs.io._

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.checker._
import org.scalajs.linker.analyzer._
import org.scalajs.linker.irio._

import org.scalajs.ir
import ir.Trees.{ClassDef, MethodDef}
import ir.Hashers

import Analysis._

/** Links the information from [[irio.VirtualScalaJSIRFile]]s into
 *  [[standard.LinkedClass LinkedClass]]es. Does a dead code elimination pass.
 */
final class BaseLinker(config: CommonPhaseConfig) {
  import BaseLinker._

  private val inputProvider = new InputProvider
  private val methodSynthesizer = new MethodSynthesizer(inputProvider)

  def link(irInput: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer], logger: Logger,
      symbolRequirements: SymbolRequirement, checkIR: Boolean)(
      implicit ex: ExecutionContext): Future[LinkingUnit] = {

    inputProvider.update(irInput)

    val allSymbolRequirements = {
      symbolRequirements ++
      ModuleInitializer.toSymbolRequirement(moduleInitializers)
    }

    for {
      analysis <- logger.timeFuture("Linker: Compute reachability") {
        analyze(allSymbolRequirements, logger)
      }
    } yield {
      val linkResult = logger.time("Linker: Assemble LinkedClasses") {
        assemble(moduleInitializers, analysis)
      }

      if (checkIR) {
        logger.time("Linker: Check IR") {
          val errorCount = IRChecker.check(linkResult, logger)
          if (errorCount != 0) {
            throw new LinkingException(
                s"There were $errorCount IR checking errors.")
          }
        }
      }

      inputProvider.cleanAfterRun()

      linkResult
    }
  }

  private def analyze(symbolRequirements: SymbolRequirement, logger: Logger)(
      implicit ex: ExecutionContext): Future[Analysis] = {
    def reportErrors(errors: Seq[Analysis.Error]) = {
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
          allowAddingSyntheticMethods = true, inputProvider)
    } yield {
      if (analysis.errors.nonEmpty) {
        reportErrors(analysis.errors)
      }

      analysis
    }
  }

  private def assemble(moduleInitializers: Seq[ModuleInitializer],
      analysis: Analysis): LinkingUnit = {
    val linkedClassDefs = for {
      analyzerInfo <- analysis.classInfos.values
    } yield {
      linkedClassDef(analyzerInfo, analysis)
    }

    new LinkingUnit(config.coreSpec, linkedClassDefs.toList,
        moduleInitializers.toList)
  }

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private def linkedClassDef(analyzerInfo: Analysis.ClassInfo,
      analysis: Analysis): LinkedClass = {
    import ir.Trees._

    val (classDef, version) =
      inputProvider.loadClassDefAndVersion(analyzerInfo.encodedName)

    val fields = mutable.Buffer.empty[FieldDef]
    val staticMethods = mutable.Buffer.empty[Versioned[MethodDef]]
    val memberMethods = mutable.Buffer.empty[Versioned[MethodDef]]
    val exportedMembers = mutable.Buffer.empty[Versioned[MemberDef]]

    def linkedMethod(m: MethodDef) = {
      val version = m.hash.map(Hashers.hashAsVersion(_))
      new Versioned(m, version)
    }

    def linkedProperty(p: PropertyDef) = {
      new Versioned(p, None)
    }

    classDef.memberDefs.foreach {
      case field: FieldDef =>
        if (analyzerInfo.isAnySubclassInstantiated)
          fields += field

      case m: MethodDef =>
        val methodInfo =
          if (m.static) analyzerInfo.staticMethodInfos(m.encodedName)
          else analyzerInfo.methodInfos(m.encodedName)

        if (methodInfo.isReachable) {
          assert(m.body.isDefined,
              s"The abstract method ${classDef.name.name}.${m.encodedName} " +
              "is reachable.")
          val linked = linkedMethod(m)
          if (m.name.isInstanceOf[Ident]) {
            if (m.static)
              staticMethods += linked
            else
              memberMethods += linked
          } else {
            exportedMembers += linked
          }
        }

      case m: PropertyDef =>
        if (analyzerInfo.isAnySubclassInstantiated)
          exportedMembers += linkedProperty(m)
    }

    memberMethods ++= methodSynthesizer.synthesizeMembers(analyzerInfo, analysis)
      .map(linkedMethod)

    val topLevelExports =
      classDef.topLevelExportDefs.map(new Versioned(_, version))

    val kind =
      if (analyzerInfo.isModuleAccessed) classDef.kind
      else classDef.kind.withoutModuleAccessor

    val ancestors = analyzerInfo.ancestors.map(_.encodedName)

    new LinkedClass(
        classDef.name,
        kind,
        classDef.jsClassCaptures,
        classDef.superClass,
        classDef.interfaces,
        classDef.jsSuperClass,
        classDef.jsNativeLoadSpec,
        fields.toList,
        staticMethods.toList,
        memberMethods.toList,
        exportedMembers.toList,
        topLevelExports,
        classDef.optimizerHints,
        classDef.pos,
        ancestors.toList,
        hasInstances = analyzerInfo.isAnySubclassInstantiated,
        hasInstanceTests = analyzerInfo.areInstanceTestsUsed,
        hasRuntimeTypeInfo = analyzerInfo.isDataAccessed,
        version)
  }
}

private object BaseLinker {
  private class InputProvider extends Analyzer.InputProvider with MethodSynthesizer.InputProvider {
    private var encodedNameToFile: collection.Map[String, VirtualScalaJSIRFile] = _
    private val cache = mutable.Map.empty[String, ClassDefAndInfoCache]

    def update(irInput: Seq[VirtualScalaJSIRFile]): Unit = {
      val encodedNameToFile = mutable.Map.empty[String, VirtualScalaJSIRFile]
      for (irFile <- irInput) {
        // Remove duplicates. Just like the JVM
        val encodedName = irFile.entryPointsInfo.encodedName
        if (!encodedNameToFile.contains(encodedName))
          encodedNameToFile += encodedName -> irFile
      }
      this.encodedNameToFile = encodedNameToFile
    }

    def classesWithEntryPoints()(
        implicit ex: ExecutionContext): Future[TraversableOnce[String]] = {
      val infos = Future.traverse(encodedNameToFile.valuesIterator)(
          irFile => Future(irFile.entryPointsInfo))

      infos.map(_.withFilter(_.hasEntryPoint).map(_.encodedName))
    }

    def loadInfo(encodedName: String)(
        implicit ex: ExecutionContext): Option[Future[Infos.ClassInfo]] =
      getCache(encodedName).map(_.loadInfo(encodedNameToFile(encodedName)))

    def loadClassDefAndVersion(
        encodedName: String): (ClassDef, Option[String]) = {
      val fileCache = getCache(encodedName).getOrElse {
        throw new AssertionError(s"Cannot load file for class $encodedName")
      }
      fileCache.loadClassDefAndVersion(encodedNameToFile(encodedName))
    }

    def loadClassDef(encodedName: String): ClassDef =
      loadClassDefAndVersion(encodedName)._1

    private def getCache(encodedName: String): Option[ClassDefAndInfoCache] = {
      cache.get(encodedName).orElse {
        if (encodedNameToFile.contains(encodedName)) {
          val fileCache = new ClassDefAndInfoCache
          cache += encodedName -> fileCache
          Some(fileCache)
        } else {
          None
        }
      }
    }

    def cleanAfterRun(): Unit = {
      encodedNameToFile = null
      cache.retain((_, fileCache) => fileCache.cleanAfterRun())
    }
  }

  private final class ClassDefAndInfoCache {
    private var cacheUsed: Boolean = false
    private var version: Option[String] = None
    private var classDef: ClassDef = _
    private var info: Infos.ClassInfo = _

    def loadInfo(irFile: VirtualScalaJSIRFile)(
        implicit ex: ExecutionContext): Future[Infos.ClassInfo] = Future {
      update(irFile)
      info
    }

    def loadClassDefAndVersion(
        irFile: VirtualScalaJSIRFile): (ClassDef, Option[String]) = {
      update(irFile)
      (classDef, version)
    }

    def update(irFile: VirtualScalaJSIRFile): Unit = synchronized {
      /* If the cache was already used in this run, the classDef and info are
       * already correct, no matter what the versions say.
       */
      if (!cacheUsed) {
        cacheUsed = true

        val newVersion = irFile.version
        if (version.isEmpty || newVersion.isEmpty ||
            version.get != newVersion.get) {
          classDef = irFile.tree
          info = Infos.generateClassInfo(classDef)
          version = newVersion
        }
      }
    }

    /** Returns true if the cache has been used and should be kept. */
    def cleanAfterRun(): Boolean = synchronized {
      val result = cacheUsed
      cacheUsed = false
      result
    }
  }
}
