/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.linker.frontend

import scala.annotation.tailrec

import scala.collection.mutable
import scala.util.Try

import org.scalajs.logging._
import org.scalajs.io._

import org.scalajs.linker._
import org.scalajs.linker.standard._
import org.scalajs.linker.checker._
import org.scalajs.linker.analyzer._
import org.scalajs.linker.irio._

import org.scalajs.ir
import ir.Trees._
import ir.Types._
import ir.ClassKind
import ir.Hashers
import ir.Position
import ir.Definitions

import Analysis._

/** Links the information from [[irio.VirtualScalaJSIRFile]]s into
 *  [[standard.LinkedClass LinkedClass]]es. Does a dead code elimination pass.
 */
final class BaseLinker(config: CommonPhaseConfig) {
  import BaseLinker._

  private val inputProvider = new InputProvider

  def link(irInput: Seq[VirtualScalaJSIRFile],
      moduleInitializers: Seq[ModuleInitializer], logger: Logger,
      symbolRequirements: SymbolRequirement, checkIR: Boolean): LinkingUnit = {

    inputProvider.update(irInput)

    val analysis = logger.time("Linker: Compute reachability") {
      val allSymbolRequirements = {
        symbolRequirements ++
        ModuleInitializer.toSymbolRequirement(moduleInitializers)
      }
      Analyzer.computeReachability(config,
          allSymbolRequirements, allowAddingSyntheticMethods = true,
          inputProvider)
    }

    if (analysis.errors.nonEmpty) {
      val maxDisplayErrors = {
        val propName = "org.scalajs.linker.maxlinkingerrors"
        Try(System.getProperty(propName, "20").toInt).getOrElse(20).max(1)
      }

      analysis.errors
        .take(maxDisplayErrors)
        .foreach(logError(_, logger, Level.Error))

      val skipped = analysis.errors.size - maxDisplayErrors
      if (skipped > 0)
        logger.log(Level.Error, s"Not showing $skipped more linking errors")

      throw new LinkingException("There were linking errors")
    }

    val linkResult = logger.time("Linker: Assemble LinkedClasses") {
      assemble(moduleInitializers, analysis)
    }

    if (checkIR) {
      logger.time("Linker: Check IR") {
        val errorCount = IRChecker.check(linkResult, inputProvider, logger)
        if (errorCount != 0) {
          throw new LinkingException(
              s"There were $errorCount IR checking errors.")
        }
      }
    }

    inputProvider.cleanAfterRun()

    linkResult
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

    def linkedSyntheticMethod(m: MethodDef) = {
      val version = m.hash.map(Hashers.hashAsVersion(_))
      new Versioned(m, version)
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

    // Synthetic members
    for {
      m <- analyzerInfo.methodInfos.valuesIterator
      if m.isReachable
    } {
      m.syntheticKind match {
        case MethodSyntheticKind.None =>
          // nothing to do

        case MethodSyntheticKind.ReflectiveProxy(targetName) =>
          val syntheticMDef = synthesizeReflectiveProxy(
              analyzerInfo, m, targetName, analysis)
          memberMethods += linkedSyntheticMethod(syntheticMDef)

        case MethodSyntheticKind.DefaultBridge(targetInterface) =>
          val syntheticMDef = synthesizeDefaultBridge(
              analyzerInfo, m, targetInterface, analysis)
          memberMethods += linkedSyntheticMethod(syntheticMDef)
      }
    }

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

  private def synthesizeReflectiveProxy(
      classInfo: Analysis.ClassInfo, methodInfo: Analysis.MethodInfo,
      targetName: String, analysis: Analysis): MethodDef = {
    val encodedName = methodInfo.encodedName

    val targetMDef = findInheritedMethodDef(analysis, classInfo, targetName)

    implicit val pos = targetMDef.pos

    val targetIdent = targetMDef.name.asInstanceOf[Ident].copy() // for the new pos
    val proxyIdent = Ident(encodedName, None)
    val params = targetMDef.args.map(_.copy()) // for the new pos
    val currentClassType = ClassType(classInfo.encodedName)

    val call = Apply(This()(currentClassType),
        targetIdent, params.map(_.ref))(targetMDef.resultType)

    val body = if (targetName.endsWith("__V")) {
      // Materialize an `undefined` result for void methods
      Block(call, Undefined())
    } else {
      call
    }

    MethodDef(static = false, proxyIdent, params, AnyType, Some(body))(
        OptimizerHints.empty, targetMDef.hash)
  }

  private def synthesizeDefaultBridge(
      classInfo: Analysis.ClassInfo, methodInfo: Analysis.MethodInfo,
      targetInterface: String, analysis: Analysis): MethodDef = {
    val encodedName = methodInfo.encodedName

    val targetInterfaceInfo = analysis.classInfos(targetInterface)
    val targetMDef = findMethodDef(targetInterfaceInfo, encodedName)

    implicit val pos = targetMDef.pos

    val targetIdent = targetMDef.name.asInstanceOf[Ident].copy() // for the new pos
    val bridgeIdent = targetIdent
    val params = targetMDef.args.map(_.copy()) // for the new pos
    val currentClassType = ClassType(classInfo.encodedName)

    val body = ApplyStatically(
        This()(currentClassType), ClassType(targetInterface), targetIdent,
        params.map(_.ref))(targetMDef.resultType)

    MethodDef(static = false, bridgeIdent, params, targetMDef.resultType, Some(body))(
        OptimizerHints.empty, targetMDef.hash)
  }

  private def findInheritedMethodDef(analysis: Analysis,
      classInfo: Analysis.ClassInfo, methodName: String,
      p: Analysis.MethodInfo => Boolean = _ => true): MethodDef = {
    @tailrec
    def loop(ancestorInfo: Analysis.ClassInfo): MethodDef = {
      val inherited = ancestorInfo.methodInfos.get(methodName)
      inherited.find(p) match {
        case Some(m) =>
          m.syntheticKind match {
            case MethodSyntheticKind.None =>
              findMethodDef(ancestorInfo, methodName)

            case MethodSyntheticKind.DefaultBridge(targetInterface) =>
              val targetInterfaceInfo = analysis.classInfos(targetInterface)
              findMethodDef(targetInterfaceInfo, methodName)

            case MethodSyntheticKind.ReflectiveProxy(_) =>
              throw new AssertionError(
                  s"Cannot recursively follow $ancestorInfo.$methodName of " +
                  s"kind ${m.syntheticKind}")
          }

        case None =>
          assert(ancestorInfo.superClass.isDefined,
              s"Could not find $methodName anywhere in ${classInfo.encodedName}")
          loop(ancestorInfo.superClass.get)
      }
    }

    loop(classInfo)
  }

  private def findMethodDef(classInfo: Analysis.ClassInfo,
      methodName: String): MethodDef = {
    val classDef = inputProvider.loadClassDef(classInfo.encodedName)
    classDef.memberDefs.collectFirst {
      case mDef: MethodDef
          if !mDef.static && mDef.encodedName == methodName => mDef
    }.getOrElse {
      throw new AssertionError(
          s"Cannot find $methodName in ${classInfo.encodedName}")
    }
  }
}

private object BaseLinker {
  private class InputProvider extends Analyzer.InputProvider {
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

    def classesWithEntryPoints(): TraversableOnce[String] = {
      for {
        irFile <- encodedNameToFile.valuesIterator
        entryPointsInfo = irFile.entryPointsInfo
        if entryPointsInfo.hasEntryPoint
      } yield {
        entryPointsInfo.encodedName
      }
    }

    def loadInfo(encodedName: String): Option[Infos.ClassInfo] =
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

    def loadInfo(irFile: VirtualScalaJSIRFile): Infos.ClassInfo = {
      update(irFile)
      info
    }

    def loadClassDefAndVersion(
        irFile: VirtualScalaJSIRFile): (ClassDef, Option[String]) = {
      update(irFile)
      (classDef, version)
    }

    def update(irFile: VirtualScalaJSIRFile): Unit = {
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
    def cleanAfterRun(): Boolean = {
      val result = cacheUsed
      cacheUsed = false
      result
    }
  }
}
