/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.collection.mutable
import scala.collection.immutable.{Seq, Traversable}

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.javascript.OutputMode
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._

import org.scalajs.core.ir
import ir.Infos
import ir.Trees.ClassDef
import ir.ClassKind
import ir.Hashers
import ir.Position
import ir.Definitions

import ScalaJSOptimizer.NoWarnMissing
import Analysis._

/** Links the information from [[VirtualScalaJSIRFile]]s into
 *  [[LinkedClassDef]]s. Does a dead code elimination pass.
 */
final class Linker(semantics: Semantics, outputMode: OutputMode,
    considerPositions: Boolean) {

  @deprecated("Use the overload with an explicit output mode", "0.6.3")
  def this(semantics: Semantics, considerPositions: Boolean) =
    this(semantics, OutputMode.ECMAScript51Isolated, considerPositions)

  private[this] val files = mutable.Map.empty[String, PersistentIRFile]

  private[this] var statsReused: Int = 0
  private[this] var statsInvalidated: Int = 0
  private[this] var statsTreesRead: Int = 0

  type TreeProvider = String => (ClassDef, Option[String])

  /** Cleans the cache. */
  def clean(): Unit = {
    files.clear()
    statsReused = 0
    statsInvalidated = 0
    statsTreesRead = 0
  }

  def link(irInput: Traversable[VirtualScalaJSIRFile], logger: Logger,
      reachOptimizerSymbols: Boolean, bypassLinkingErrors: Boolean,
      noWarnMissing: Seq[NoWarnMissing],
      @deprecatedName('checkInfos) checkIR: Boolean): LinkingUnit = {
    startRun()

    val encodedNameToPersistentFile =
      logTime(logger, "Linker: Read info")(updateFiles(irInput))

    val infos = encodedNameToPersistentFile.values.map(_.info).toList

    val getTree: TreeProvider = { name =>
      val pf = encodedNameToPersistentFile(name)
      (pf.tree, pf.version)
    }

    try {
      link(infos, getTree, logger, reachOptimizerSymbols,
          bypassLinkingErrors, noWarnMissing, checkIR)
    } finally {
      endRun(logger)
    }
  }

  def link(infoInput: List[Infos.ClassInfo], getTree: TreeProvider,
      logger: Logger, reachOptimizerSymbols: Boolean,
      bypassLinkingErrors: Boolean,
      noWarnMissing: Seq[NoWarnMissing],
      @deprecatedName('checkInfos) checkIR: Boolean): LinkingUnit = {

    if (checkIR) {
      logTime(logger, "Linker: Check Infos") {
        val infoAndTrees =
          infoInput.map(info => (info, getTree(info.encodedName)._1))
        val checker = new InfoChecker(infoAndTrees, logger)
        if (!checker.check())
          sys.error(s"There were ${checker.errorCount} Info checking errors.")
      }
    }

    val analysis = logTime(logger, "Linker: Compute reachability") {
      val analyzer = new Analyzer(semantics, outputMode, reachOptimizerSymbols)
      analyzer.computeReachability(infoInput)
    }

    val bypass = bypassLinkingErrors || noWarnMissing.nonEmpty
    val linkingErrLevel = if (bypass) Level.Warn else Level.Error
    val filteredErrors = filterErrors(analysis.errors, noWarnMissing)
    filteredErrors.foreach(logError(_, logger, linkingErrLevel))

    if (analysis.errors.nonEmpty && !bypass)
      sys.error("There were linking errors")

    val linkResult = logTime(logger, "Linker: Assemble LinkedClasses") {
      assemble(infoInput, getTree, analysis)
    }

    if (checkIR) {
      logTime(logger, "Linker: Check IR") {
        if (linkResult.isComplete) {
          val checker = new IRChecker(linkResult, logger)
          if (!checker.check())
            sys.error(s"There were ${checker.errorCount} IR checking errors.")
        } else if (noWarnMissing.isEmpty)
          sys.error("Could not check IR because there where linking errors.")
      }
    }

    linkResult
  }

  private def filterErrors(errors: scala.collection.Seq[Error],
      noWarnMissing: Seq[NoWarnMissing]) = {
    import NoWarnMissing._

    val classNoWarns = mutable.Set.empty[String]
    val methodNoWarnsBuf = mutable.Buffer.empty[Method]

    // Basically a type safe partition
    noWarnMissing.foreach {
      case Class(className) =>
        classNoWarns += className
      case m: Method =>
        methodNoWarnsBuf += m
    }

    val methodNoWarns = for {
      (className, elems) <- methodNoWarnsBuf.groupBy(_.className)
    } yield {
      className -> elems.map(_.methodName).toSet
    }

    errors filterNot {
      case MissingClass(info, _) =>
        classNoWarns.contains(info.encodedName)
      case MissingMethod(info, _) =>
        classNoWarns.contains(info.owner.encodedName) ||
        methodNoWarns.get(info.owner.encodedName).exists { setf =>
          setf(info.encodedName)
        }
      case _ =>
        false
    }
  }

  private def assemble(infoInput: List[Infos.ClassInfo],
      getTree: TreeProvider, analysis: Analysis) = {
    val infoByName = Map(infoInput.map(c => c.encodedName -> c): _*)

    def optClassDef(analyzerInfo: Analysis.ClassInfo) = {
      val encodedName = analyzerInfo.encodedName

      def optDummyParent =
        if (!analyzerInfo.isAnySubclassInstantiated) None
        else Some(LinkedClass.dummyParent(encodedName, Some("dummy")))

      infoByName.get(encodedName).map { info =>
        val (tree, version) = getTree(encodedName)
        val newVersion = version.map("real" + _) // avoid collision with dummy
        linkedClassDef(info, tree, analyzerInfo, newVersion)
      }.orElse(optDummyParent)
    }

    val linkedClassDefs = for {
      classInfo <- analysis.classInfos.values
      if classInfo.isNeededAtAll
      linkedClassDef <- optClassDef(classInfo)
    } yield linkedClassDef

    new LinkingUnit(linkedClassDefs.toList, infoByName, analysis.allAvailable)
  }

  /** Takes a Infos, a ClassDef and DCE infos to construct a stripped down
   *  LinkedClassDef */
  private def linkedClassDef(info: Infos.ClassInfo, classDef: ClassDef,
      analyzerInfo: Analysis.ClassInfo, version: Option[String]) = {
    import ir.Trees._

    val memberInfoByName = Map(info.methods.map(m => m.encodedName -> m): _*)

    val fields = mutable.Buffer.empty[FieldDef]
    val staticMethods = mutable.Buffer.empty[LinkedMember[MethodDef]]
    val memberMethods = mutable.Buffer.empty[LinkedMember[MethodDef]]
    val abstractMethods = mutable.Buffer.empty[LinkedMember[MethodDef]]
    val exportedMembers = mutable.Buffer.empty[LinkedMember[Tree]]
    val classExports = mutable.Buffer.empty[Tree]

    def linkedMethod(m: MethodDef) = {
      val info = memberInfoByName(m.name.name)
      val version = m.hash.map(Hashers.hashAsVersion(_, considerPositions))
      new LinkedMember(info, m, version)
    }

    def linkedProperty(p: PropertyDef) = {
      val info = memberInfoByName(p.name.name)
      new LinkedMember(info, p, None)
    }

    classDef.defs.foreach {
      // Static methods
      case m: MethodDef if m.static =>
        if (analyzerInfo.staticMethodInfos(m.name.name).isReachable)
          staticMethods += linkedMethod(m)

      // Fields
      case field @ FieldDef(_, _, _) =>
        if (analyzerInfo.isAnySubclassInstantiated)
          fields += field

      // Normal methods
      case m: MethodDef =>
        if (analyzerInfo.methodInfos(m.name.name).isReachable) {
          if (m.name.isInstanceOf[StringLiteral])
            exportedMembers += linkedMethod(m)
          else if (m.body == EmptyTree)
            abstractMethods += linkedMethod(m)
          else
            memberMethods += linkedMethod(m)
        }

      case m: PropertyDef =>
        if (analyzerInfo.isAnySubclassInstantiated)
          exportedMembers += linkedProperty(m)

      case e: ConstructorExportDef =>
        classExports += e

      case e: ModuleExportDef =>
        classExports += e

      case tree =>
        sys.error(s"Illegal tree in ClassDef of class ${tree.getClass}")
    }

    val classExportInfo =
      memberInfoByName.get(Definitions.ExportedConstructorsName)

    val kind = {
      if (classDef.kind == ClassKind.ModuleClass &&
          !analyzerInfo.isModuleAccessed)
        ClassKind.Class
      else
        classDef.kind
    }

    val ancestors = analyzerInfo.ancestors.map(_.encodedName)

    new LinkedClass(
        classDef.name,
        classDef.kind,
        classDef.superClass,
        classDef.interfaces,
        classDef.jsName,
        fields.toList,
        staticMethods.toList,
        memberMethods.toList,
        abstractMethods.toList,
        exportedMembers.toList,
        classExports.toList,
        classExportInfo,
        classDef.optimizerHints,
        classDef.pos,
        ancestors.toList,
        hasInstances = analyzerInfo.isAnySubclassInstantiated,
        hasInstanceTests = analyzerInfo.areInstanceTestsUsed,
        hasRuntimeTypeInfo = analyzerInfo.isDataAccessed,
        version)
  }

  private def startRun(): Unit = {
    statsReused = 0
    statsInvalidated = 0
    statsTreesRead = 0
    for (file <- files.values)
      file.startRun()
  }

  private def updateFiles(irFiles: Traversable[VirtualScalaJSIRFile]) = {
    val tups = for (irFile <- irFiles) yield {
      val file = files.getOrElseUpdate(irFile.path,
        new PersistentIRFile(irFile.path))
      file.updateFile(irFile)
      file.info.encodedName -> file
    }
    tups.toMap
  }

  private def endRun(logger: Logger): Unit = {
    logger.debug(
        s"Linker: cache stats: reused: $statsReused -- "+
        s"invalidated: $statsInvalidated -- "+
        s"trees read: $statsTreesRead")

    // "Garbage-collect" persisted versions of files that have disappeared
    files.retain((_, f) => f.cleanAfterRun())
  }

  private final class PersistentIRFile(val path: String) {
    import ir.Trees._

    private[this] var existedInThisRun: Boolean = false

    private[this] var _irFile: VirtualScalaJSIRFile = null
    private[this] var _version: Option[String] = None
    private[this] var _info: Infos.ClassInfo = null
    private[this] var _tree: ClassDef = null

    def startRun(): Unit = {
      existedInThisRun = false
    }

    def updateFile(irFile: VirtualScalaJSIRFile): Unit = {
      existedInThisRun = true
      _irFile = irFile
      if (_version.isDefined && _version == _irFile.version) {
        // yeepeeh, nothing to do
        statsReused += 1
      } else {
        _version = irFile.version
        _info = irFile.info
        _tree = null
        statsInvalidated += 1
      }
    }

    def version: Option[String] = _version

    def info: Infos.ClassInfo = _info

    def tree: ClassDef = {
      if (_tree == null) {
        statsTreesRead += 1
        _tree = _irFile.tree
      }
      _tree
    }

    def treeIfChanged(lastVersion: Option[String]): Option[(ClassDef, Option[String])] = {
      if (lastVersion.isDefined && lastVersion == version) None
      else Some((tree, version))
    }

    /** Returns true if this file should be kept for the next run at all. */
    def cleanAfterRun(): Boolean = {
      _irFile = null
      existedInThisRun
    }
  }

}
