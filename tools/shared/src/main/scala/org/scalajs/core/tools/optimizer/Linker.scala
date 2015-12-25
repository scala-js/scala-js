/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.annotation.tailrec

import scala.collection.mutable
import scala.collection.immutable.{Seq, Traversable}

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.javascript.OutputMode
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._

import org.scalajs.core.ir
import ir.Infos
import ir.Trees._
import ir.Types._
import ir.ClassKind
import ir.Hashers
import ir.Position
import ir.Definitions

import Analysis._

/** Links the information from [[io.VirtualScalaJSIRFile]]s into
 *  [[LinkedClass]]es. Does a dead code elimination pass.
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
      checkIR: Boolean): LinkingUnit = {
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
          bypassLinkingErrors, checkIR)
    } finally {
      endRun(logger)
    }
  }

  def link(infoInput: List[Infos.ClassInfo], getTree: TreeProvider,
      logger: Logger, reachOptimizerSymbols: Boolean,
      bypassLinkingErrors: Boolean, checkIR: Boolean): LinkingUnit = {

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
      val analyzer = new Analyzer(semantics, outputMode, reachOptimizerSymbols,
          initialLink = true)
      analyzer.computeReachability(infoInput)
    }

    val linkingErrLevel = if (bypassLinkingErrors) Level.Warn else Level.Error
    analysis.errors.foreach(logError(_, logger, linkingErrLevel))

    if (analysis.errors.nonEmpty && !bypassLinkingErrors)
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
        } else {
          sys.error("Could not check IR because there where linking errors.")
        }
      }
    }

    linkResult
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
        linkedClassDef(info, tree, analyzerInfo, newVersion, getTree, analysis)
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
      analyzerInfo: Analysis.ClassInfo, version: Option[String],
      getTree: TreeProvider, analysis: Analysis) = {
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

    def linkedSyntheticMethod(m: MethodDef) = {
      val info = Infos.generateMethodInfo(m)
      val version = m.hash.map(Hashers.hashAsVersion(_, considerPositions))
      new LinkedMember(info, m, version)
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

      case e: JSClassExportDef =>
        classExports += e

      case e: ModuleExportDef =>
        classExports += e

      case tree =>
        sys.error(s"Illegal tree in ClassDef of class ${tree.getClass}")
    }

    // Synthetic members
    for {
      m <- analyzerInfo.methodInfos.valuesIterator
      if m.isReachable
    } {
      m.syntheticKind match {
        case MethodSyntheticKind.None =>
          // nothing to do

        case MethodSyntheticKind.InheritedConstructor =>
          val syntheticMDef = synthesizeInheritedConstructor(
              analyzerInfo, m, getTree, analysis)(classDef.pos)
          memberMethods += linkedSyntheticMethod(syntheticMDef)
      }
    }

    val classExportInfo =
      memberInfoByName.get(Definitions.ExportedConstructorsName)

    val kind =
      if (analyzerInfo.isModuleAccessed) classDef.kind
      else classDef.kind.withoutModuleAccessor

    val ancestors = analyzerInfo.ancestors.map(_.encodedName)

    new LinkedClass(
        classDef.name,
        kind,
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

  private def synthesizeInheritedConstructor(
      classInfo: Analysis.ClassInfo, methodInfo: Analysis.MethodInfo,
      getTree: TreeProvider, analysis: Analysis)(
      implicit pos: Position): MethodDef = {
    val encodedName = methodInfo.encodedName

    @tailrec
    def findInheritedMethodDef(ancestorInfo: Analysis.ClassInfo): MethodDef = {
      val inherited = ancestorInfo.methodInfos(methodInfo.encodedName)
      if (inherited.syntheticKind == MethodSyntheticKind.None) {
        val (classDef, _) = getTree(ancestorInfo.encodedName)
        classDef.defs.collectFirst {
          case mDef: MethodDef if mDef.name.name == encodedName => mDef
        }.getOrElse {
          throw new AssertionError(
              s"Cannot find $encodedName in ${ancestorInfo.encodedName}")
        }
      } else {
        findInheritedMethodDef(ancestorInfo.superClass)
      }
    }

    val inheritedMDef = findInheritedMethodDef(classInfo.superClass)

    val origName = inheritedMDef.name.asInstanceOf[Ident].originalName
    val ctorIdent = Ident(encodedName, origName)
    val params = inheritedMDef.args.map(_.copy()) // for the new pos
    val currentClassType = ClassType(classInfo.encodedName)
    val superClassType = ClassType(classInfo.superClass.encodedName)
    MethodDef(static = false, ctorIdent,
        params, NoType,
        ApplyStatically(This()(currentClassType),
            superClassType, ctorIdent, params.map(_.ref))(NoType))(
        OptimizerHints.empty,
        inheritedMDef.hash) // over-approximation
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
