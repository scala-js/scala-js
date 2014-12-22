/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.optimizer

import scala.annotation.{switch, tailrec}

import scala.collection.mutable
import scala.collection.immutable.{Seq, Traversable}

import java.net.URI

import org.scalajs.core.ir
import ir.Infos
import ir.ClassKind

import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.sourcemap._
import org.scalajs.core.tools.corelib._

import org.scalajs.core.tools.sem.Semantics

import org.scalajs.core.tools.javascript
import javascript.{Trees => js}

/** Scala.js optimizer: does type-aware global dce. */
class ScalaJSOptimizer(
    semantics: Semantics,
    optimizerFactory: (Semantics) => GenIncOptimizer) {
  import ScalaJSOptimizer._

  private val classEmitter = new javascript.ScalaJSClassEmitter(semantics)

  private[this] var persistentState: PersistentState = new PersistentState
  private[this] var optimizer: GenIncOptimizer = optimizerFactory(semantics)

  def this(semantics: Semantics) = this(semantics, new IncOptimizer(_))

  /** Applies Scala.js-specific optimizations to a CompleteIRClasspath.
   *  See [[ScalaJSOptimizer.Config]] for details about the configuration
   *  for the output of this method.
   *  Returns a [[CompleteCIClasspath]] containing the result of the
   *  optimizations.
   *
   *  analyzes, dead code eliminates and concatenates IR content
   *  - Maintains/establishes order
   *  - No IR in result
   *  - CoreJSLibs in result (since they are implicitly in the CompleteIRCP)
   */
  def optimizeCP(classpath: IRClasspath, cfg: Config,
      logger: Logger): LinkedClasspath = {

    CacheUtils.cached(classpath.version, cfg.output, cfg.cache) {
      logger.info(s"Fast optimizing ${cfg.output.path}")
      optimizeIR(classpath.scalaJSIR, cfg, logger)
    }

    new LinkedClasspath(classpath.jsLibs, cfg.output,
        classpath.requiresDOM, classpath.version)
  }

  def optimizeIR(irFiles: Traversable[VirtualScalaJSIRFile],
      cfg: Config, logger: Logger): Unit = {

    val builder = {
      import cfg._
      if (wantSourceMap)
        new JSFileBuilderWithSourceMap(output.name,
            output.contentWriter,
            output.sourceMapWriter,
            relativizeSourceMapBase)
      else
        new JSFileBuilder(output.name, output.contentWriter)
    }

    builder.addLine("'use strict';")
    CoreJSLibs.libs(semantics).foreach(builder.addFile _)

    optimizeIR(irFiles, cfg, builder, logger)

    builder.complete()
    builder.closeWriters()
  }

  def optimizeIR(irFiles: Traversable[VirtualScalaJSIRFile],
      cfg: OptimizerConfig, builder: JSTreeBuilder, logger: Logger): Unit = {

    /* Handle tree equivalence: If we handled source maps so far, positions are
       still up-to-date. Otherwise we need to flush the state if proper
       positions are requested now.
     */
    if (cfg.wantSourceMap && !persistentState.wasWithSourceMap)
      clean()

    persistentState.wasWithSourceMap = cfg.wantSourceMap

    persistentState.startRun()
    try {
      val allData =
        GenIncOptimizer.logTime(logger, "Read info") {
          readAllData(irFiles, logger)
        }
      val (useOptimizer, refinedAnalysis) = GenIncOptimizer.logTime(
          logger, "Optimizations part") {
        val analysis =
          GenIncOptimizer.logTime(logger, "Compute reachability") {
            val analyzer = new Analyzer(semantics,
                reachOptimizerSymbols = !cfg.disableOptimizer)
            analyzer.computeReachability(allData)
          }

        val filteredErrors = filterErrors(analysis.errors, cfg.noWarnMissing)
        filteredErrors.foreach(Analysis.logError(_, logger, Level.Warn))

        if (cfg.checkIR) {
          GenIncOptimizer.logTime(logger, "Check IR") {
            if (analysis.allAvailable)
              checkIR(analysis, logger)
            else if (cfg.noWarnMissing.isEmpty)
              sys.error("Could not check IR because there where linking errors.")
          }
        }
        def getClassTreeIfChanged(encodedName: String,
            lastVersion: Option[String]): Option[(ir.Trees.ClassDef, Option[String])] = {
          val persistentFile = persistentState.encodedNameToPersistentFile(encodedName)
          persistentFile.treeIfChanged(lastVersion)
        }

        val useOptimizer = analysis.allAvailable && !cfg.disableOptimizer

        if (cfg.batchMode)
          optimizer = optimizerFactory(semantics)

        val refinedAnalysis = if (useOptimizer) {
          GenIncOptimizer.logTime(logger, "Inliner") {
            optimizer.update(analysis, getClassTreeIfChanged,
                cfg.wantSourceMap, logger)
          }
          GenIncOptimizer.logTime(logger, "Refined reachability analysis") {
            val refinedData = computeRefinedData(allData, optimizer)
            val refinedAnalyzer = new Analyzer(semantics,
                reachOptimizerSymbols = false)
            refinedAnalyzer.computeReachability(refinedData)
          }
        } else {
          if (cfg.noWarnMissing.isEmpty && !cfg.disableOptimizer)
            logger.warn("Not running the inliner because there where linking errors.")
          analysis
        }
        (useOptimizer, refinedAnalysis)
      }
      GenIncOptimizer.logTime(logger, "Write DCE'ed output") {
        buildDCEedOutput(builder, refinedAnalysis, useOptimizer)
      }
    } finally {
      persistentState.endRun(cfg.unCache)
      logger.debug(
          s"Inc. opt stats: reused: ${persistentState.statsReused} -- "+
          s"invalidated: ${persistentState.statsInvalidated} -- "+
          s"trees read: ${persistentState.statsTreesRead}")
    }
  }

  /** Resets all persistent state of this optimizer */
  def clean(): Unit = {
    persistentState = new PersistentState
    optimizer = optimizerFactory(semantics)
  }

  private def readAllData(ir: Traversable[VirtualScalaJSIRFile],
      logger: Logger): scala.collection.Seq[Infos.ClassInfo] = {
    ir.map(persistentState.getPersistentIRFile(_).info).toSeq
  }

  private def checkIR(analysis: Analysis, logger: Logger): Unit = {
    val allClassDefs = for {
      classInfo <- analysis.classInfos.values
      persistentIRFile <- persistentState.encodedNameToPersistentFile.get(
          classInfo.encodedName)
    } yield persistentIRFile.tree
    val checker = new IRChecker(analysis, allClassDefs.toSeq, logger)
    if (!checker.check())
      sys.error(s"There were ${checker.errorCount} IR checking errors.")
  }

  private def computeRefinedData(
      allData: scala.collection.Seq[Infos.ClassInfo],
      optimizer: GenIncOptimizer): scala.collection.Seq[Infos.ClassInfo] = {

    def refineMethodInfo(container: Option[optimizer.MethodContainer],
        staticContainer: Option[optimizer.MethodContainer],
        methodInfo: Infos.MethodInfo): Infos.MethodInfo = {

      val optPreciseInfo = for {
        ctnr <- if (methodInfo.isStatic) staticContainer else container
        methodImpl <- ctnr.methods.get(methodInfo.encodedName)
      } yield methodImpl.preciseInfo

      optPreciseInfo.getOrElse(methodInfo)
    }

    def refineMethodInfos(container: Option[optimizer.MethodContainer],
        staticContainer: Option[optimizer.MethodContainer],
        methodInfos: List[Infos.MethodInfo]): List[Infos.MethodInfo] = {
      methodInfos.map(m => refineMethodInfo(container, staticContainer, m))
    }

    def refineClassInfo(container: Option[optimizer.MethodContainer],
        staticContainer: Option[optimizer.MethodContainer],
        info: Infos.ClassInfo): Infos.ClassInfo = {
      val refinedMethods = refineMethodInfos(
          container, staticContainer, info.methods)
      Infos.ClassInfo(info.encodedName, info.isExported,
          info.kind, info.superClass, info.parents, refinedMethods)
    }

    for {
      info <- allData
    } yield {
      info.kind match {
        case ClassKind.Class | ClassKind.ModuleClass =>
          val container = optimizer.getClass(info.encodedName)
          val staticContainer = optimizer.getStaticsNamespace(info.encodedName)
          if (container.isEmpty && staticContainer.isEmpty) info
          else refineClassInfo(container, staticContainer, info)

        case _ =>
          info
      }
    }
  }

  private def buildDCEedOutput(builder: JSTreeBuilder,
      analysis: Analysis, useInliner: Boolean): Unit = {

    def compareClassInfo(lhs: Analysis.ClassInfo, rhs: Analysis.ClassInfo) = {
      if (lhs.ancestorCount != rhs.ancestorCount) lhs.ancestorCount < rhs.ancestorCount
      else lhs.encodedName.compareTo(rhs.encodedName) < 0
    }

    def addPersistentFile(classInfo: Analysis.ClassInfo,
        persistentFile: PersistentIRFile) = {
      import ir.Trees._

      val d = persistentFile.desugared
      lazy val classDef = {
        persistentState.statsTreesRead += 1
        persistentFile.tree
      }

      def addTree(tree: js.Tree): Unit =
        builder.addJSTree(tree)

      def addReachableMethods(statics: Boolean): Unit = {
        /* This is a bit convoluted because we have to:
         * * avoid to use classDef at all if we already know all the needed methods
         * * if any new method is needed, better to go through the defs once
         */
        val (methodInfos, methodNamesCache, methodsCache) = {
          if (statics)
            (classInfo.staticMethodInfos, d.staticMethodNames, d.staticMethods)
          else
            (classInfo.methodInfos, d.methodNames, d.methods)
        }
        val methodNames = methodNamesCache.getOrElseUpdate(
            classDef.defs collect {
              case MethodDef(`statics`, Ident(encodedName, _), _, _, _) =>
                encodedName
            })
        val reachableMethods = methodNames.filter(
            name => methodInfos(name).isReachable)
        if (reachableMethods.forall(methodsCache.contains(_))) {
          for (encodedName <- reachableMethods) {
            addTree(methodsCache(encodedName))
          }
        } else {
          classDef.defs.foreach {
            case m: MethodDef if m.name.isInstanceOf[Ident] &&
                m.body != EmptyTree =>
              if (methodInfos(m.name.name).isReachable) {
                addTree(methodsCache.getOrElseUpdate(m.name.name,
                    classEmitter.genMethod(classInfo.encodedName, m)))
              }
            case _ =>
          }
        }
      }

      def ancestorNames = classInfo.ancestors.map(_.encodedName).toList

      // Static members
      if (useInliner) {
        for {
          staticsNS <- optimizer.getStaticsNamespace(classInfo.encodedName)
          method <- staticsNS.methods.values
          if classInfo.staticMethodInfos(method.encodedName).isReachable
        } addTree(method.desugaredDef)
      } else {
        addReachableMethods(statics = true)
      }

      if (classInfo.kind == ClassKind.Interface ||
          classInfo.kind == ClassKind.RawJSType ||
          classInfo.kind == ClassKind.HijackedClass) {
        // there is only the data anyway
        if (classInfo.isDataAccessed) {
          addTree(d.wholeClass.getOrElseUpdate(
              classEmitter.genClassDef(classDef, ancestorNames)))
        }
      } else {
        if (classInfo.isAnySubclassInstantiated) {
          addTree(d.constructor.getOrElseUpdate(
              classEmitter.genConstructor(classDef)))
          if (useInliner) {
            for {
              method <- optimizer.findClass(classInfo.encodedName).methods.values
              if (classInfo.methodInfos(method.encodedName).isReachable)
            } {
              addTree(method.desugaredDef)
            }
          } else {
            addReachableMethods(statics = false)
          }
          addTree(d.exportedMembers.getOrElseUpdate(js.Block {
            classDef.defs collect {
              case m: MethodDef if m.name.isInstanceOf[StringLiteral] =>
                classEmitter.genMethod(classInfo.encodedName, m)
              case p: PropertyDef =>
                classEmitter.genProperty(classInfo.encodedName, p)
            }
          }(classDef.pos)))
        }
        if (classInfo.isDataAccessed) {
          addTree(d.typeData.getOrElseUpdate(js.Block(
            classEmitter.genInstanceTests(classDef),
            classEmitter.genArrayInstanceTests(classDef),
            classEmitter.genTypeData(classDef, ancestorNames)
          )(classDef.pos)))
        }
        if (classInfo.isAnySubclassInstantiated)
          addTree(d.setTypeData.getOrElseUpdate(
              classEmitter.genSetTypeData(classDef)))
        if (classInfo.isModuleAccessed)
          addTree(d.moduleAccessor.getOrElseUpdate(
              classEmitter.genModuleAccessor(classDef)))
        addTree(d.classExports.getOrElseUpdate(
            classEmitter.genClassExports(classDef)))
      }
    }


    for {
      classInfo <- analysis.classInfos.values.toSeq.sortWith(compareClassInfo)
      if classInfo.isNeededAtAll
    } {
      val optPersistentFile =
        persistentState.encodedNameToPersistentFile.get(classInfo.encodedName)

      // if we have a persistent file, this is not a dummy class
      optPersistentFile.fold {
        if (classInfo.isAnySubclassInstantiated) {
          // Subclass will emit constructor that references this dummy class.
          // Therefore, we need to emit a dummy parent.
          builder.addJSTree(
              classEmitter.genDummyParent(classInfo.encodedName))
        }
      } { pf => addPersistentFile(classInfo, pf) }
    }
  }
}

object ScalaJSOptimizer {

  sealed abstract class NoWarnMissing {
    def className: String
  }

  object NoWarnMissing {
    final case class Class(className: String) extends NoWarnMissing
    final case class Method(className: String, methodName: String)
        extends NoWarnMissing
  }

  /** Configurations relevant to the optimizer */
  trait OptimizerConfig {
    /** Ask to produce source map for the output. Is used in the incremental
     *  optimizer to decide whether a position change should trigger re-inlining
     */
    val wantSourceMap: Boolean
    /** If true, performs expensive checks of the IR for the used parts. */
    val checkIR: Boolean
    /** If true, the optimizer removes trees that have not been used in the
     *  last run from the cache. Otherwise, all trees that has been used once,
     *  are kept in memory. */
    val unCache: Boolean
    /** If true, no optimizations are performed */
    val disableOptimizer: Boolean
    /** If true, nothing is performed incrementally */
    val batchMode: Boolean
    /** Elements we won't warn even if they don't exist */
    val noWarnMissing: Seq[NoWarnMissing]
  }

  /** Configuration for the output of the Scala.js optimizer. */
  final case class Config(
      /** Writer for the output. */
      output: WritableVirtualJSFile,
      /** Cache file */
      cache: Option[WritableVirtualTextFile] = None,
      /** Ask to produce source map for the output */
      wantSourceMap: Boolean = false,
      /** Base path to relativize paths in the source map. */
      relativizeSourceMapBase: Option[URI] = None,
      /** If true, performs expensive checks of the IR for the used parts. */
      checkIR: Boolean = false,
      /** If true, the optimizer removes trees that have not been used in the
       *  last run from the cache. Otherwise, all trees that has been used once,
       *  are kept in memory. */
      unCache: Boolean = true,
      /** If true, no optimizations are performed */
      disableOptimizer: Boolean = false,
      /** If true, nothing is performed incrementally */
      batchMode: Boolean = false,
      /** Elements we won't warn even if they don't exist */
      noWarnMissing: Seq[NoWarnMissing] = Nil
  ) extends OptimizerConfig

  // Private helpers -----------------------------------------------------------

  private def filterErrors(errors: scala.collection.Seq[Analysis.Error],
      noWarnMissing: Seq[NoWarnMissing]) = {
    import NoWarnMissing._
    import Analysis._

    val classNoWarns = mutable.Set.empty[String]
    val methodNoWarnsBuf = mutable.Buffer.empty[Method]

    // Basically a type safe partition
    noWarnMissing.foreach {
      case Class(className) =>
        classNoWarns += className
      case m: Method =>
        methodNoWarnsBuf += m
    }

    val trueFct = (_: String) => true

    val methodNoWarns = for {
      (className, elems) <- methodNoWarnsBuf.groupBy(_.className)
    } yield {
      if (classNoWarns.contains(className))
        className -> trueFct
      else
        className -> elems.map(_.methodName).toSet
    }

    errors filter {
      case MissingClass(info, _) =>
        !classNoWarns.contains(info.encodedName)
      case MissingMethod(info, _) =>
        methodNoWarns.get(info.owner.encodedName).fold(true) { setf =>
          !setf(info.encodedName)
        }
      case _ =>
        true
    }
  }

  private final class PersistentState {
    val files = mutable.Map.empty[String, PersistentIRFile]
    val encodedNameToPersistentFile =
      mutable.Map.empty[String, PersistentIRFile]

    var statsReused: Int = 0
    var statsInvalidated: Int = 0
    var statsTreesRead: Int = 0

    var wasWithSourceMap: Boolean = true

    def startRun(): Unit = {
      statsReused = 0
      statsInvalidated = 0
      statsTreesRead = 0
      for (file <- files.values)
        file.startRun()
    }

    def getPersistentIRFile(irFile: VirtualScalaJSIRFile): PersistentIRFile = {
      val file = files.getOrElseUpdate(irFile.path,
          new PersistentIRFile(irFile.path))
      if (file.updateFile(irFile))
        statsReused += 1
      else
        statsInvalidated += 1
      encodedNameToPersistentFile += ((file.info.encodedName, file))
      file
    }

    def endRun(unCache: Boolean): Unit = {
      // "Garbage-collect" persisted versions of files that have disappeared
      files.retain((_, f) => f.cleanAfterRun(unCache))
      encodedNameToPersistentFile.clear()
    }
  }

  private final class PersistentIRFile(val path: String) {
    import ir.Trees._

    private[this] var existedInThisRun: Boolean = false
    private[this] var desugaredUsedInThisRun: Boolean = false

    private[this] var irFile: VirtualScalaJSIRFile = null
    private[this] var version: Option[String] = None
    private[this] var _info: Infos.ClassInfo = null
    private[this] var _tree: ClassDef = null
    private[this] var _desugared: Desugared = null

    def startRun(): Unit = {
      existedInThisRun = false
      desugaredUsedInThisRun = false
    }

    def updateFile(irFile: VirtualScalaJSIRFile): Boolean = {
      existedInThisRun = true
      this.irFile = irFile
      if (version.isDefined && version == irFile.version) {
        // yeepeeh, nothing to do
        true
      } else {
        version = irFile.version
        _info = irFile.info
        _tree = null
        _desugared = null
        false
      }
    }

    def info: Infos.ClassInfo = _info

    def desugared: Desugared = {
      desugaredUsedInThisRun = true
      if (_desugared == null)
        _desugared = new Desugared
      _desugared
    }

    def tree: ClassDef = {
      if (_tree == null)
        _tree = irFile.tree
      _tree
    }

    def treeIfChanged(lastVersion: Option[String]): Option[(ClassDef, Option[String])] = {
      if (lastVersion.isDefined && lastVersion == version) None
      else Some((tree, version))
    }

    /** Returns true if this file should be kept for the next run at all. */
    def cleanAfterRun(unCache: Boolean): Boolean = {
      irFile = null
      if (unCache && !desugaredUsedInThisRun)
        _desugared = null // free desugared if unused in this run
      existedInThisRun
    }
  }

  private final class Desugared {
    // for class kinds that are not decomposed
    val wholeClass = new OneTimeCache[js.Tree]

    val staticMethodNames = new OneTimeCache[List[String]]
    val staticMethods = mutable.Map.empty[String, js.Tree]

    val constructor = new OneTimeCache[js.Tree]
    val methodNames = new OneTimeCache[List[String]]
    val methods = mutable.Map.empty[String, js.Tree]
    val exportedMembers = new OneTimeCache[js.Tree]
    val typeData = new OneTimeCache[js.Tree]
    val setTypeData = new OneTimeCache[js.Tree]
    val moduleAccessor = new OneTimeCache[js.Tree]
    val classExports = new OneTimeCache[js.Tree]
  }

  private final class OneTimeCache[A >: Null] {
    private[this] var value: A = null
    def getOrElseUpdate(v: => A): A = {
      if (value == null)
        value = v
      value
    }
  }
}
