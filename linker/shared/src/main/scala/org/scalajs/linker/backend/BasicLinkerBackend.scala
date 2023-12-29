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

package org.scalajs.linker.backend

import scala.concurrent._

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.{IRFile, OutputDirectory, Report}
import org.scalajs.linker.interface.unstable.OutputPatternsImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.emitter.Emitter
import org.scalajs.linker.backend.javascript.{ByteArrayWriter, Printers, SourceMapWriter, Trees => js}

/** The basic backend for the Scala.js linker.
 *
 *  Simply emits the JavaScript without applying any further optimizations.
 */
final class BasicLinkerBackend(config: LinkerBackendImpl.Config)
    extends LinkerBackendImpl(config) {

  import BasicLinkerBackend._

  private[this] val emitter = {
    val emitterConfig = Emitter.Config(config.commonConfig.coreSpec)
      .withJSHeader(config.jsHeader)
      .withInternalModulePattern(m => OutputPatternsImpl.moduleName(config.outputPatterns, m.id))

    new Emitter(emitterConfig)
  }

  val symbolRequirements: SymbolRequirement = emitter.symbolRequirements

  private var isFirstRun: Boolean = true

  private val printedModuleSetCache = new PrintedModuleSetCache(config.sourceMap)

  override def injectedIRFiles: Seq[IRFile] = emitter.injectedIRFiles

  /** Emit the given [[standard.ModuleSet ModuleSet]] to the target output.
   *
   *  @param moduleSet [[standard.ModuleSet ModuleSet]] to emit
   *  @param output Directory to write to
   */
  def emit(moduleSet: ModuleSet, output: OutputDirectory, logger: Logger)(
      implicit ec: ExecutionContext): Future[Report] = {
    verifyModuleSet(moduleSet)

    val emitterResult = logger.time("Emitter") {
      emitter.emit(moduleSet, logger)
    }

    val skipContentCheck = !isFirstRun
    isFirstRun = false

    printedModuleSetCache.startRun(moduleSet)
    val allChanged =
      printedModuleSetCache.updateGlobal(emitterResult.header, emitterResult.footer)

    val writer = new OutputWriter(output, config, skipContentCheck) {
      protected def writeModuleWithoutSourceMap(moduleID: ModuleID, force: Boolean): Option[ByteBuffer] = {
        val cache = printedModuleSetCache.getModuleCache(moduleID)
        val changed = cache.update(emitterResult.body(moduleID))

        if (force || changed || allChanged) {
          printedModuleSetCache.incRewrittenModules()

          val jsFileWriter = new ByteArrayWriter(sizeHintFor(cache.getPreviousFinalJSFileSize()))

          jsFileWriter.write(printedModuleSetCache.headerBytes)
          jsFileWriter.writeASCIIString("'use strict';\n")

          for (printedTree <- cache.printedTrees)
            jsFileWriter.write(printedTree.jsCode)

          jsFileWriter.write(printedModuleSetCache.footerBytes)

          cache.recordFinalSizes(jsFileWriter.currentSize, 0)
          Some(jsFileWriter.toByteBuffer())
        } else {
          None
        }
      }

      protected def writeModuleWithSourceMap(moduleID: ModuleID, force: Boolean): Option[(ByteBuffer, ByteBuffer)] = {
        val cache = printedModuleSetCache.getModuleCache(moduleID)
        val changed = cache.update(emitterResult.body(moduleID))

        if (force || changed || allChanged) {
          printedModuleSetCache.incRewrittenModules()

          val jsFileWriter = new ByteArrayWriter(sizeHintFor(cache.getPreviousFinalJSFileSize()))
          val sourceMapWriter = new ByteArrayWriter(sizeHintFor(cache.getPreviousFinalSourceMapSize()))

          val jsFileURI = OutputPatternsImpl.jsFileURI(config.outputPatterns, moduleID.id)
          val sourceMapURI = OutputPatternsImpl.sourceMapURI(config.outputPatterns, moduleID.id)

          val smWriter = new SourceMapWriter(sourceMapWriter, jsFileURI,
              config.relativizeSourceMapBase)

          jsFileWriter.write(printedModuleSetCache.headerBytes)
          for (_ <- 0 until printedModuleSetCache.headerNewLineCount)
            smWriter.nextLine()

          jsFileWriter.writeASCIIString("'use strict';\n")
          smWriter.nextLine()

          for (printedTree <- cache.printedTrees) {
            jsFileWriter.write(printedTree.jsCode)
            smWriter.insertFragment(printedTree.sourceMapFragment)
          }

          jsFileWriter.write(printedModuleSetCache.footerBytes)
          jsFileWriter.write(("//# sourceMappingURL=" + sourceMapURI + "\n").getBytes(StandardCharsets.UTF_8))

          smWriter.complete()

          cache.recordFinalSizes(jsFileWriter.currentSize, sourceMapWriter.currentSize)
          Some((jsFileWriter.toByteBuffer(), sourceMapWriter.toByteBuffer()))
        } else {
          None
        }
      }

      private def sizeHintFor(previousSize: Int): Int =
        previousSize + (previousSize / 10)
    }

    logger.timeFuture("BasicBackend: Write result") {
      writer.write(moduleSet)
    }.andThen { case _ =>
      printedModuleSetCache.cleanAfterRun()
      printedModuleSetCache.logStats(logger)
    }
  }
}

private object BasicLinkerBackend {
  private final class PrintedModuleSetCache(withSourceMaps: Boolean) {
    private var lastHeader: String = null
    private var lastFooter: String = null

    private var _headerBytesCache: Array[Byte] = null
    private var _footerBytesCache: Array[Byte] = null
    private var _headerNewLineCountCache: Int = 0

    private val modules = new java.util.concurrent.ConcurrentHashMap[ModuleID, PrintedModuleCache]

    private var totalModules = 0
    private val rewrittenModules = new java.util.concurrent.atomic.AtomicInteger(0)

    private var totalTopLevelTrees = 0
    private var recomputedTopLevelTrees = 0

    def startRun(moduleSet: ModuleSet): Unit = {
      totalModules = moduleSet.modules.size
      rewrittenModules.set(0)

      totalTopLevelTrees = 0
      recomputedTopLevelTrees = 0
    }

    def updateGlobal(header: String, footer: String): Boolean = {
      if (header == lastHeader && footer == lastFooter) {
        false
      } else {
        _headerBytesCache = header.getBytes(StandardCharsets.UTF_8)
        _footerBytesCache = footer.getBytes(StandardCharsets.UTF_8)
        _headerNewLineCountCache = _headerBytesCache.count(_ == '\n')
        lastHeader = header
        lastFooter = footer
        true
      }
    }

    def headerBytes: Array[Byte] = _headerBytesCache
    def footerBytes: Array[Byte] = _footerBytesCache
    def headerNewLineCount: Int = _headerNewLineCountCache

    def getModuleCache(moduleID: ModuleID): PrintedModuleCache = {
      val result = modules.computeIfAbsent(moduleID, { _ =>
        if (withSourceMaps) new PrintedModuleCacheWithSourceMaps
        else new PrintedModuleCache
      })

      result.startRun()
      result
    }

    def incRewrittenModules(): Unit =
      rewrittenModules.incrementAndGet()

    def cleanAfterRun(): Unit = {
      val iter = modules.entrySet().iterator()
      while (iter.hasNext()) {
        val moduleCache = iter.next().getValue()
        if (moduleCache.cleanAfterRun()) {
          totalTopLevelTrees += moduleCache.getTotalTopLevelTrees
          recomputedTopLevelTrees += moduleCache.getRecomputedTopLevelTrees
        } else {
          iter.remove()
        }
      }
    }

    def logStats(logger: Logger): Unit = {
      /* These messages are extracted in BasicLinkerBackendTest to assert that
       * we do not invalidate anything in a no-op second run.
       */
      logger.debug(
          s"BasicBackend: total top-level trees: $totalTopLevelTrees; re-computed: $recomputedTopLevelTrees")
      logger.debug(
          s"BasicBackend: total modules: $totalModules; re-written: ${rewrittenModules.get()}")
    }
  }

  private final class PrintedTree(val jsCode: Array[Byte], val sourceMapFragment: SourceMapWriter.Fragment) {
    var cachedUsed: Boolean = false
  }

  private sealed class PrintedModuleCache {
    private var cacheUsed = false
    private var changed = false
    private var lastJSTrees: List[js.Tree] = Nil
    private var printedTreesCache: List[PrintedTree] = Nil
    private val cache = new java.util.IdentityHashMap[js.Tree, PrintedTree]

    private var previousFinalJSFileSize: Int = 0
    private var previousFinalSourceMapSize: Int = 0

    private var recomputedTopLevelTrees = 0

    def startRun(): Unit = {
      cacheUsed = true
      recomputedTopLevelTrees = 0
    }

    def getPreviousFinalJSFileSize(): Int = previousFinalJSFileSize

    def getPreviousFinalSourceMapSize(): Int = previousFinalSourceMapSize

    def recordFinalSizes(finalJSFileSize: Int, finalSourceMapSize: Int): Unit = {
      previousFinalJSFileSize = finalJSFileSize
      previousFinalSourceMapSize = finalSourceMapSize
    }

    def update(newJSTrees: List[js.Tree]): Boolean = {
      val changed = !newJSTrees.corresponds(lastJSTrees)(_ eq _)
      this.changed = changed
      if (changed) {
        printedTreesCache = newJSTrees.map(getOrComputePrintedTree(_))
        lastJSTrees = newJSTrees
      }
      changed
    }

    private def getOrComputePrintedTree(tree: js.Tree): PrintedTree = {
      val result = cache.computeIfAbsent(tree, { (tree: js.Tree) =>
        recomputedTopLevelTrees += 1
        computePrintedTree(tree)
      })

      result.cachedUsed = true
      result
    }

    protected def computePrintedTree(tree: js.Tree): PrintedTree = {
      val jsCodeWriter = new ByteArrayWriter()
      val printer = new Printers.JSTreePrinter(jsCodeWriter)

      printer.printStat(tree)

      new PrintedTree(jsCodeWriter.toByteArray(), SourceMapWriter.Fragment.Empty)
    }

    def printedTrees: List[PrintedTree] = printedTreesCache

    def cleanAfterRun(): Boolean = {
      if (cacheUsed) {
        cacheUsed = false

        if (changed) {
          val iter = cache.entrySet().iterator()
          while (iter.hasNext()) {
            val printedTree = iter.next().getValue()
            if (printedTree.cachedUsed)
              printedTree.cachedUsed = false
            else
              iter.remove()
          }
        }

        true
      } else {
        false
      }
    }

    def getTotalTopLevelTrees: Int = lastJSTrees.size
    def getRecomputedTopLevelTrees: Int = recomputedTopLevelTrees
  }

  private final class PrintedModuleCacheWithSourceMaps extends PrintedModuleCache {
    override protected def computePrintedTree(tree: js.Tree): PrintedTree = {
      val jsCodeWriter = new ByteArrayWriter()
      val smFragmentBuilder = new SourceMapWriter.FragmentBuilder()
      val printer = new Printers.JSTreePrinterWithSourceMap(jsCodeWriter, smFragmentBuilder)

      printer.printStat(tree)
      smFragmentBuilder.complete()

      new PrintedTree(jsCodeWriter.toByteArray(), smFragmentBuilder.result())
    }
  }
}
