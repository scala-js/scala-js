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

import java.nio.charset.StandardCharsets

import org.scalajs.logging.Logger

import org.scalajs.linker.interface.{IRFile, OutputDirectory, Report}
import org.scalajs.linker.interface.unstable.OutputPatternsImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

import org.scalajs.linker.backend.emitter.Emitter
import org.scalajs.linker.backend.javascript.{Printers, SourceMapWriter, Trees => js}

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

    val writer = new OutputWriter(output, config) {
      protected def writeModule(moduleID: ModuleID, jsFileWriter: ByteArrayWriter): Unit = {
        val printedModuleCache = printedModuleSetCache.getModuleCache(moduleID)

        jsFileWriter.sizeHint(sizeHintFor(printedModuleCache.getPreviousFinalJSFileSize()))

        jsFileWriter.write(emitterResult.header.getBytes(StandardCharsets.UTF_8))
        jsFileWriter.writeASCIIString("'use strict';\n")

        for (topLevelTree <- emitterResult.body(moduleID)) {
          val printedTree = printedModuleCache.getPrintedTree(topLevelTree)
          jsFileWriter.write(printedTree.jsCode)
        }

        jsFileWriter.write(emitterResult.footer.getBytes(StandardCharsets.UTF_8))

        printedModuleCache.recordFinalSizes(jsFileWriter.currentSize, 0)
      }

      protected def writeModule(moduleID: ModuleID, jsFileWriter: ByteArrayWriter,
          sourceMapWriter: ByteArrayWriter): Unit = {
        val printedModuleCache = printedModuleSetCache.getModuleCache(moduleID)

        jsFileWriter.sizeHint(sizeHintFor(printedModuleCache.getPreviousFinalJSFileSize()))
        sourceMapWriter.sizeHint(sizeHintFor(printedModuleCache.getPreviousFinalSourceMapSize()))

        val jsFileURI = OutputPatternsImpl.jsFileURI(config.outputPatterns, moduleID.id)
        val sourceMapURI = OutputPatternsImpl.sourceMapURI(config.outputPatterns, moduleID.id)

        val smWriter = new SourceMapWriter(sourceMapWriter, jsFileURI,
            config.relativizeSourceMapBase)

        jsFileWriter.write(emitterResult.header.getBytes(StandardCharsets.UTF_8))
        for (_ <- 0 until emitterResult.header.count(_ == '\n'))
          smWriter.nextLine()

        jsFileWriter.writeASCIIString("'use strict';\n")
        smWriter.nextLine()

        for (topLevelTree <- emitterResult.body(moduleID)) {
          val printedTree = printedModuleCache.getPrintedTree(topLevelTree)
          jsFileWriter.write(printedTree.jsCode)
          smWriter.insertFragment(printedTree.sourceMapFragment)
        }

        jsFileWriter.write(emitterResult.footer.getBytes(StandardCharsets.UTF_8))
        jsFileWriter.write(("//# sourceMappingURL=" + sourceMapURI + "\n").getBytes(StandardCharsets.UTF_8))

        smWriter.complete()

        printedModuleCache.recordFinalSizes(jsFileWriter.currentSize, sourceMapWriter.currentSize)
      }

      private def sizeHintFor(previousSize: Int): Int =
        previousSize + (previousSize / 10)
    }

    printedModuleSetCache.startRun()

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
    private val modules = new java.util.concurrent.ConcurrentHashMap[ModuleID, PrintedModuleCache]

    private var totalTopLevelTrees = 0
    private var recomputedTopLevelTrees = 0

    def startRun(): Unit = {
      totalTopLevelTrees = 0
      recomputedTopLevelTrees = 0
    }

    def getModuleCache(moduleID: ModuleID): PrintedModuleCache = {
      val result = modules.computeIfAbsent(moduleID, { _ =>
        if (withSourceMaps) new PrintedModuleCacheWithSourceMaps
        else new PrintedModuleCache
      })

      result.startRun()
      result
    }

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
      /* This message is extracted in BasicLinkerBackendTest to assert that we
       * do not invalidate anything in a no-op second run.
       */
      logger.debug(
          s"BasicBackend: total top-level trees: $totalTopLevelTrees; re-computed: $recomputedTopLevelTrees")
    }
  }

  private final class PrintedTree(val jsCode: Array[Byte], val sourceMapFragment: SourceMapWriter.Fragment) {
    var cachedUsed: Boolean = false
  }

  private sealed class PrintedModuleCache {
    private var cacheUsed = false
    private val cache = new java.util.IdentityHashMap[js.Tree, PrintedTree]

    private var previousFinalJSFileSize: Int = 0
    private var previousFinalSourceMapSize: Int = 0

    private var totalTopLevelTrees = 0
    private var recomputedTopLevelTrees = 0

    def startRun(): Unit = {
      cacheUsed = true
      totalTopLevelTrees = 0
      recomputedTopLevelTrees = 0
    }

    def getPreviousFinalJSFileSize(): Int = previousFinalJSFileSize

    def getPreviousFinalSourceMapSize(): Int = previousFinalSourceMapSize

    def recordFinalSizes(finalJSFileSize: Int, finalSourceMapSize: Int): Unit = {
      previousFinalJSFileSize = finalJSFileSize
      previousFinalSourceMapSize = finalSourceMapSize
    }

    def getPrintedTree(tree: js.Tree): PrintedTree = {
      totalTopLevelTrees += 1

      val result = cache.computeIfAbsent(tree, { (tree: js.Tree) =>
        recomputedTopLevelTrees += 1
        computePrintedTree(tree)
      })

      result.cachedUsed = true
      result
    }

    protected def computePrintedTree(tree: js.Tree): PrintedTree = {
      val jsCodeWriter = new java.io.StringWriter()
      val printer = new Printers.JSTreePrinter(jsCodeWriter)

      printer.printTopLevelTree(tree)

      val jsCode = jsCodeWriter.toString().getBytes(StandardCharsets.UTF_8)
      new PrintedTree(jsCode, SourceMapWriter.Fragment.Empty)
    }

    def cleanAfterRun(): Boolean = {
      if (cacheUsed) {
        cacheUsed = false

        val iter = cache.entrySet().iterator()
        while (iter.hasNext()) {
          val printedTree = iter.next().getValue()
          if (printedTree.cachedUsed)
            printedTree.cachedUsed = false
          else
            iter.remove()
        }

        true
      } else {
        false
      }
    }

    def getTotalTopLevelTrees: Int = totalTopLevelTrees
    def getRecomputedTopLevelTrees: Int = recomputedTopLevelTrees
  }

  private final class PrintedModuleCacheWithSourceMaps extends PrintedModuleCache {
    override protected def computePrintedTree(tree: js.Tree): PrintedTree = {
      val jsCodeWriter = new java.io.StringWriter()
      val smFragmentBuilder = new SourceMapWriter.FragmentBuilder()
      val printer = new Printers.JSTreePrinterWithSourceMap(jsCodeWriter, smFragmentBuilder)

      printer.printTopLevelTree(tree)
      smFragmentBuilder.complete()

      val jsCode = jsCodeWriter.toString().getBytes(StandardCharsets.UTF_8)
      val smFragment = smFragmentBuilder.result()
      new PrintedTree(jsCode, smFragment)
    }
  }
}
