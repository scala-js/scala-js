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

import java.util.concurrent.atomic.AtomicInteger

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

  private[this] var totalModules = 0
  private[this] val rewrittenModules = new AtomicInteger(0)

  private[this] val postTransformer: Emitter.PostTransformer = {
    if (config.minify) Emitter.PostTransformer.Identity
    else if (config.sourceMap) PostTransformerWithSourceMap
    else PostTransformerWithoutSourceMap
  }

  private[this] val emitter: Emitter = {
    val emitterConfig = Emitter.Config(config.commonConfig.coreSpec)
      .withJSHeader(config.jsHeader)
      .withInternalModulePattern(m => OutputPatternsImpl.moduleName(config.outputPatterns, m.id))
      .withMinify(config.minify)

    new Emitter(emitterConfig, postTransformer)
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

    // Reset stats.

    totalModules = moduleSet.modules.size
    rewrittenModules.set(0)

    val emitterResult = logger.time("Emitter") {
      emitter.emit(moduleSet, logger)
    }

    val skipContentCheck = !isFirstRun
    isFirstRun = false

    val allChanged0 =
      printedModuleSetCache.updateGlobal(emitterResult.header, emitterResult.footer)
    val allChanged = allChanged0 || config.minify

    val writer = new OutputWriter(output, config, skipContentCheck) {
      protected def moduleChanged(moduleID: ModuleID): Boolean =
        allChanged || emitterResult.body(moduleID)._2

      protected def writeModuleWithoutSourceMap(moduleID: ModuleID, prevFile: Option[ByteBuffer]): ByteBuffer = {
        val cache = printedModuleSetCache.getModuleCache(moduleID)
        val (trees, _) = emitterResult.body(moduleID)

        rewrittenModules.incrementAndGet()

        val jsFileWriter = new ByteArrayWriter(sizeHintFor(cache.getPreviousFinalJSFileSize()))

        jsFileWriter.write(printedModuleSetCache.headerBytes)
        jsFileWriter.writeASCIIString("'use strict';\n")

        val printer = new Printers.JSTreePrinter(prevFile, jsFileWriter)
        for (tree <- trees)
          printer.printStat(tree)

        jsFileWriter.write(printedModuleSetCache.footerBytes)

        cache.recordFinalSizes(jsFileWriter.currentSize, 0)
        jsFileWriter.toByteBuffer()
      }

      protected def writeModuleWithSourceMap(moduleID: ModuleID, prevFile: Option[ByteBuffer]): (ByteBuffer, ByteBuffer) = {
        val cache = printedModuleSetCache.getModuleCache(moduleID)
        val (trees, _) = emitterResult.body(moduleID)

        rewrittenModules.incrementAndGet()

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

        val printer = new Printers.JSTreePrinterWithSourceMap(prevFile, jsFileWriter, smWriter, initIndent = 0)
        for (tree <- trees)
          printer.printStat(tree)

        jsFileWriter.write(printedModuleSetCache.footerBytes)
        jsFileWriter.write(("//# sourceMappingURL=" + sourceMapURI + "\n").getBytes(StandardCharsets.UTF_8))

        smWriter.complete()

        cache.recordFinalSizes(jsFileWriter.currentSize, sourceMapWriter.currentSize)
        (jsFileWriter.toByteBuffer(), sourceMapWriter.toByteBuffer())
      }

      private def sizeHintFor(previousSize: Int): Int =
        previousSize + (previousSize / 10)
    }

    logger.timeFuture("BasicBackend: Write result") {
      writer.write(moduleSet)
    }.andThen { case _ =>
      printedModuleSetCache.cleanAfterRun()
      logStats(logger)
    }
  }

  private def logStats(logger: Logger): Unit = {
    // Message extracted in BasicLinkerBackendTest
    logger.debug(
        s"BasicBackend: total modules: $totalModules; re-written: ${rewrittenModules.get()}")
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
      val result = modules.computeIfAbsent(moduleID, _ => new PrintedModuleCache)
      result.startRun()
      result
    }

    def cleanAfterRun(): Unit = {
      val iter = modules.entrySet().iterator()
      while (iter.hasNext()) {
        val moduleCache = iter.next().getValue()
        if (!moduleCache.cleanAfterRun()) {
          iter.remove()
        }
      }
    }
  }

  private sealed class PrintedModuleCache {
    private var cacheUsed = false

    private var previousFinalJSFileSize: Int = 0
    private var previousFinalSourceMapSize: Int = 0

    def startRun(): Unit = {
      cacheUsed = true
    }

    def getPreviousFinalJSFileSize(): Int = previousFinalJSFileSize

    def getPreviousFinalSourceMapSize(): Int = previousFinalSourceMapSize

    def recordFinalSizes(finalJSFileSize: Int, finalSourceMapSize: Int): Unit = {
      previousFinalJSFileSize = finalJSFileSize
      previousFinalSourceMapSize = finalSourceMapSize
    }

    def cleanAfterRun(): Boolean = {
      val wasUsed = cacheUsed
      cacheUsed = false
      wasUsed
    }
  }

  private object PostTransformerWithoutSourceMap extends Emitter.PostTransformer {
    def transformStats(trees: List[js.Tree], indent: Int): List[js.PrintedTree] = {
      if (trees.isEmpty) {
        Nil // Fast path
      } else {
        val jsCodeWriter = new ByteArrayWriter()
        val printer = new Printers.JSTreePrinter(None, jsCodeWriter, indent)

        trees.foreach(printer.printStat(_))

        js.PrintedTree(jsCodeWriter.toByteArray(), SourceMapWriter.Fragment.Empty) :: Nil
      }
    }
  }

  private object PostTransformerWithSourceMap extends Emitter.PostTransformer {
    def transformStats(trees: List[js.Tree], indent: Int): List[js.PrintedTree] = {
      if (trees.isEmpty) {
        Nil // Fast path
      } else {
        val jsCodeWriter = new ByteArrayWriter()
        val smFragmentBuilder = new SourceMapWriter.FragmentBuilder()
        val printer = new Printers.JSTreePrinterWithSourceMap(None, jsCodeWriter, smFragmentBuilder, indent)

        trees.foreach(printer.printStat(_))
        smFragmentBuilder.complete()

        js.PrintedTree(jsCodeWriter.toByteArray(), smFragmentBuilder.result()) :: Nil
      }
    }
  }
}
