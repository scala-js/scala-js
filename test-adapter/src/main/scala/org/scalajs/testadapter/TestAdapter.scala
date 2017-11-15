/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js test adapter      **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2017, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.collection.concurrent.TrieMap

import java.util.concurrent.atomic.{AtomicInteger, AtomicBoolean}

import org.scalajs.core.ir.Utils.escapeJS
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker.ModuleKind

import org.scalajs.jsenv._
import org.scalajs.testcommon._

import sbt.testing.Framework

final class TestAdapter(jsEnv: ComJSEnv, jsFiles: Seq[VirtualJSFile],
    config: TestAdapter.Config) {

  import TestAdapter.ManagedRunner

  /** Map of ThreadId -> ManagedRunner */
  private[this] val runners = TrieMap.empty[Long, ManagedRunner]

  private[this] val closing = new AtomicBoolean(false)

  private[this] val nextRunID = new AtomicInteger(0)
  private[this] val runs = TrieMap.empty[RunMux.RunID, Unit]

  /** Creates an `sbt.testing.Framework` for each framework that can be found.
   *
   *  The returned Frameworks bind to this TestAdapter and are only valid until
   *  [[close]] is called.
   */
  def loadFrameworks(frameworkNames: List[List[String]]): List[Option[Framework]] = {
    val runner = getRunnerForThread()

    val frameworks = runner.com
      .call(JSEndpoints.detectFrameworks)(frameworkNames)
      .map(_.map(_.map(info => new FrameworkAdapter(info, this))))

    // If there is no testing framework loaded, nothing will reply.
    val fallback: Future[List[Option[Framework]]] =
      runner.runner.future.map(_ => frameworkNames.map(_ => None))

    Future.firstCompletedOf(List(frameworks, fallback)).await()
  }

  /** Releases all resources. All associated runs must be done. */
  def close(): Unit = {
    if (!closing.getAndSet(true)) {
      // Snapshot runs.
      val seenRuns = runs.keySet

      runners.values.foreach(_.com.close())
      runners.values.foreach(_.runner.stop())
      runners.clear()

      runs.clear()
      if (seenRuns.nonEmpty) {
        throw new IllegalStateException(
            s"close() called with incomplete runs: $seenRuns")
      }
    }
  }

  private[testadapter] def runStarting(): RunMux.RunID = {
    require(!closing.get(), "We are closing. Cannot create new run.")
    val runID = nextRunID.getAndIncrement()
    runs.put(runID, ())
    runID
  }

  /** Called by [[RunnerAdapter]] when the run is completed. */
  private[testadapter] def runDone(runID: RunMux.RunID): Unit = {
    val old = runs.remove(runID)
    require(old.nonEmpty, s"Tried to remove nonexistent run $runID")
  }

  private[testadapter] def getRunnerForThread(): ManagedRunner = {
    // Prevent runners from being started after closing started.
    // Otherwise we might leak runners.
    require(!closing.get(), "We are closing. Cannot create new runner.")

    val threadId = Thread.currentThread().getId()

    // Note that this is thread safe, since each thread can only operate on
    // the value associated to its thread id.
    runners.getOrElseUpdate(threadId, startManagedRunner(threadId))
  }

  private def startManagedRunner(threadId: Long): ManagedRunner = {
    // !!! DUPLICATE code with ScalaJSPlugin.makeExportsNamespaceExpr
    val orgExpr = config.moduleKind match {
      case ModuleKind.NoModule =>
        "typeof(org) != 'undefined' ? org : {}"

      case ModuleKind.CommonJSModule =>
        val moduleIdent = config.moduleIdentifier.getOrElse {
          throw new IllegalArgumentException(
              "The module identifier must be specified for CommonJS modules")
        }
        s"""require("${escapeJS(moduleIdent)}").org || {}"""
    }

    /* #2752: if there is no testing framework at all on the classpath,
     * the testing interface will not be there, and therefore the
     * `startBridge` function will not exist. We must therefore be
     * careful when selecting it.
     * If it is not present, we will simply exit; `loadFrameworks` is prepared
     * to deal with this case.
     */
    val code = s"""
      (function() {
        "use strict";
        var namespace = $orgExpr;
        namespace = namespace.scalajs || {};
        namespace = namespace.testinterface || {};
        namespace = namespace.internal || {};
        var bridge = namespace.startBridge || function() {};
        bridge();
      })();
    """

    val launcher = new MemVirtualJSFile("startTestBridge.js").withContent(code)
    val runner = jsEnv.comRunner(jsFiles :+ launcher)
    runner.start(config.logger, config.console)
    new ManagedRunner(threadId, runner)
  }
}

object TestAdapter {
  final class Config private (
      val logger: Logger,
      val console: JSConsole,
      val moduleKind: ModuleKind,
      val moduleIdentifier: Option[String]
  ) {
    private def this() = {
      this(
          logger = NullLogger,
          console = ConsoleJSConsole,
          moduleKind = ModuleKind.NoModule,
          moduleIdentifier = None
      )
    }

    def withLogger(logger: Logger): Config =
      copy(logger = logger)

    def withJSConsole(console: JSConsole): Config =
      copy(console = console)

    def withModuleSettings(moduleKind: ModuleKind,
        moduleIdentifier: Option[String]): Config = {
      require((moduleKind == ModuleKind.NoModule) != moduleIdentifier.nonEmpty,
          "Need a module identifier with modules")
      copy(moduleKind = moduleKind, moduleIdentifier = moduleIdentifier)
    }

    private def copy(
        logger: Logger = logger,
        console: JSConsole = console,
        moduleKind: ModuleKind = moduleKind,
        moduleIdentifier: Option[String] = moduleIdentifier
    ): Config = {
      new Config(logger, console, moduleKind, moduleIdentifier)
    }
  }

  object Config {
    def apply(): Config = new Config()
  }

  private[testadapter] final class ManagedRunner(
      val id: Long, val runner: ComJSRunner) {
    val com = new ComJSEnvRPC(runner)
    val mux = new RunMuxRPC(com)
  }
}
