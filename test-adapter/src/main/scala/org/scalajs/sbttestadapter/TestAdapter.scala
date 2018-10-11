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

package org.scalajs.testadapter

import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.concurrent.TrieMap

import org.scalajs.core.ir.Utils.escapeJS
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker.ModuleKind

import org.scalajs.jsenv._
import org.scalajs.testcommon._

import sbt.testing.Framework

final class TestAdapter(jsEnv: ComJSEnv, config: TestAdapter.Config) {

  import TestAdapter.ManagedRunner

  /** Map of ThreadId -> ManagedRunner */
  private[this] val runners = TrieMap.empty[Long, ManagedRunner]

  /** State management. May only be accessed under synchronization. */
  private[this] var closed = false
  private[this] var nextRunID = 0
  private[this] var runs = Set.empty[RunMux.RunID]

  /** A custom execution context that delegates to the global one for execution,
   *  but handles failures internally.
   */
  private implicit val executionContext =
    ExecutionContext.fromExecutor(ExecutionContext.global, reportFailure)

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

    val recovered = frameworks.recoverWith {
      // If there is no testing framework loaded, nothing will reply.
      case _: RPCCore.ClosedException =>
        // We reply with no framework at all.
        runner.runner.future.map(_ => frameworkNames.map(_ => None))
    }

    recovered.await()
  }

  /** Releases all resources. All associated runs must be done. */
  def close(): Unit = synchronized {
    val runInfo =
      if (runs.isEmpty) "All runs have completed."
      else s"Incomplete runs: $runs"

    val msg = "TestAdapter.close() was called. " + runInfo

    if (runs.nonEmpty)
      config.logger.warn(msg)

    /* This is the exception callers will see if they are still pending.
     * That's why it is an IllegalStateException.
     */
    val cause = new IllegalStateException(msg)
    stopEverything(cause)
  }

  /** Called when a throwable bubbles up the execution stack.
   *
   *  We terminate everyting if this happens to make sure nothing hangs waiting
   *  on an async operation to complete.
   */
  private def reportFailure(cause: Throwable): Unit = {
    val msg = "Failure in async execution. Aborting all test runs."
    val error = new AssertionError(msg, cause)
    config.logger.error(msg)
    config.logger.trace(error)
    stopEverything(error)
  }

  private def stopEverything(cause: Throwable): Unit = synchronized {
    if (!closed) {
      closed = true
      runners.values.foreach(_.com.close(cause))
      runners.values.foreach(_.runner.stop())
      runners.clear()
    }
  }

  private[testadapter] def runStarting(): RunMux.RunID = synchronized {
    require(!closed, "We are closed. Cannot create new run.")
    val runID = nextRunID
    nextRunID += 1
    runs += runID
    runID
  }

  /** Called by [[RunnerAdapter]] when the run is completed. */
  private[testadapter] def runDone(runID: RunMux.RunID): Unit = synchronized {
    require(runs.contains(runID), s"Tried to remove nonexistent run $runID")
    runs -= runID
  }

  private[testadapter] def getRunnerForThread(): ManagedRunner = {
    val threadId = Thread.currentThread().getId()

    // Note that this is thread safe, since each thread can only operate on
    // the value associated to its thread id.
    runners.getOrElseUpdate(threadId, startManagedRunner(threadId))
  }

  private def startManagedRunner(threadId: Long): ManagedRunner = synchronized {
    // Prevent runners from being started after we are closed.
    // Otherwise we might leak runners.
    require(!closed, "We are closed. Cannot create new runner.")

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
    val runner = jsEnv.comRunner(launcher)
    val com = new ComJSEnvRPC(runner)
    val mux = new RunMuxRPC(com)

    runner.start(config.logger, config.console)

    new ManagedRunner(threadId, runner, com, mux)
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
      val id: Long,
      val runner: ComJSRunner,
      val com: RPCCore,
      val mux: RunMuxRPC
  )
}
