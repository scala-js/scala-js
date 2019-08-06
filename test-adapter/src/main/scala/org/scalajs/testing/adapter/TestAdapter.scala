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

package org.scalajs.testing.adapter

import scala.concurrent._
import scala.concurrent.duration._
import scala.collection.concurrent.TrieMap

import org.scalajs.logging._

import org.scalajs.jsenv._
import org.scalajs.jsenv.JSUtils.escapeJS

import org.scalajs.testing.common._

import sbt.testing.Framework

final class TestAdapter(jsEnv: JSEnv, input: Seq[Input], config: TestAdapter.Config) {

  import TestAdapter._

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
    getRunnerForThread().com
      .call(JSEndpoints.detectFrameworks)(frameworkNames)
      .map(_.map(_.map(info => new FrameworkAdapter(info, this))))
      .await()
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
   *  We terminate everything if this happens to make sure nothing hangs waiting
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
      runners.clear()
    }
  }

  private[adapter] def runStarting(): RunMux.RunID = synchronized {
    require(!closed, "We are closed. Cannot create new run.")
    val runID = nextRunID
    nextRunID += 1
    runs += runID
    runID
  }

  /** Called by [[RunnerAdapter]] when the run is completed. */
  private[adapter] def runDone(runID: RunMux.RunID): Unit = synchronized {
    require(runs.contains(runID), s"Tried to remove nonexistent run $runID")
    runs -= runID
  }

  private[adapter] def getRunnerForThread(): ManagedRunner = {
    val threadId = Thread.currentThread().getId()

    // Note that this is thread safe, since each thread can only operate on
    // the value associated to its thread id.
    runners.getOrElseUpdate(threadId, startManagedRunner(threadId))
  }

  private def startManagedRunner(threadId: Long): ManagedRunner = synchronized {
    // Prevent runners from being started after we are closed.
    // Otherwise we might leak runners.
    require(!closed, "We are closed. Cannot create new runner.")

    val runConfig = RunConfig().withLogger(config.logger)
    val com = new JSEnvRPC(jsEnv, input, runConfig)
    val mux = new RunMuxRPC(com)

    new ManagedRunner(threadId, com, mux)
  }
}

object TestAdapter {
  final class Config private (
      val logger: Logger
  ) {
    private def this() = {
      this(
          logger = NullLogger
      )
    }

    def withLogger(logger: Logger): Config =
      copy(logger = logger)

    private def copy(
        logger: Logger = logger
    ): Config = {
      new Config(logger)
    }
  }

  object Config {
    def apply(): Config = new Config()
  }

  private[adapter] final class ManagedRunner(
      val id: Long,
      val com: RPCCore,
      val mux: RunMuxRPC
  )
}
