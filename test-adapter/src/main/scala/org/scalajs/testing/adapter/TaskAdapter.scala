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

import scala.concurrent.Await
import scala.concurrent.duration.Duration

import org.scalajs.testing.common._

import sbt.testing._

private[adapter] final class TaskAdapter(taskInfo: TaskInfo, runID: RunMux.RunID,
    runnerGetter: () => RunMuxRPC)
    extends Task {

  def taskDef: TaskDef = taskInfo.taskDef
  def tags: Array[String] = taskInfo.tags.toArray

  def execute(handler: EventHandler, loggers: Array[Logger]): Array[Task] = {
    val runner = runnerGetter()

    def log[T](level: Logger => (T => Unit))(log: LogElement[T]) =
      level(loggers(log.index))(log.x)

    runner.attach(JVMEndpoints.event, runID)(handler.handle _)
    runner.attach(JVMEndpoints.logError, runID)(log(_.error))
    runner.attach(JVMEndpoints.logWarn, runID)(log(_.warn))
    runner.attach(JVMEndpoints.logInfo, runID)(log(_.info))
    runner.attach(JVMEndpoints.logDebug, runID)(log(_.debug))
    runner.attach(JVMEndpoints.logTrace, runID)(log(_.trace))

    try {
      val colorSupport = loggers.map(_.ansiCodesSupported).toList
      val req = new ExecuteRequest(taskInfo, colorSupport)

      runner.call(JSEndpoints.execute, runID)(req)
        .await()
        .map(new TaskAdapter(_, runID, runnerGetter))
        .toArray
    } finally {
      runner.detach(JVMEndpoints.event, runID)
      runner.detach(JVMEndpoints.logError, runID)
      runner.detach(JVMEndpoints.logWarn, runID)
      runner.detach(JVMEndpoints.logInfo, runID)
      runner.detach(JVMEndpoints.logDebug, runID)
      runner.detach(JVMEndpoints.logTrace, runID)
    }
  }
}
