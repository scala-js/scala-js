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

import scala.collection.concurrent.TrieMap

import scala.concurrent._
import scala.concurrent.duration._

import scala.util._

import org.scalajs.testing.common._

import sbt.testing._

import TestAdapter.ManagedRunner

private final class RunnerAdapter private (runnerArgs: RunnerArgs,
    controller: ManagedRunner, testAdapter: TestAdapter)
    extends Runner {

  private val runID = runnerArgs.runID
  private val rpcGetter = () => getRunnerRPC()

  private val workers = TrieMap.empty[Long, ManagedRunner]

  // Route controller messages to workers.
  controller.mux.attach(JVMEndpoints.msgController, runID) { msg =>
    workers(msg.workerId).mux.send(JSEndpoints.msgWorker, runID)(msg.msg)
  }

  def args(): Array[String] = runnerArgs.args.toArray

  def remoteArgs(): Array[String] = runnerArgs.remoteArgs.toArray

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = {
    getRunnerRPC()
      .call(JSEndpoints.tasks, runID)(taskDefs.toList)
      .await()
      .map(new TaskAdapter(_, runID, rpcGetter))
      .toArray
  }

  def done(): String = synchronized {
    val workers = this.workers.values.toList // .toList to make it strict.

    try {
      workers.map(_.mux.call(JSEndpoints.done, runID)(())).foreach(_.await())
      controller.mux.call(JSEndpoints.done, runID)(()).await()
    } finally {
      workers.foreach(_.mux.detach(JVMEndpoints.msgWorker, runID))
      controller.mux.detach(JVMEndpoints.msgController, runID)

      this.workers.clear()
      testAdapter.runDone(runID)
    }
  }

  private def getRunnerRPC(): RunMuxRPC = {
    val mRunner = testAdapter.getRunnerForThread()

    if (mRunner != controller && !workers.contains(mRunner.id)) {
      // Put the worker in the map so messages can be routed.
      workers.put(mRunner.id, mRunner)

      // Attach message endpoint.
      mRunner.mux.attach(JVMEndpoints.msgWorker, runID) { msg =>
        controller.mux.send(JSEndpoints.msgController, runID)(
            new FrameworkMessage(mRunner.id, msg))
      }

      // Start worker.
      mRunner.com.call(JSEndpoints.createWorkerRunner)(runnerArgs).await()
    }

    mRunner.mux
  }
}

private[adapter] object RunnerAdapter {
  def apply(testAdapter: TestAdapter, frameworkImplName: String,
      args: Array[String], remoteArgs: Array[String]): Runner = {
    val runID = testAdapter.runStarting()

    try {
      val runnerArgs = new RunnerArgs(runID, frameworkImplName,
          args.toList, remoteArgs.toList)
      val mRunner = testAdapter.getRunnerForThread()
      mRunner.com.call(JSEndpoints.createControllerRunner)(runnerArgs).await()

      new RunnerAdapter(runnerArgs, mRunner, testAdapter)
    } catch {
      case t: Throwable =>
        testAdapter.runDone(runID)
        throw t
    }
  }
}
