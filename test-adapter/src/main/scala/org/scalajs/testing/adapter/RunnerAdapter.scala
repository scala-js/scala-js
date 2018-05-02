/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js test adapter      **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2017, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testing.adapter

import scala.collection.concurrent.TrieMap

import scala.concurrent._
import scala.concurrent.duration._

import scala.util._

import org.scalajs.testing.common._

import sbt.testing._

import FutureUtil._
import TestAdapter.ManagedRunner

private final class RunnerAdapter private (runnerArgs: RunnerArgs,
    master: ManagedRunner, testAdapter: TestAdapter) extends Runner {

  private val runID = runnerArgs.runID
  private val rpcGetter = () => getRunnerRPC()

  private val slaves = TrieMap.empty[Long, ManagedRunner]

  // Route master messages to slaves.
  master.mux.attach(JVMEndpoints.msgMaster, runID) { msg =>
    slaves(msg.slaveId).mux.send(JSEndpoints.msgSlave, runID)(msg.msg)
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
    val slaves = this.slaves.values.toList // .toList to make it strict.

    try {
      slaves.map(_.mux.call(JSEndpoints.done, runID)(())).foreach(_.await())
      master.mux.call(JSEndpoints.done, runID)(()).await()
    } finally {
      slaves.foreach(_.mux.detach(JVMEndpoints.msgSlave, runID))
      master.mux.detach(JVMEndpoints.msgMaster, runID)

      this.slaves.clear()
      testAdapter.runDone(runID)
    }
  }

  private def getRunnerRPC(): RunMuxRPC = {
    val mRunner = testAdapter.getRunnerForThread()

    if (mRunner != master && !slaves.contains(mRunner.id)) {
      // Put the slave in the map so messages can be routed.
      slaves.put(mRunner.id, mRunner)

      // Attach message endpoint.
      mRunner.mux.attach(JVMEndpoints.msgSlave, runID) { msg =>
        master.mux.send(JSEndpoints.msgMaster, runID)(
            new FrameworkMessage(mRunner.id, msg))
      }

      // Start slave.
      mRunner.com.call(JSEndpoints.createSlaveRunner)(runnerArgs).await()
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
      mRunner.com.call(JSEndpoints.createMasterRunner)(runnerArgs).await()

      new RunnerAdapter(runnerArgs, mRunner, testAdapter)
    } catch {
      case t: Throwable =>
        testAdapter.runDone(runID)
        throw t
    }
  }
}
