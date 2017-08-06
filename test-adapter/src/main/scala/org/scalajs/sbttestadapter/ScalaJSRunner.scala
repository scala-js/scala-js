/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.json._
import org.scalajs.jsenv._
import org.scalajs.testcommon._

import scala.collection.concurrent.TrieMap

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.util.{Try, Failure, Success}

import java.util.concurrent.atomic.AtomicInteger

import sbt.testing._

final class ScalaJSRunner private[testadapter] (
    framework: ScalaJSFramework,
    val args: Array[String],
    val remoteArgs: Array[String]
) extends Runner {
  import ScalaJSRunner._

  // State and simple vals

  private[this] var master: ComJSEnvRPC = null

  /** Map of ThreadId -> Slave */
  private[this] val slaves = TrieMap.empty[Long, ComJSEnvRPC]

  /** An object used as lock for the loggers. Ensures output does not get
   *  interleaved.
   */
  private[testadapter] val loggerLock = new Object

  // Constructor body

  createRemoteRunner()

  // Public API

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = synchronized {
    ensureNotDone()

    val taskInfos = Await.result(
        master.call(JSMasterEndpoints.tasks)(taskDefs.toList), 1.minutes)

    taskInfos.map(new ScalaJSTask(this, _)).toArray
  }

  def done(): String = synchronized {
    ensureNotDone()

    val slaves = this.slaves.values.toList // .toList to make it strict.

    def waitAndClose[T](v: Future[T], r: ComJSEnvRPC): Future[Try[T]] =
      v.liftToTry.map { x => r.close(); x }.liftToTry.map(_.flatten)

    def stopSlave(slave: ComJSEnvRPC): Future[Try[Unit]] =
      waitAndClose(slave.call(JSSlaveEndpoints.stopSlave)(()), slave)

    def stopMaster(): Future[Try[String]] =
      waitAndClose(master.call(JSMasterEndpoints.runnerDone)(()), master)

    val futureSummary = for {
      // First we run the stopping sequence of the slaves
      slaveTries <- Future.sequence(slaves.map(stopSlave))

      // Once all slaves are closing, we can schedule termination of the master.
      summaryTry <- stopMaster()

      // Now wait for termination of the VMs
      doneTries <- Future.sequence(master.runner.future.liftToTry ::
          slaves.map(_.runner.future.liftToTry))
    } yield {
      // Now that everything is done, we can throw any exception.
      (slaveTries ::: summaryTry :: doneTries).foreach(_.get)

      // Get the summary.
      summaryTry.get
    }

    try {
      /* We need to double the VMTermTimeout since we wait for the slaves and
       * the master in sequence.
       */
      Await.result(futureSummary, VMTermTimeout * 2)
    } finally {
      // Do the best we can to stop the VMs.
      master.runner.stop()
      slaves.foreach(_.runner.stop())

      master = null
      this.slaves.clear()

      framework.runDone()
    }
  }

  // Slave Management

  private[testadapter] def getSlave(): ComJSEnvRPC = {
    val threadId = Thread.currentThread().getId()

    // Note that this is thread safe, since each thread can only operate on
    // the value associated to its thread id.
    slaves.getOrElse(threadId, createSlave(threadId))
  }

  private def createSlave(threadId: Long): ComJSEnvRPC = {
    // We don't want to create new slaves when we're closing/closed
    ensureNotDone()

    // Launch the slave
    val slave = framework.libEnv.comRunner(slaveLauncher)
    slave.start(framework.logger, framework.jsConsole)

    // Setup RPC
    val com = new ComJSEnvRPC(slave)
    com.attach(JVMSlaveEndpoints.msg) { msg =>
      master.send(JSMasterEndpoints.msg)(new FrameworkMessage(threadId, msg))
    }

    // Put the slave into the map, so replies from the master can be routed.
    slaves.put(threadId, com)

    // Create a runner on the slave
    Await.result(com.call(JSSlaveEndpoints.newRunner)(()), 1.minutes)

    com
  }

  // Helpers

  private def slaveLauncher = {
    val prefix = framework.optionalExportsNamespacePrefix
    val frameworkJS = jsonToString(framework.frameworkName.toJSON)
    val argsJS = jsonToString(args.toList.toJSON)
    val remoteArgsJS = jsonToString(remoteArgs.toList.toJSON)
    val code = s"""
      new ${prefix}org.scalajs.testinterface.internal.Slave($frameworkJS,
        $argsJS, $remoteArgsJS);
    """
    new MemVirtualJSFile("testSlave.js").withContent(code)
  }

  private def masterLauncher = {
    val prefix = framework.optionalExportsNamespacePrefix
    val name = jsonToString(framework.frameworkName.toJSON)
    val code = s"""
      new ${prefix}org.scalajs.testinterface.internal.Master($name);
    """
    new MemVirtualJSFile(s"testMaster.js").withContent(code)
  }

  private def ensureNotDone(): Unit = synchronized {
    if (master == null)
      throw new IllegalStateException("Runner is already done")
  }

  private def createRemoteRunner(): Unit = {
    assert(master == null)

    val runner = framework.libEnv.comRunner(masterLauncher)
    runner.start(framework.logger, framework.jsConsole)

    master = new ComJSEnvRPC(runner)

    master.attach(JVMMasterEndpoints.msg) { msg =>
      slaves(msg.slaveId).send(JSSlaveEndpoints.msg)(msg.msg)
    }

    val req = new RunnerArgs(args.toList, remoteArgs.toList)
    Await.result(master.call(JSMasterEndpoints.newRunner)(req), 1.minute)
  }
}

private object ScalaJSRunner {
  implicit class RichFuture[T](val __self: Future[T]) extends AnyVal {
    def liftToTry: Future[Try[T]] =
      __self.map(Success(_)).recover(PartialFunction(Failure(_)))
  }
}
