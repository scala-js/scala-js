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

import scala.collection.concurrent.TrieMap

import scala.concurrent.duration._
import scala.util.{Try, Failure, Success}

import java.util.concurrent.atomic.AtomicInteger

import sbt.testing._

import TaskDefSerializers._
import ComUtils.LoopHandler

final class ScalaJSRunner private[testadapter] (
    framework: ScalaJSFramework,
    val args: Array[String],
    val remoteArgs: Array[String]
) extends Runner {

  // State and simple vals

  private[this] var master: ComJSRunner = null

  /** Map of ThreadId -> Slave */
  private[this] val slaves = TrieMap.empty[Long, ComJSRunner]

  /** An object used as lock for the loggers. Ensures output does not get
   *  interleaved.
   */
  private[testadapter] val loggerLock = new Object

  // Constructor body

  createRemoteRunner()

  // Public API

  def tasks(taskDefs: Array[TaskDef]): Array[Task] = synchronized {
    ensureNotDone()

    val outData = taskDefs.toList.toJSON
    master.send("tasks:" + jsonToString(outData))

    val taskInfos = ComUtils.receiveResponse(master) {
      case ("ok", data) => fromJSON[List[TaskInfo]](readJSON(data))
    }

    taskInfos.map(ScalaJSTask.fromInfo(this, _)).toArray
  }

  def done(): String = synchronized {
    ensureNotDone()

    /* Whatever happens in here, we must close and eventually terminate all
     * VMs. So we capture all exceptions in Try's, and we'll rethrow one of
     * them (if any) at the end.
     */

    // First we run the stopping sequence of the slaves
    val slavesDeadline = VMTermTimeout.fromNow
    val slavesClosing = stopSlaves(slavesDeadline)

    /* Once all slaves are closing, we can schedule termination of the master.
     * We need a fresh deadline for the master, since we can only start its
     * scheduling when the slaves are closing.
     * If we used the same deadline, and a slave timed out during its stopping
     * sequence, the master would have 0 ms to stop, which is not fair.
     */
    val masterDeadline = VMTermTimeout.fromNow
    val summaryTry = Try {
      master.send("runnerDone")
      val summary = ComUtils.receiveResponse(master, masterDeadline.timeLeft) {
        case ("ok", summary) => summary
      }
      master.close()
      summary
    }

    // Now we wait for everyone to be completely stopped
    val slavesStopped =
      slaves.values.toList.map(s => Try(s.awaitOrStop(slavesDeadline.timeLeft)))
    val masterStopped = Try(master.awaitOrStop(masterDeadline.timeLeft))

    // Cleanup
    master = null
    slaves.clear()

    framework.runDone()

    // At this point, rethrow any exception we captured on the way with Try's
    slavesClosing.get
    slavesStopped.foreach(_.get)
    masterStopped.get

    // And finally, if all went well, return the summary
    summaryTry.get
  }

  // Runner Messaging

  /** A handler for messages sent from the slave to the master */
  private[testadapter] def msgHandler(
      slave: ComJSRunner): LoopHandler[Nothing] = {
    case ("msg", msg) =>
      val optReply = synchronized {
        master.send(s"msg:$msg")
        ComUtils.receiveResponse(master) {
          case ("ok", msg) =>
            if (msg.startsWith(":s:")) Some(msg.stripPrefix(":s:"))
            else None
        }
      }

      for (reply <- optReply)
        slave.send(s"msg:$reply")

      None
  }

  // Slave Management

  private[testadapter] def getSlave(): ComJSRunner = {
    val threadId = Thread.currentThread().getId()

    // Note that this is thread safe, since each thread can only operate on
    // the value associated to its thread id.
    if (!slaves.contains(threadId)) {
      val slave = createSlave()
      slaves.put(threadId, slave)
      slave
    } else {
      slaves(threadId)
    }
  }

  /** Starts the stopping sequence of all slaves.
   *  The returned future will be completed when all slaves are closing.
   */
  private def stopSlaves(deadline: Deadline): Try[Unit] = {
    val slaves = this.slaves.values.toList // .toList to make it strict

    // First launch the stopping sequence on all slaves
    val stopMessagesSent = for (slave <- slaves) yield Try {
      slave.send("stopSlave")
    }

    // Then process all their messages and close them
    val slavesClosed = for (slave <- slaves) yield Try {
      ComUtils.receiveLoop(slave, deadline)(
          msgHandler(slave) orElse ComUtils.doneHandler)
      slave.close()
    }

    // Return the first failed of all these Try's
    (stopMessagesSent ++ slavesClosed) collectFirst {
      case failure: Failure[Unit] => failure
    } getOrElse Success(())
  }

  private def createSlave(): ComJSRunner = {
    // We don't want to create new slaves when we're closing/closed
    ensureNotDone()

    // Launch the slave
    val slave = framework.libEnv.comRunner(slaveLauncher)
    slave.start(framework.logger, framework.jsConsole)

    // Create a runner on the slave
    slave.send("newRunner")
    ComUtils.receiveLoop(slave)(msgHandler(slave) orElse ComUtils.doneHandler)

    slave
  }

  // Helpers

  private def slaveLauncher = {
    val prefix = framework.optionalExportsNamespacePrefix
    val frameworkJS = jsonToString(framework.frameworkName.toJSON)
    val argsJS = jsonToString(args.toList.toJSON)
    val remoteArgsJS = jsonToString(args.toList.toJSON)
    val code = s"""
      new ${prefix}org.scalajs.testinterface.internal.Slave($frameworkJS,
        $argsJS, $remoteArgsJS).init();
    """
    new MemVirtualJSFile("testSlave.js").withContent(code)
  }

  private def masterLauncher = {
    val prefix = framework.optionalExportsNamespacePrefix
    val name = jsonToString(framework.frameworkName.toJSON)
    val code = s"""
      new ${prefix}org.scalajs.testinterface.internal.Master($name).init();
    """
    new MemVirtualJSFile(s"testMaster.js").withContent(code)
  }

  private def ensureNotDone(): Unit = synchronized {
    if (master == null)
      throw new IllegalStateException("Runner is already done")
  }

  private def createRemoteRunner(): Unit = {
    assert(master == null)

    master = framework.libEnv.comRunner(masterLauncher)
    master.start(framework.logger, framework.jsConsole)

    val data = {
      val bld = new JSONObjBuilder
      bld.fld("args", args.toList)
      bld.fld("remoteArgs", remoteArgs.toList)
      bld.toJSON
    }

    master.send("newRunner:" + jsonToString(data))
    ComUtils.receiveResponse(master) {
      case ("ok", "") =>
    }
  }

}
