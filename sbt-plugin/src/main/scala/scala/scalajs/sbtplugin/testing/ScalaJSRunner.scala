/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.testing

import scala.scalajs.tools.io._
import scala.scalajs.tools.env._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.json._

import scala.collection.concurrent.TrieMap

import java.util.concurrent.atomic.AtomicInteger

import sbt.testing._

import TaskDefSerializers._
import ComUtils.LoopHandler

final class ScalaJSRunner private[testing] (
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
  private[testing] val loggerLock = new Object

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

    stopSlaves()

    master.send("runnerDone")
    val summary = ComUtils.receiveResponse(master) {
      case ("ok", summary) => summary
    }
    master.close()

    // Await all JS VMs
    slaves.values.foreach(_.await())
    master.await()

    // Cleanup
    master = null
    slaves.clear()

    framework.runDone()

    summary
  }

  // Runner Messaging

  /** A handler for messages sent from the slave to the master */
  private[testing] def msgHandler(slave: ComJSRunner): LoopHandler[Nothing] = {
    case ("msg", msg) =>
      val optReply = synchronized {
        master.send(s"msg:$msg")
        ComUtils.receiveResponse(master) {
          case ("ok", msg) =>
            if (msg.startsWith(":s:")) Some(msg.stripPrefix(":s:"))
            else None
        }
      }

      for (reply <- optReply) {
        slave.send(s"msg:$reply")
        ComUtils.receiveResponse(slave)(ComUtils.okHandler)
      }

      None
  }

  // Slave Management

  private[testing] def getSlave(): ComJSRunner = {
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

  private def stopSlaves(): Unit = {
    val slaves = this.slaves.values

    slaves.foreach(_.send("stopSlave"))
    slaves foreach { slave =>
      ComUtils.receiveLoop(slave)(msgHandler(slave) orElse ComUtils.doneHandler)
    }
    slaves.foreach(_.close())
  }

  private def createSlave(): ComJSRunner = {
    // Launch the slave
    val slave = framework.createRunner(slaveLauncher)
    slave.start()

    // Create a runner on the slave
    slave.send("newRunner")
    ComUtils.receiveLoop(slave)(msgHandler(slave) orElse ComUtils.doneHandler)

    slave
  }

  // Helpers

  private def slaveLauncher = {
    val frameworkJS = jsonToString(framework.frameworkName.toJSON)
    val argsJS = jsonToString(args.toList.toJSON)
    val remoteArgsJS = jsonToString(args.toList.toJSON)
    val code = s"""
      new org.scalajs.testinterface.internal.Slave($frameworkJS,
        $argsJS, $remoteArgsJS).init();
    """
    new MemVirtualJSFile("testSlave.js").withContent(code)
  }

  private def masterLauncher = {
    val name = jsonToString(framework.frameworkName.toJSON)
    val code = s"""
      new org.scalajs.testinterface.internal.Master($name).init();
    """
    new MemVirtualJSFile(s"testMaster.js").withContent(code)
  }

  private def ensureNotDone(): Unit = synchronized {
    if (master == null)
      throw new IllegalStateException("Runner is already done")
  }

  private def createRemoteRunner(): Unit = {
    assert(master == null)

    master = framework.createRunner(masterLauncher)
    master.start()

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
