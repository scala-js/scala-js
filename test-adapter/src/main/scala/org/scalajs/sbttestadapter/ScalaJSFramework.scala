/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.logging._

import org.scalajs.jsenv._

import sbt.testing.{Logger => _, _}

final class ScalaJSFramework(
    private[testadapter] val frameworkName: String,
    jsEnv: ComJSEnv,
    classpath: CompleteClasspath,
    logger: Logger,
    jsConsole: JSConsole
) extends Framework {

  private[this] val frameworkInfo = fetchFrameworkInfo()

  private[this] var _isRunning = false

  val name: String = frameworkInfo.name

  def fingerprints: Array[Fingerprint] = frameworkInfo.fingerprints.toArray

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner = synchronized {

    if (_isRunning) {
      throw new IllegalStateException(
        "Scala.js test frameworks do not support concurrent runs")
    }

    _isRunning = true

    new ScalaJSRunner(this, args, remoteArgs)
  }

  private[testadapter] def runDone(): Unit = synchronized(_isRunning = false)

  private[testadapter] def createRunner(launcher: VirtualJSFile): ComJSRunner =
    jsEnv.comRunner(classpath, launcher, logger, jsConsole)

  private def fetchFrameworkInfo() = {
    val runner = createRunner(frameworkInfoLauncher)
    runner.start()

    try {
      val msg = readJSON(runner.receive())
      fromJSON[FrameworkInfo](msg)
    } finally {
      runner.close()
      runner.await()
    }
  }

  private def frameworkInfoLauncher = {
    val name = jsonToString(frameworkName.toJSON)
    val code = s"""
      new org.scalajs.testinterface.internal.InfoSender($name).initAndSend();
    """
    new MemVirtualJSFile(s"testFrameworkInfo.js").withContent(code)
  }
}
