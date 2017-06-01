/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testadapter

import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker.backend.ModuleKind

import org.scalajs.jsenv._

import sbt.testing.{Logger => _, _}

final class ScalaJSFramework(
    private[testadapter] val frameworkName: String,
    private val jsEnv: ComJSEnv,
    private val jsFiles: Seq[VirtualJSFile],
    private[testadapter] val moduleKind: ModuleKind,
    private[testadapter] val moduleIdentifier: Option[String],
    private[testadapter] val logger: Logger
) extends Framework {

  def this(frameworkName: String, jsEnv: ComJSEnv, jsFiles: Seq[VirtualJSFile],
      logger: Logger) = {
    this(frameworkName, jsEnv, jsFiles, ModuleKind.NoModule, None, logger)
  }

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

  private[testadapter] def newComRunner(files: Seq[VirtualJSFile]): ComJSRunner =
    jsEnv.comRunner(jsFiles ++ files)

  private def fetchFrameworkInfo() = {
    val runner = newComRunner(frameworkInfoLauncher :: Nil)
    runner.start(logger, ConsoleJSConsole)

    try {
      val msg = readJSON(runner.receive())
      fromJSON[FrameworkInfo](msg)
    } finally {
      runner.close()
      runner.await(VMTermTimeout)
    }
  }

  private def frameworkInfoLauncher = {
    val prefix = optionalExportsNamespacePrefix
    val name = jsonToString(frameworkName.toJSON)
    val code = s"""
      new ${prefix}org.scalajs.testinterface.internal.InfoSender($name).initAndSend();
    """
    new MemVirtualJSFile(s"testFrameworkInfo.js").withContent(code)
  }

  private[testadapter] def optionalExportsNamespacePrefix: String = {
    // !!! DUPLICATE code with ScalaJSPlugin.makeExportsNamespaceExpr
    moduleKind match {
      case ModuleKind.NoModule =>
        ""

      case ModuleKind.CommonJSModule =>
        val moduleIdent = moduleIdentifier.getOrElse {
          throw new IllegalArgumentException(
              "The module identifier must be specified for CommonJS modules")
        }
        s"""require("${escapeJS(moduleIdent)}").""" // note the final '.'
    }
  }
}
