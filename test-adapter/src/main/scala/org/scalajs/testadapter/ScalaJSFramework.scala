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
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker.ModuleKind

import org.scalajs.testadapter.json._
import org.scalajs.jsenv._
import org.scalajs.testcommon._

import sbt.testing.{Logger => _, _}

/** A shim over [[TestAdapter]] for compatiblity. */
@deprecated("Use TestAdapter instead.", "0.6.22")
final class ScalaJSFramework(
    frameworkName: String,
    jsEnv: ComJSEnv,
    jsFiles: Seq[VirtualJSFile],
    moduleKind: ModuleKind,
    moduleIdentifier: Option[String],
    logger: Logger
) extends Framework {

  def this(frameworkName: String, jsEnv: ComJSEnv, jsFiles: Seq[VirtualJSFile],
      logger: Logger) = {
    this(frameworkName, jsEnv, jsFiles, ModuleKind.NoModule, None, logger)
  }

  private[this] val adapter = {
    val config = TestAdapter.Config()
      .withLogger(logger)
      .withModuleSettings(moduleKind, moduleIdentifier)

    new TestAdapter(jsEnv, jsFiles, config)
  }

  private[this] val realFramework =
    adapter.loadFrameworks(List(List(frameworkName))).head.get

  def name: String = realFramework.name

  def fingerprints: Array[Fingerprint] = realFramework.fingerprints

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner = {
    realFramework.runner(args, remoteArgs, testClassLoader)
  }

  override protected def finalize(): Unit = adapter.close()
}
