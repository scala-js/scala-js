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

package org.scalajs.testadapter

import org.scalajs.core.ir.Utils.escapeJS

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.linker.backend.ModuleKind

import org.scalajs.jsenv._
import org.scalajs.testcommon._

import sbt.testing.{Logger => _, _}

/** A shim over [[TestAdapter]] for compatiblity. */
@deprecated("Use TestAdapter instead.", "0.6.22")
final class ScalaJSFramework(
    frameworkName: String,
    libEnv: ComJSEnv,
    moduleKind: ModuleKind,
    moduleIdentifier: Option[String],
    logger: Logger,
    jsConsole: JSConsole
) extends Framework {

  def this(frameworkName: String, libEnv: ComJSEnv, logger: Logger,
      jsConsole: JSConsole) = {
    this(frameworkName, libEnv, ModuleKind.NoModule, None, logger, jsConsole)
  }

  private[this] val adapter = {
    val config = TestAdapter.Config()
      .withLogger(logger)
      .withJSConsole(jsConsole)
      .withModuleSettings(moduleKind, moduleIdentifier)

    new TestAdapter(libEnv, config)
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
