/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Framework    **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jasminetest

import sbt.testing._

final class JasmineFramework extends Framework {
  private[this] var hasRunner = false

  private object JasmineFingerprint extends SubclassFingerprint {
    val isModule: Boolean = true
    val superclassName: String = "org.scalajs.jasminetest.JasmineTest"
    val requireNoArgConstructor: Boolean = true
  }

  val name: String = "Jasmine for Scala.js"

  def fingerprints: Array[Fingerprint] = Array(JasmineFingerprint)

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner = {
    acquireRunner()
    new JasmineRunner(this, args, remoteArgs, testClassLoader)
  }

  def slaveRunner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader, channel: String => Unit): Runner = {
    acquireRunner()
    new JasmineRunner(this, args, remoteArgs, testClassLoader)
  }

  private[jasminetest] def runnerDone(): Unit = hasRunner = false

  private def acquireRunner(): Unit = {
    if (hasRunner)
      throw new IllegalStateException("Jasmine doesn't support concurrent runs")
    hasRunner = true
  }
}
