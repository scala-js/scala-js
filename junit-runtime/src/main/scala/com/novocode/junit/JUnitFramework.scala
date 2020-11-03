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

package com.novocode.junit

import sbt.testing._

/** Forwarder framework so no additional framework name is needed in sbt.
 *
 *  Note that a type alias is not enough, since sbt looks at the runtime type.
 */
final class JUnitFramework extends Framework {
  private val f = new org.scalajs.junit.JUnitFramework

  val name: String = f.name

  def fingerprints(): Array[Fingerprint] = f.fingerprints()

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner = {
    f.runner(args, remoteArgs, testClassLoader)
  }

  def slaveRunner(args: Array[String], remoteArgs: Array[String], testClassLoader: ClassLoader,
      send: String => Unit): Runner = {
    f.slaveRunner(args, remoteArgs, testClassLoader, send)
  }
}
