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

package org.scalajs.testing.adapter

import org.scalajs.testing.common._
import sbt.testing._

private[adapter] final class FrameworkAdapter(info: FrameworkInfo, testAdapter: TestAdapter)
    extends Framework {

  val name: String = info.displayName

  def fingerprints: Array[Fingerprint] = info.fingerprints.toArray

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner = {
    RunnerAdapter(testAdapter, info.implName, args, remoteArgs)
  }

  override def toString(): String = s"FrameworkAdapter($name)"
}
