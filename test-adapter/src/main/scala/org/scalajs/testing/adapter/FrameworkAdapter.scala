/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testing.adapter

import org.scalajs.testing.common._
import sbt.testing._

private[adapter] final class FrameworkAdapter(info: FrameworkInfo,
    testAdapter: TestAdapter) extends Framework {

  val name: String = info.displayName

  def fingerprints: Array[Fingerprint] = info.fingerprints.toArray

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner = {
    RunnerAdapter(testAdapter, info.implName, args, remoteArgs)
  }

  override def toString(): String = s"FrameworkAdapter($name)"
}
