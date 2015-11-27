/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib.util.concurrent

import java.{util => ju}

import org.scalajs.testsuite.javalib.util.MapFactory

object ConcurrentMapFactory {
  def allFactories: Iterator[ConcurrentMapFactory] =
    ConcurrentHashMapFactory.allFactories
}

trait ConcurrentMapFactory extends MapFactory {
  def empty[K, V]: ju.concurrent.ConcurrentMap[K, V]
}
