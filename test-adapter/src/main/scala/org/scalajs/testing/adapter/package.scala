/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.testing

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

package object adapter {
  private[adapter] implicit class AwaitFuture[T](val t: Future[T]) extends AnyVal {
    def await(): T = Await.result(t, Duration.Inf)
  }
}
