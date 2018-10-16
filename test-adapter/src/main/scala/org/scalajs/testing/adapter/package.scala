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

package org.scalajs.testing

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

package object adapter {
  private[adapter] implicit class AwaitFuture[T](val t: Future[T]) extends AnyVal {
    def await(): T = Await.result(t, Duration.Inf)
  }
}
