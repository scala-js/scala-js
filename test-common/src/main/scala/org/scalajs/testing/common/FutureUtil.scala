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

package org.scalajs.testing.common

import scala.util._
import scala.concurrent._

private[testing] object FutureUtil {
  /** Same as Future.fromTry(x) but works in 2.10 */
  def futureFromTry[T](x: Try[T]): Future[T] = {
    val promise = Promise[T]
    promise.complete(x)
    promise.future
  }
}
