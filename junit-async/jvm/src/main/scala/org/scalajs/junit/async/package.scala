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

package org.scalajs.junit

import scala.concurrent._
import scala.concurrent.duration.Duration

package object async {
  type AsyncResult = Unit
  def await(f: Future[_]): AsyncResult = Await.result(f, Duration.Inf)
}
