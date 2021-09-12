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

import scala.concurrent.Future

/* Use the queue execution context (based on JS promises) explicitly:
 * We do not have anything better at our disposal and it is accceptable in
 * terms of fairness: All we use it for is to map over a completed Future once.
 */
import scala.scalajs.concurrent.JSExecutionContext.Implicits.queue

import scala.util.{Try, Success}

package object async {
  type AsyncResult = Future[Try[Unit]]
  def await(f: Future[_]): AsyncResult = f.map(_ => Success(()))
}
