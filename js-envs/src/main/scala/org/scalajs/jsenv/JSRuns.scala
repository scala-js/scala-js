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

package org.scalajs.jsenv

import scala.concurrent.Future

/** A launched instance of a [[JSEnv]].
 *
 *  This is the interface to actually running JS code (whether this is in
 *  process or not depens on the [[JSEnv]] that created the [[JSRun]]).
 *
 *  Any implementation is expected to be fully thread-safe.
 */
trait JSRun extends AutoCloseable {
  /** A [[scala.concurrent.Future Future]] that completes if the run completes.
   *
   *  The future is failed if the run fails.
   *
   *  Note that a [[JSRun]] is not required to ever terminate on it's own. That
   *  means even if all code is executed and the event loop is empty, the run
   *  may continue to run. As a consequence, it is *not* correct to rely on
   *  termination of a [[JSRun]] without any external means of stopping it
   *  (i.e. calling [[close]]).
   */
  def future: Future[Unit]

  /** Stops the run and releases all the resources.
   *
   *  This <strong>must</strong> be called to ensure the run's resources are
   *  released.
   *
   *  Whether or not this makes the run fail or not is up to the implementation.
   *  However, in the following cases, calling [[close]] may not fail the run:
   *  <ul>
   *  <li>[[future]] is already completed when [[close]] is called.
   *  <li>This is a [[JSComRun]] and the event loop inside the VM is empty.
   *  </ul>
   *
   *  Idempotent, async, nothrow.
   */
  def close(): Unit
}

object JSRun {
  /** Creates a [[JSRun]] that has failed. */
  def failed(cause: Throwable): JSRun = new JSRun {
    def close(): Unit = ()
    val future: Future[Unit] = Future.failed(cause)
  }
}

/** A [[JSRun]] that has a communication channel to the running JS code. */
trait JSComRun extends JSRun {
  /** Sends a message to the JS end.
   *
   *  Async, nothrow. See [[JSEnv#startWithCom]] for expected message delivery
   *  guarantees.
   */
  def send(msg: String): Unit
}

object JSComRun {
  /** Creates a [[JSComRun]] that has failed. */
  def failed(cause: Throwable): JSComRun = new JSComRun {
    def close(): Unit = ()
    val future: Future[Unit] = Future.failed(cause)
    def send(msg: String): Unit = ()
  }
}
