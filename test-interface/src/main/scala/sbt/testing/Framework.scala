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

package sbt.testing

import scala.scalajs.reflect.annotation._

/** Interface implemented by test frameworks. */
@EnableReflectiveInstantiation
trait Framework {

  /** A human-friendly name of the test framework that this object represents. */
  def name(): String

  /** An array of <a href="Fingerprint.html"><code>Fingerprint</code></a>s
   *  that specify how to identify test classes during discovery.
   */
  def fingerprints(): Array[Fingerprint]

  /** Initiates a run.
   *
   *  If a client invokes this method before a previously initiated run has
   *  completed, the test framework may throw
   *  <code>IllegalStateException</code> to indicate it cannot perform the two
   *  runs concurrently.
   *
   *  @param args the test-framework-specific arguments for the new run
   *  @param remoteArgs the test-framework-specific remote arguments for the run in a forked JVM
   *  @param testClassLoader a class loader to use when loading test classes during the run
   *
   *  @return a <code>Runner</code> representing the newly started run.
   *  @throws java.lang.IllegalStateException if the test framework is unable to
   *      initiate a run because it is already performing a previously initiated
   *      run that has not yet completed.
   */
  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): Runner

  /** Scala.js specific: Creates a worker runner for a given run.
   *
   *  The worker may send a message to the controller runner by calling `send`.
   *
   *  @note
   *    This method is called `slaveRunner` rather than `workerRunner` for
   *    historical reasons. To preserve binary compatibility, it cannot be
   *    renamed. Moreover, since it must be implemented by user code, we cannot
   *    add another method with the right name and deprecate this one either.
   */
  def slaveRunner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader, send: String => Unit): Runner
}
