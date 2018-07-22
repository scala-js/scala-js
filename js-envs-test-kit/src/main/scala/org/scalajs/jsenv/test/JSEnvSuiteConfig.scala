package org.scalajs.jsenv.test

import org.scalajs.jsenv.JSEnv

import scala.concurrent.duration._

/** Configuration for a [[JSEnvSuite]].
 *
 *  @see [[JSEnvSuite]] for usage.
 *
 *  @param jsEnv [[JSEnv]] under test.
 *  @param terminateVMJSCode A JavaScript expression that terminates the VM.
 *      If set, proper handling of VM termination is tested.
 *  @param supportsCom Whether the [[JSEnv]] under test supports
 *      [[JSEnv#startWithCom]].
 *  @param supportsTimeout Whether the [[JSEnv]] under test supports the
 *      JavaScript timeout methods (as defined in
 *      [[http://www.scala-js.org/api/scalajs-library/latest/#scala.scalajs.js.timers.RawTimers$ RawTimers]]).
 *  @param awaitTimeout Amount of time test cases wait for "things". This is
 *      deliberately not very well specified. Leave this as the default and
 *      increase it if your tests fail spuriously due to timeouts.
 *  @param description A human readable description of this configuration;
 *      defaults to [[JSEnv#name]]. This is only ever used in the parametrized
 *      JUnit test name. Can be customized if the same [[JSEnv]] is used with
 *      different configurations (e.g. Selenium with different browsers).
 */
final class JSEnvSuiteConfig private (
    val jsEnv: JSEnv,
    val supportsExit: Boolean,
    val supportsCom: Boolean,
    val supportsTimeout: Boolean,
    val awaitTimeout: FiniteDuration,
    val description: String
) {
  private def this(jsEnv: JSEnv) = this(
      jsEnv = jsEnv,
      supportsExit = true,
      supportsCom = true,
      supportsTimeout = true,
      awaitTimeout = 1.minute,
      description = jsEnv.name
  )

  def withSupportsExit(supportsExit: Boolean): JSEnvSuiteConfig =
    copy(supportsExit = supportsExit)

  def withSupportsCom(supportsCom: Boolean): JSEnvSuiteConfig =
    copy(supportsCom = supportsCom)

  def withSupportsTimeout(supportsTimeout: Boolean): JSEnvSuiteConfig =
    copy(supportsTimeout = supportsTimeout)

  def withAwaitTimeout(awaitTimeout: FiniteDuration): JSEnvSuiteConfig =
    copy(awaitTimeout = awaitTimeout)

  def withDescription(description: String): JSEnvSuiteConfig =
    copy(description = description)

  private def copy(
      supportsExit: Boolean = supportsExit,
      supportsCom: Boolean = supportsCom,
      supportsTimeout: Boolean = supportsTimeout,
      awaitTimeout: FiniteDuration = awaitTimeout,
      description: String = description) = {
    new JSEnvSuiteConfig(jsEnv, supportsExit, supportsCom,
        supportsTimeout, awaitTimeout, description)
  }
}

object JSEnvSuiteConfig {
  def apply(jsEnv: JSEnv): JSEnvSuiteConfig = new JSEnvSuiteConfig(jsEnv)
}
