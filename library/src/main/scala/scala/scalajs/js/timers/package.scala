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

package scala.scalajs.js

import scala.concurrent.duration.FiniteDuration

/** <span class="badge badge-non-std" style="float: right;">Non-Standard</span>
 *  Non-standard, but in general well supported methods to schedule asynchronous
 *  execution.
 *
 *  The methods in this package work in all JavaScript virtual machines
 *  supporting `setTimeout` and `setInterval`.
 */
package object timers {

  /** Schedule something for execution in `interval` milliseconds.
   *
   *  @param interval duration in milliseconds to wait
   *  @param body code to execute after `interval` has passed
   *  @return A handle that can be used to cancel the timeout by passing it
   *          to [[clearTimeout]].
   *  @note Uses JavaScript's non-standard `setTimeout`
   */
  def setTimeout(interval: Double)(body: => Unit): SetTimeoutHandle =
    RawTimers.setTimeout(() => body, interval)

  /** Schedule something for execution after a duration.
   *
   *  @param interval duration to wait
   *  @param body code to execute after `interval` has passed
   *  @return A handle that can be used to cancel the timeout by passing it
   *          to [[clearTimeout]].
   *  @note Uses JavaScript's non-standard `setTimeout`
   */
  def setTimeout(interval: FiniteDuration)(body: => Unit): SetTimeoutHandle =
    RawTimers.setTimeout(() => body, interval.toMillis.toDouble)

  /** Cancel a timeout execution
   *  @param handle The handle returned by
   *         [[setTimeout(interval:scala\.concurrent\.duration\.FiniteDuration)* setTimeout]].
   *  @note Uses JavaScript's non-standard `clearTimeout`
   */
  def clearTimeout(handle: SetTimeoutHandle): Unit =
    RawTimers.clearTimeout(handle)

  /** Schedule something for repeated execution every `interval` milliseconds.
   *
   *  @param interval duration in milliseconds between executions
   *  @param body code to execute after each `interval`
   *  @return A handle that can be used to cancel the interval by passing it
   *          to [[clearInterval]].
   *  @note Uses JavaScript's non-standard `setInterval`
   */
  def setInterval(interval: Double)(body: => Unit): SetIntervalHandle =
    RawTimers.setInterval(() => body, interval)

  /** Schedule something for repeated execution every duration.
   *
   *  @param interval duration between executions
   *  @param body code to execute after each `interval`
   *  @return A handle that can be used to cancel the interval by passing it
   *          to [[clearInterval]].
   *  @note Uses JavaScript's non-standard `setInterval`
   */
  def setInterval(interval: FiniteDuration)(body: => Unit): SetIntervalHandle =
    RawTimers.setInterval(() => body, interval.toMillis.toDouble)

  /** Cancel an interval execution
   *  @param handle The handle returned by
   *         [[setInterval(interval:scala\.concurrent\.duration\.FiniteDuration)* setInterval]].
   *  @note Uses JavaScript's non-standard `clearInterval`
   */
  def clearInterval(handle: SetIntervalHandle): Unit =
    RawTimers.clearInterval(handle)

}
