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

package scala.scalajs.js.timers

import scala.scalajs.js
import js.annotation.JSGlobalScope

/** <span class="badge badge-non-std" style="float: right;">Non-Standard</span>
 *  Raw JavaScript timer methods.
 *
 *  The methods on this object expose the raw JavaScript methods for timers. In
 *  general it is more advisable to use the methods directly defined on
 *  [[timers]] as they are more Scala-like.
 *
 *  The methods exposed by this object are not standard in ECMAScript.
 *  Different JavaScript environments support all, some or none of them.
 *
 *  Browsers support all those methods as part the of the DOM standard.
 */
@js.native
@JSGlobalScope
object RawTimers extends js.Object {

  /** Schedule `handler` for execution in `interval` milliseconds.
   *
   *  @param handler the function to call after `interval` has passed
   *  @param interval duration in milliseconds to wait
   *  @return A handle that can be used to cancel the timeout by passing it
   *          to [[clearTimeout]].
   */
  def setTimeout(handler: js.Function0[Any], interval: Double): SetTimeoutHandle = js.native

  /** Cancel a timeout execution
   *  @param handle The handle returned by [[setTimeout]]
   */
  def clearTimeout(handle: SetTimeoutHandle): Unit = js.native

  /** Schedule `handler` for repeated execution every `interval`
   *  milliseconds.
   *
   *  @param handler the function to call after each `interval`
   *  @param interval duration in milliseconds between executions
   *  @return A handle that can be used to cancel the interval by passing it
   *          to [[clearInterval]].
   */
  def setInterval(handler: js.Function0[Any], interval: Double): SetIntervalHandle = js.native

  /** Cancel an interval execution
   *  @param handle The handle returned by [[setInterval]]
   */
  def clearInterval(handle: SetIntervalHandle): Unit = js.native

}
