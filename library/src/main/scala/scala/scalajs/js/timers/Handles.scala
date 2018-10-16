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

/** <span class="badge badge-non-std" style="float: right;">Non-Standard</span>
 *  A handle returned from a call to
 * [[setTimeout(interval:scala\.concurrent\.duration\.FiniteDuration)* setTimeout]].
 *
 *  May only be used to pass to [[clearTimeout]].
 */
sealed trait SetTimeoutHandle extends js.Any

/** <span class="badge badge-non-std" style="float: right;">Non-Standard</span>
 *  A handle returned from a call to
 *  [[setInterval(interval:scala\.concurrent\.duration\.FiniteDuration)* setInterval]].
 *
 *  May only be used to pass to [[clearInterval]].
 */
sealed trait SetIntervalHandle extends js.Any
