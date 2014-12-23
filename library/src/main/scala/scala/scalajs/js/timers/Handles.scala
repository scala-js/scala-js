/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.js.timers

import scala.scalajs.js

/** <span class="badge badge-non-std" style="float: right;">Non-Standard</span>
 *  A handle returned from a call to [[setTimeout]].
 *
 *  May only be used to pass to [[clearTimeout]].
 */
trait SetTimeoutHandle extends js.Any

/** <span class="badge badge-non-std" style="float: right;">Non-Standard</span>
 *  A handle returned from a call to [[setInterval]].
 *
 *  May only be used to pass to [[clearInterval]].
 */
trait SetIntervalHandle extends js.Any
