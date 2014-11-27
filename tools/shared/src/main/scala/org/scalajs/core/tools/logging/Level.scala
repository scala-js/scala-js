/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2014, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.logging

import scala.math.Ordered

abstract sealed class Level extends Ordered[Level] { x =>
  protected val order: Int
  def compare(y: Level) = x.order - y.order
}

object Level {
  case object Error extends Level { protected val order = 4 }
  case object Warn  extends Level { protected val order = 3 }
  case object Info  extends Level { protected val order = 2 }
  case object Debug extends Level { protected val order = 1 }
}
