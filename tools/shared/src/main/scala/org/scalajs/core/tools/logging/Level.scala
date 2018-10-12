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

package org.scalajs.core.tools.logging

import scala.math.Ordered

abstract sealed class Level extends Ordered[Level] { x =>
  protected val order: Int
  def compare(y: Level): Int = x.order - y.order
}

object Level {
  case object Error extends Level { protected val order = 4 }
  case object Warn  extends Level { protected val order = 3 }
  case object Info  extends Level { protected val order = 2 }
  case object Debug extends Level { protected val order = 1 }
}
