/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2014, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.core.tools.sem

sealed abstract class CheckedBehavior {
  import CheckedBehavior._
  def optimized: CheckedBehavior = this match {
    case Fatal => Unchecked
    case _     => this
  }
}

object CheckedBehavior {
  case object Compliant extends CheckedBehavior
  case object Fatal extends CheckedBehavior
  case object Unchecked extends CheckedBehavior
}
