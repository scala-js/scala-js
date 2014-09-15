/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin

sealed trait OptimizerMode {
  val disabled: Boolean
  val batch: Boolean
}

object OptimizerMode {
  case object Incremental extends OptimizerMode {
    val disabled = false
    val batch = false
  }

  case object Batch extends OptimizerMode {
    val disabled = false
    val batch = true
  }

  case object Off extends OptimizerMode {
    val disabled = true
    val batch = false
  }
}
