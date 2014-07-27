/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin

sealed trait InliningMode {
  val disabled: Boolean
  val batch: Boolean
}

object InliningMode {
  case object Incremental extends InliningMode {
    val disabled = false
    val batch = false
  }

  case object Batch extends InliningMode {
    val disabled = false
    val batch = true
  }

  case object Off extends InliningMode {
    val disabled = true
    val batch = false
  }
}
