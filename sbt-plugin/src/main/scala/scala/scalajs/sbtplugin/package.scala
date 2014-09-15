/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs

package object sbtplugin {

  @deprecated("Use OptimzerMode instead", "0.5.0")
  type InliningMode = OptimizerMode

  @deprecated("Use OptimzerMode instead", "0.5.0")
  final val InliningMode = OptimizerMode

}
