/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.environment

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._

trait ScalaJSEnvironment {
  /** Run the codeimport scala.scalajs.tools.environment.Console
 in the virtual file. Return Some(<error message>) if failed
   *  None otherwise
   */
  def runJS(classpath: JSClasspath, code: VirtualJSFile,
      console: Console): Option[String]
}
