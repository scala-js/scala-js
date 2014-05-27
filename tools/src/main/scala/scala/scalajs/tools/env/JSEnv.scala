/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.tools.env

import scala.scalajs.tools.io._
import scala.scalajs.tools.classpath._
import scala.scalajs.tools.logging._

trait JSEnv {
  /** Run the code in the virtual file. Throw if failure */
  def runJS(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): Unit
}
