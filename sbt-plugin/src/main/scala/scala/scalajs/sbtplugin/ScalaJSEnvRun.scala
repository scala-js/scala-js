/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin

import sbt._
import scala.scalajs.tools.environment.ScalaJSEnvironment

import scala.scalajs.tools
import tools.io.MemVirtualJSFile
import tools.classpath.ScalaJSClasspath


/** An sbt Scala runner that runs Scala.js code in a Scala.js environment */
class ScalaJSEnvRun(env: ScalaJSEnvironment) extends ScalaRun {

  def run(mainClass: String, classpath: Seq[File], options: Seq[String],
      log: Logger): Option[String] = {

    log.info("Running " + mainClass + options.mkString(" ", " ", ""))
    log.debug(s"with classpath $classpath")

    env.runJS(
        // TODO abstract what kind of classpath to create.
        ScalaJSClasspath.readEntriesInClasspath(classpath),
        runnerVirtualFile(mainClass),
        new LoggerConsole(log))
  }

  private def runnerVirtualFile(mainClass: String) = {
    new MemVirtualJSFile("Generated launcher file").
      withContent(s"$mainClass().main();")
  }

}
