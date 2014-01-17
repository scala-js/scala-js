/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin

import sbt._

import environment.rhino.{ CodeBlock, Utilities }

/** An sbt Scala runner that runs Scala.js code in a Scala.js environment */
class ScalaJSEnvRun(env: ScalaJSEnvironment) extends ScalaRun {
  def run(mainClass: String, classpath: Seq[File], options: Seq[String],
      log: Logger): Option[String] = {
    log.info("Running " + mainClass + options.mkString(" ", " ", ""))
    env.runInContextAndScope { (context, scope) =>
      new CodeBlock(context, scope) with Utilities {
        callMainMethod(mainClass, options.toArray)
      }
    }
    None
  }
}
