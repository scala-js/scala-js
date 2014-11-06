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

/** An [[AsyncJSEnv]] that provides communication to and from the JS VM.
 *
 *  Inside the VM there is a global JavaScript object named `scalajsCom` that
 *  can be used to control the message channel. It's operations are:
 *  {{{
 *  // initialize com (with callback)
 *  scalajsCom.init(function(msg) { console.log("Received: " + msg); });
 *
 *  // send a message to host system
 *  scalajsCom.send("my message");
 *
 *  // close com (releases callback, allowing VM to terminate)
 *  scalajsCom.close();
 *  }}}
 */
trait ComJSEnv extends AsyncJSEnv {
  def comRunner(classpath: CompleteClasspath, code: VirtualJSFile,
      logger: Logger, console: JSConsole): ComJSRunner
}

object ComJSEnv {
  class ComClosedException extends Exception("JSCom has been closed")
}
