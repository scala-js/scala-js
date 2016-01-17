/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv

import org.scalajs.core.tools.io.VirtualJSFile
import org.scalajs.core.tools.jsdep.ResolvedJSDependency

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
  def comRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile): ComJSRunner

  final def comRunner(code: VirtualJSFile): ComJSRunner = comRunner(Nil, code)

  override def loadLibs(libs: Seq[ResolvedJSDependency]): ComJSEnv =
    new ComLoadedLibs { val loadedLibs = libs }

  private[jsenv] trait ComLoadedLibs extends AsyncLoadedLibs with ComJSEnv {
    def comRunner(libs: Seq[ResolvedJSDependency],
        code: VirtualJSFile): ComJSRunner = {
      ComJSEnv.this.comRunner(loadedLibs ++ libs, code)
    }
  }
}

object ComJSEnv {
  private final val defaultMsg = "JSCom has been closed"

  class ComClosedException(msg: String,
      cause: Throwable) extends Exception(msg, cause) {
    def this() = this(defaultMsg, null)
    def this(cause: Throwable) = this(defaultMsg, cause)
    def this(msg: String) = this(msg, null)
  }
}
