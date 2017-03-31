/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv

import org.scalajs.core.tools.io.VirtualJSFile

trait AsyncJSEnv extends JSEnv {
  def asyncRunner(libs: Seq[VirtualJSFile], code: VirtualJSFile): AsyncJSRunner

  final def asyncRunner(code: VirtualJSFile): AsyncJSRunner =
    asyncRunner(Nil, code)

  override def loadLibs(libs: Seq[VirtualJSFile]): AsyncJSEnv =
    new AsyncLoadedLibs { val loadedLibs = libs }

  private[jsenv] trait AsyncLoadedLibs extends LoadedLibs with AsyncJSEnv {
    def asyncRunner(libs: Seq[VirtualJSFile],
        code: VirtualJSFile): AsyncJSRunner = {
      AsyncJSEnv.this.asyncRunner(loadedLibs ++ libs, code)
    }
  }
}
