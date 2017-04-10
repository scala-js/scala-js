/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv

import org.scalajs.core.tools.io.VirtualJSFile

trait JSEnv {
  /** Human-readable name for this [[JSEnv]] */
  def name: String

  /** Prepare a runner with the specified JavaScript files. */
  def jsRunner(files: Seq[VirtualJSFile]): JSRunner

  /** Return this [[JSEnv]] with the given libraries already loaded.
   *
   *  The following two are equivalent:
   *  {{{
   *  jsEnv.loadLibs(a).jsRunner(b)
   *  jsEnv.jsRunner(a ++ b)
   *  }}}
   */
  def loadLibs(libs: Seq[VirtualJSFile]): JSEnv =
    new LoadedLibs { val loadedLibs = libs }

  private[jsenv] trait LoadedLibs extends JSEnv {
    val loadedLibs: Seq[VirtualJSFile]

    def name: String = JSEnv.this.name

    def jsRunner(files: Seq[VirtualJSFile]): JSRunner =
      JSEnv.this.jsRunner(loadedLibs ++ files)
  }
}
