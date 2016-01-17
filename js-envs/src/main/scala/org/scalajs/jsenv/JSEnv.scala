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

trait JSEnv {
  /** Human-readable name for this [[JSEnv]] */
  def name: String

  /** Prepare a runner for the code in the virtual file. */
  def jsRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile): JSRunner

  /** Prepare a runner without any libraries.
   *
   *  Strictly equivalent to:
   *  {{{
   *  this.jsRunner(Nil, code)
   *  }}}
   */
  final def jsRunner(code: VirtualJSFile): JSRunner = jsRunner(Nil, code)

  /** Return this [[JSEnv]] with the given libraries already loaded.
   *
   *  The following two are equivalent:
   *  {{{
   *  jsEnv.loadLibs(a).jsRunner(b, c)
   *  jsEnv.jsRunner(a ++ b, c)
   *  }}}
   */
  def loadLibs(libs: Seq[ResolvedJSDependency]): JSEnv =
    new LoadedLibs { val loadedLibs = libs }

  private[jsenv] trait LoadedLibs extends JSEnv {
    val loadedLibs: Seq[ResolvedJSDependency]

    def name: String = JSEnv.this.name

    def jsRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile): JSRunner =
      JSEnv.this.jsRunner(loadedLibs ++ libs, code)
  }
}
