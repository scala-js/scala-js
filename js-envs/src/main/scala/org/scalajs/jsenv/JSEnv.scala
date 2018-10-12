/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
