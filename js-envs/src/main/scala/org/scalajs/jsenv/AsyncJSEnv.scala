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

trait AsyncJSEnv extends JSEnv {
  def asyncRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile): AsyncJSRunner

  final def asyncRunner(code: VirtualJSFile): AsyncJSRunner =
    asyncRunner(Nil, code)

  override def loadLibs(libs: Seq[ResolvedJSDependency]): AsyncJSEnv =
    new AsyncLoadedLibs { val loadedLibs = libs }

  private[jsenv] trait AsyncLoadedLibs extends LoadedLibs with AsyncJSEnv {
    def asyncRunner(libs: Seq[ResolvedJSDependency],
        code: VirtualJSFile): AsyncJSRunner = {
      AsyncJSEnv.this.asyncRunner(loadedLibs ++ libs, code)
    }
  }
}
