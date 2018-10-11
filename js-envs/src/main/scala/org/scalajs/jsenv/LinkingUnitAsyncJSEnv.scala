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
import org.scalajs.core.tools.linker.LinkingUnit

trait LinkingUnitAsyncJSEnv extends LinkingUnitJSEnv with AsyncJSEnv {
  def asyncRunner(preLibs: Seq[ResolvedJSDependency], linkingUnit: LinkingUnit,
      postLibs: Seq[ResolvedJSDependency], code: VirtualJSFile): AsyncJSRunner

  override def loadLibs(libs: Seq[ResolvedJSDependency]): LinkingUnitAsyncJSEnv =
    new LinkingUnitAsyncLoadedLibs { val loadedLibs = libs }

  override def loadLinkingUnit(linkingUnit: LinkingUnit): AsyncJSEnv =
    new AsyncLoadedUnit { val loadedUnit = linkingUnit }

  private[jsenv] trait LinkingUnitAsyncLoadedLibs extends LinkingUnitLoadedLibs
      with AsyncLoadedLibs with LinkingUnitAsyncJSEnv {
    def asyncRunner(preLibs: Seq[ResolvedJSDependency], linkingUnit: LinkingUnit,
        postLibs: Seq[ResolvedJSDependency],
        code: VirtualJSFile): AsyncJSRunner = {
      LinkingUnitAsyncJSEnv.this.asyncRunner(loadedLibs ++ preLibs, linkingUnit,
          postLibs, code)
    }
  }

  private[jsenv] trait AsyncLoadedUnit extends LoadedUnit with AsyncJSEnv {
    def asyncRunner(libs: Seq[ResolvedJSDependency],
        code: VirtualJSFile): AsyncJSRunner = {
      LinkingUnitAsyncJSEnv.this.asyncRunner(Nil, loadedUnit, libs, code)
    }
  }
}
