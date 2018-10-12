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

trait LinkingUnitComJSEnv extends LinkingUnitAsyncJSEnv with ComJSEnv {
  def comRunner(preLibs: Seq[ResolvedJSDependency], linkingUnit: LinkingUnit,
      postLibs: Seq[ResolvedJSDependency], code: VirtualJSFile): ComJSRunner

  override def loadLibs(libs: Seq[ResolvedJSDependency]): LinkingUnitComJSEnv =
    new LinkingUnitComLoadedLibs { val loadedLibs = libs }

  override def loadLinkingUnit(linkingUnit: LinkingUnit): ComJSEnv =
    new ComLoadedUnit { val loadedUnit = linkingUnit }

  private[jsenv] trait LinkingUnitComLoadedLibs
      extends LinkingUnitAsyncLoadedLibs with ComLoadedLibs
      with LinkingUnitComJSEnv {
    def comRunner(preLibs: Seq[ResolvedJSDependency], linkingUnit: LinkingUnit,
        postLibs: Seq[ResolvedJSDependency],
        code: VirtualJSFile): ComJSRunner = {
      LinkingUnitComJSEnv.this.comRunner(loadedLibs ++ preLibs, linkingUnit,
          postLibs, code)
    }
  }

  private[jsenv] trait ComLoadedUnit extends AsyncLoadedUnit with ComJSEnv {
    def comRunner(libs: Seq[ResolvedJSDependency],
        code: VirtualJSFile): ComJSRunner = {
      LinkingUnitComJSEnv.this.comRunner(Nil, loadedUnit, libs, code)
    }
  }
}
