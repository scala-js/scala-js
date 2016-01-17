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
