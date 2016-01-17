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
