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
import org.scalajs.core.tools.linker.analyzer.SymbolRequirement

trait LinkingUnitJSEnv extends JSEnv {
  /** Symbols this [[LinkingUnitJSEnv]] needs present in the
   *  [[org.scalajs.core.tools.linker.LinkingUnit LinkingUnit]] it receives.
   */
  val symbolRequirements: SymbolRequirement

  /** Prepare a runner for the code in the virtual file. */
  def jsRunner(preLibs: Seq[ResolvedJSDependency], linkingUnit: LinkingUnit,
      postLibs: Seq[ResolvedJSDependency], code: VirtualJSFile): JSRunner

  override def loadLibs(libs: Seq[ResolvedJSDependency]): LinkingUnitJSEnv =
    new LinkingUnitLoadedLibs { val loadedLibs = libs }

  /** Returns a [[JSEnv]] with the given
   *  [[org.scalajs.core.tools.linker.LinkingUnit LinkingUnit]] already loaded.
   *
   *  Note that any subsequent libraries will be inserted after the
   *  [[org.scalajs.core.tools.linker.LinkingUnit LinkingUnit]].
   *
   *  Hence, the following are equivalent:
   *  {{{
   *  jsEnv.loadUnit(a).jsRunner(b, c)
   *  jsEnv.jsRunner(Nil, a, b, c)
   *  }}}
   *
   *  If you need to load libraries before, you can use the [[loadLibs]] method:
   *  {{{
   *  jsEnv.loadLibs(a).loadUnit(b).jsRunner(c, d)
   *  // equivalent to
   *  jsEnv.jsRunner(a, b, c, d)
   *  }}}
   */
  def loadLinkingUnit(linkingUnit: LinkingUnit): JSEnv =
    new LoadedUnit { val loadedUnit = linkingUnit }

  private[jsenv] trait LinkingUnitLoadedLibs
      extends LoadedLibs with LinkingUnitJSEnv {
    val symbolRequirements: SymbolRequirement =
      LinkingUnitJSEnv.this.symbolRequirements

    def jsRunner(preLibs: Seq[ResolvedJSDependency], linkingUnit: LinkingUnit,
        postLibs: Seq[ResolvedJSDependency], code: VirtualJSFile): JSRunner = {
      LinkingUnitJSEnv.this.jsRunner(loadedLibs ++ preLibs,
          linkingUnit, postLibs, code)
    }
  }

  private[jsenv] trait LoadedUnit extends JSEnv {
    val loadedUnit: LinkingUnit

    def name: String = LinkingUnitJSEnv.this.name

    def jsRunner(libs: Seq[ResolvedJSDependency], code: VirtualJSFile): JSRunner =
      LinkingUnitJSEnv.this.jsRunner(Nil, loadedUnit, libs, code)
  }
}
