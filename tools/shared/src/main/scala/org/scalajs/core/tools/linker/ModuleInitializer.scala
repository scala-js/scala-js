/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js tools             **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2017, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

package org.scalajs.core.tools.linker

import org.scalajs.core.ir.Definitions._
import org.scalajs.core.ir.Types.ClassType

import org.scalajs.core.tools.linker.analyzer.SymbolRequirement

/** A module initializer for a Scala.js application.
 *
 *  When linking a Scala.js application, a sequence of `ModuleInitializer`s can
 *  be given. Those module initializers will be executed at the startup of the
 *  application. More specifically, the top-level code of the ECMAScript 2015
 *  module emitted for the application will invoke the specified module
 *  initializers in the specified order, after having initialized everything
 *  else (notably static initializers).
 *
 *  Instances of `ModuleInitializer` can be created with methods of
 *  [[ModuleInitializer$ the ModuleInitializer companion object]].
 */
sealed abstract class ModuleInitializer

/** Factory for [[ModuleInitializer]]s. */
object ModuleInitializer {
  private[linker] final case class VoidMainMethod(moduleClassName: String,
      encodedMainMethodName: String)
      extends ModuleInitializer

  /** Makes an [[ModuleInitializer]] that calls a zero-argument method returning
   *  `Unit` in a top-level `object`.
   *
   *  @param moduleClassName
   *    The fully-qualified name of the module class, e.g., `"foo.bar.Babar"`.
   *    Note that it does not end with `$`.
   *  @param mainMethodName
   *    The name of the main method to invoke, e.g., `"main"`.
   */
  def mainMethod(moduleClassName: String,
      mainMethodName: String): ModuleInitializer = {
    VoidMainMethod(encodeClassName(moduleClassName + "$"),
        mainMethodName + "__V")
  }

  def toSymbolRequirement(
      entryPoints: Seq[ModuleInitializer]): SymbolRequirement = {
    val factory = SymbolRequirement.factory("module initializers")
    val requirements = for (entryPoint <- entryPoints) yield {
      entryPoint match {
        case VoidMainMethod(moduleClassName, mainMethodName) =>
          factory.callOnModule(moduleClassName, mainMethodName)
      }
    }
    factory.multiple(requirements: _*)
  }
}
