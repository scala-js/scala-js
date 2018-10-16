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

package org.scalajs.linker

import org.scalajs.ir.Definitions._
import org.scalajs.ir.Types.ClassType

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

  private[linker] final case class MainMethodWithArgs(moduleClassName: String,
      encodedMainMethodName: String, args: List[String])
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

  /** Makes an [[ModuleInitializer]] that calls a method of a top-level
   *  `object`, taking an `Array[String]` and returning `Unit`.
   *
   *  An empty array is passed as argument.
   *
   *  @param moduleClassName
   *    The fully-qualified name of the module class, e.g., `"foo.bar.Babar"`.
   *    Note that it does not end with `$`.
   *  @param mainMethodName
   *    The name of the main method to invoke, e.g., `"main"`.
   */
  def mainMethodWithArgs(moduleClassName: String,
      mainMethodName: String): ModuleInitializer = {
    mainMethodWithArgs(moduleClassName, mainMethodName, Nil)
  }

  /** Makes an [[ModuleInitializer]] that calls a method of a top-level
   *  `object`, taking an `Array[String]` and returning `Unit`.
   *
   *  An array containing the specified `args` is passed as argument.
   *
   *  @param moduleClassName
   *    The fully-qualified name of the module class, e.g., `"foo.bar.Babar"`.
   *    Note that it does not end with `$`.
   *  @param mainMethodName
   *    The name of the main method to invoke, e.g., `"main"`.
   *  @param args
   *    The arguments to pass as an array.
   */
  def mainMethodWithArgs(moduleClassName: String, mainMethodName: String,
      args: List[String]): ModuleInitializer = {
    MainMethodWithArgs(encodeClassName(moduleClassName + "$"),
        mainMethodName + "__AT__V", args)
  }

  private[linker] def toSymbolRequirement(
      entryPoints: Seq[ModuleInitializer]): standard.SymbolRequirement = {
    val factory = standard.SymbolRequirement.factory("module initializers")
    val requirements = for (entryPoint <- entryPoints) yield {
      entryPoint match {
        case VoidMainMethod(moduleClassName, mainMethodName) =>
          factory.callOnModule(moduleClassName, mainMethodName)

        case MainMethodWithArgs(moduleClassName, mainMethodName, _) =>
          factory.callOnModule(moduleClassName, mainMethodName) ++
          factory.classData("T")
      }
    }
    factory.multiple(requirements: _*)
  }
}
