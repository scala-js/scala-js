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

package org.scalajs.linker.interface.unstable

import org.scalajs.ir.Definitions._

import org.scalajs.linker.interface.ModuleInitializer

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
sealed abstract class ModuleInitializerImpl extends ModuleInitializer {
  private[interface] def impl: ModuleInitializerImpl = this
}

object ModuleInitializerImpl {
  def fromModuleInitializer(mi: ModuleInitializer): ModuleInitializerImpl = mi.impl

  final case class VoidMainMethod(moduleClassName: ClassName,
      encodedMainMethodName: MethodName)
      extends ModuleInitializerImpl

  final case class MainMethodWithArgs(moduleClassName: ClassName,
      encodedMainMethodName: MethodName, args: List[String])
      extends ModuleInitializerImpl
}
