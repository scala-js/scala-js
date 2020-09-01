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

package org.scalajs.linker.interface

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.unstable.ModuleInitializerImpl

import Fingerprint.FingerprintBuilder

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
final class ModuleInitializer private (
    val initializer: ModuleInitializer.Initializer,
    val moduleID: String
) {
  private def this(initializer: ModuleInitializer.Initializer) =
    this(initializer, DefaultModuleID)

  def withModuleID(moduleID: String): ModuleInitializer =
    new ModuleInitializer(initializer, moduleID)
}

/** Factory for [[ModuleInitializer]]s. */
object ModuleInitializer {
  import ModuleInitializerImpl._

  abstract class Initializer private[interface] () {
    private[interface] def impl: ModuleInitializerImpl
  }

  private val ArrayOfStringTypeRef =
    ArrayTypeRef(ClassRef(BoxedStringClass), 1)

  /** Makes a [[ModuleInitializer]] that calls a static zero-argument method
   *  returning `Unit` in a top-level `class`.
   *
   *  @param className
   *    The fully-qualified name of the class, e.g., `"foo.bar.Babar"`.
   *  @param mainMethodName
   *    The name of the main method to invoke, e.g., `"main"`.
   */
  def mainMethod(className: String,
      mainMethodName: String): ModuleInitializer = {
    new ModuleInitializer(VoidMainMethod(ClassName(className),
        MethodName(mainMethodName, Nil, VoidRef)))
  }

  /** Makes a [[ModuleInitializer]] that calls a static method of a top-level
   *  `class` taking an `Array[String]` and returning `Unit`.
   *
   *  An empty array is passed as argument.
   *
   *  @param className
   *    The fully-qualified name of the class, e.g., `"foo.bar.Babar"`.
   *  @param mainMethodName
   *    The name of the main method to invoke, e.g., `"main"`.
   */
  def mainMethodWithArgs(className: String,
      mainMethodName: String): ModuleInitializer = {
    mainMethodWithArgs(className, mainMethodName, Nil)
  }

  /** Makes a [[ModuleInitializer]] that calls a static method of a top-level
   *  `class` taking an `Array[String]` and returning `Unit`.
   *
   *  An array containing the specified `args` is passed as argument.
   *
   *  @param className
   *    The fully-qualified name of the class, e.g., `"foo.bar.Babar"`.
   *  @param mainMethodName
   *    The name of the main method to invoke, e.g., `"main"`.
   *  @param args
   *    The arguments to pass as an array.
   */
  def mainMethodWithArgs(className: String, mainMethodName: String,
      args: List[String]): ModuleInitializer = {
    new ModuleInitializer(MainMethodWithArgs(ClassName(className),
        MethodName(mainMethodName, ArrayOfStringTypeRef :: Nil, VoidRef),
        args))
  }

  private implicit object MethodNameFingerprint
      extends Fingerprint[MethodName] {

    override def fingerprint(methodName: MethodName): String =
      methodName.nameString
  }

  private implicit object ClassNameFingerprint extends Fingerprint[ClassName] {
    override def fingerprint(className: ClassName): String =
      className.nameString
  }

  private implicit object InitializerFingerprint
      extends Fingerprint[Initializer] {

    override def fingerprint(initializer: Initializer): String =
      initializer.impl match {
        case VoidMainMethod(className, encodedMainMethodName) =>
          new FingerprintBuilder("VoidMainMethod")
            .addField("className", className)
            .addField("encodedMainMethodName", encodedMainMethodName)
            .build()

        case MainMethodWithArgs(className, encodedMainMethodName, args) =>
          new FingerprintBuilder("MainMethodWithArgs")
            .addField("className", className)
            .addField("encodedMainMethodName", encodedMainMethodName)
            .addField("args", args)
            .build()
      }
  }

  private implicit object ModuleInitializerFingerprint
      extends Fingerprint[ModuleInitializer] {
    override def fingerprint(moduleInitializer: ModuleInitializer): String = {
      new FingerprintBuilder("ModuleInitializer")
        .addField("initializer", moduleInitializer.initializer)
        .addField("moduleID", moduleInitializer.moduleID)
        .build()
    }
  }

  def fingerprint(moduleInitializer: ModuleInitializer): String =
    Fingerprint.fingerprint(moduleInitializer)
}
