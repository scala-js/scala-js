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

/** Module output specification. */
final class ModuleOutputSpec private (modules: Seq[(ModuleOutputSpec.Selector, ModuleOutputSpec.Module)]) {
  private def this() = this(Nil)

  def addModule(selector: ModuleOutputSpec.Selector, module: ModuleOutputSpec.Module): ModuleOutputSpec = {
    // TODO ensure selectors don't conflict.
    ???
    new ModuleOutputSpec(modules :+ (selector, module))
  }
}

object ModuleOutputSpec {
  def apply(): ModuleOutputSpec = new ModuleOutputSpec()

  final class ModuleID private[ModuleOutputSpec] (val id: String) extends AnyVal

  final class Module private (
      val id: ModuleID,
      initializers: Seq[ModuleInitializer],
      deps: Seq[ModuleID]
  ) {
    private def this(id: ModuleID) = this(id, Nil, Nil)

    def withInitializers(initializers: ModuleInitializer*): Module =
      copy(initializers = initializers)

    def withDeps(deps: ModuleID*): Module =
      copy(deps = deps)

    private def copy(
        initializers: Seq[ModuleInitializer] = initializers,
        deps: Seq[ModuleID] = deps): Module = {
      new Module(id, initializers, deps)
    }
  }

  object Module {
    def apply(id: String): Module = new Module(new ModuleID(id))
  }

  sealed abstract class Selector {
    def ++(that: Selector): Selector = ???
  }

  private final case class PackageName(name: String) extends Selector
  private final case class ClassName(name: String) extends Selector
  private final case object Default extends Selector

  object Selector {
    def default: Selector = Default
    def packageName(name: String): Selector = PackageName(name)
    def className(name: String): Selector = ClassName(name)
  }
}
