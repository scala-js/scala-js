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

package org.scalajs.sbtplugin

import sbt._

import org.scalajs.core.tools.jsdep.JSDependency

import SBTCompat._

/** Something JavaScript related a project may depend on. Either a JavaScript
 *  module/library, or the DOM at runtime. */
sealed trait AbstractJSDep {
  def configurations: Option[String]

  protected def withConfigs(configs: Option[String]): AbstractJSDep

  def %(configurations: Configuration): AbstractJSDep = %(configurations.name)
  def %(configurations: String): AbstractJSDep = {
    require(this.configurations.isEmpty,
        "Configurations already specified for jsModule " + this)
    require(configurations.trim.nonEmpty, "Configurations cannot be empty.")
    withConfigs(Some(configurations))
  }

}

/** A JavaScript module/library a Scala.js project may depend on */
sealed trait JSModuleID extends AbstractJSDep {
  def jsDep: JSDependency

  protected def withJSDep(jsDep: JSDependency): JSModuleID

  def commonJSName(name: String): JSModuleID =
    withJSDep(jsDep = jsDep.commonJSName(name))

  def dependsOn(names: String*): JSModuleID =
    withJSDep(jsDep = jsDep.dependsOn(names: _*))

  def minified(name: String): JSModuleID =
    withJSDep(jsDep = jsDep.minified(name))
}

/** A JavaScript module that resides inside a jar (probably webjar) */
final case class JarJSModuleID(
    module: ModuleID,
    jsDep: JSDependency) extends JSModuleID {

  def configurations: Option[String] = module.configurations

  protected def withConfigs(configs: Option[String]): JSModuleID =
    copy(module = moduleIDWithConfigurations(module, configs))

  protected def withJSDep(jsDep: JSDependency): JSModuleID =
    copy(jsDep = jsDep)
}

object JarJSModuleID {
  def apply(module: ModuleID, name: String): JarJSModuleID =
    JarJSModuleID(module, new JSDependency(name, Nil))
}

/** A JavaScript module that we depend on, but is provided externally or
 *  by the project itself */
final case class ProvidedJSModuleID(
    jsDep: JSDependency,
    configurations: Option[String]) extends JSModuleID {

  protected def withConfigs(configs: Option[String]): JSModuleID =
    copy(configurations = configs)
  protected def withJSDep(jsDep: JSDependency): JSModuleID =
    copy(jsDep = jsDep)
}

object ProvidedJSModuleID {
  def apply(name: String, configurations: Option[String]): ProvidedJSModuleID =
    ProvidedJSModuleID(new JSDependency(name, Nil), configurations)
}

/* If we mark RuntimeDOMDep as @deprecated, we cannot pattern-match on it in
 * our own implementation to implement its behavior. Therefore it is only
 * deprecated in the documentation. This shouldn't be an issue because it is
 * not advertised as a public API anyway. Instead, it is typically constructed
 * using the RuntimeDOM DSL in sbtplugin.impl.DependencyBuilders (which is
 * actually @deprecated).
 */
/** Deprecated.
 *
 *  For internal reasons, this class is not marked as `@deprecated`, but should
 *  be considered as such.
 */
final case class RuntimeDOMDep(
    configurations: Option[String]) extends AbstractJSDep {

  protected def withConfigs(configs: Option[String]): RuntimeDOMDep =
    copy(configurations = configs)
}
