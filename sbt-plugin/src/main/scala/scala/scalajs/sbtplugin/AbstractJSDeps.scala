package scala.scalajs.sbtplugin

import sbt._

import StringUtilities.nonEmpty

import scala.scalajs.tools.jsdep.JSDependency

/** Something JavaScript related a project may depend on. Either a JavaScript
 *  module/library, or the DOM at runtime. */
sealed trait AbstractJSDep {
  def configurations: Option[String]

  protected def withConfigs(configs: Option[String]): AbstractJSDep

  def %(configurations: Configuration): AbstractJSDep = %(configurations.name)
  def %(configurations: String): AbstractJSDep = {
    require(this.configurations.isEmpty,
        "Configurations already specified for jsModule " + this)
    nonEmpty(configurations, "Configurations")
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
}

/** A JavaScript module that resides inside a jar (probably webjar) */
final case class JarJSModuleID(
    module: ModuleID,
    jsDep: JSDependency) extends JSModuleID {

  def configurations: Option[String] = module.configurations

  protected def withConfigs(configs: Option[String]): JSModuleID =
    copy(module = module.copy(configurations = configs))
  protected def withJSDep(jsDep: JSDependency): JSModuleID =
    copy(jsDep = jsDep)
}

object JarJSModuleID {
  def apply(module: ModuleID, name: String): JarJSModuleID =
    JarJSModuleID(module, JSDependency(name, Nil))
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
    ProvidedJSModuleID(JSDependency(name, Nil), configurations)
}

sealed case class RuntimeDOM(
    configurations: Option[String]) extends AbstractJSDep {

  protected def withConfigs(configs: Option[String]): RuntimeDOM =
    copy(configurations = configs)
}

object RuntimeDOM extends RuntimeDOM(None)
