package scala.scalajs.sbtplugin

import sbt._

import StringUtilities.nonEmpty

import scala.scalajs.tools.jsdep.JSDependency

/** A JavaScript module/library a Scala.js project may depend on */
sealed trait JSModuleID {
  def jsDep: JSDependency
  def configurations: Option[String]

  protected def withConfigs(configs: Option[String]): JSModuleID
  protected def withJSDep(jsDep: JSDependency): JSModuleID

  def %(configurations: Configuration): JSModuleID = %(configurations.name)
  def %(configurations: String): JSModuleID = {
    require(this.configurations.isEmpty,
        "Configurations already specified for jsModule " + this)
    nonEmpty(configurations, "Configurations")
    withConfigs(Some(configurations))
  }

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
