import sbt._
import Keys._
import Def.ScopedKey

import bintray.Plugin.bintrayPublishSettings
import bintray.Keys._

object PublishToBintray {

  private val bintrayPublishIvyStyle =
    settingKey[Boolean]("=== !publishMavenStyle")

  def publishToBintraySettings = (
      patchedBintrayPublishSettings
  ) ++ Seq(
      repository in bintray := "scala-js-releases",
      bintrayOrganization in bintray := Some("scala-js"),
      Def.derive(bintrayPublishIvyStyle := !publishMavenStyle.value)
  )

  /* work around https://github.com/softprops/bintray-sbt/issues/14 and
   * https://github.com/softprops/bintray-sbt/issues/15
   */
  private def patchedBintrayPublishSettings = {
    val f = new (ScopedKey ~> ScopedKey) {
      def apply[T](key: ScopedKey[T]) = {
        if (key.key == name.key) {
          ScopedKey(key.scope, moduleName.key.asInstanceOf[AttributeKey[T]])
        } else if (key.key == sbtPlugin.key) {
          ScopedKey(key.scope, bintrayPublishIvyStyle.key.asInstanceOf[AttributeKey[T]])
        } else {
          key
        }
      }
    }

    bintrayPublishSettings.map(_ mapKey f mapReferenced f)
  }
}
