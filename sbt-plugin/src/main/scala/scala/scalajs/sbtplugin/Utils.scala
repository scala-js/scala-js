package scala.scalajs.sbtplugin

import sbt._

object Utils {
  def changeExt(f: File, oldExt: String, newExt: String): File =
    file(f.getPath.stripSuffix(oldExt) + newExt)
}
