package scala.scalajs.sbtplugin

import sbt._

object Utils {
  def changeExt(f: File, oldExt: String, newExt: String): File =
    file(f.getPath.stripSuffix(oldExt) + newExt)

  object ScalaJSClassFile {
    /** Returns the .sjsinfo file when matches. */
    def unapply(f: File): Option[File] = {
      if (!f.getPath.endsWith(".js")) None
      else {
        val infoFile = changeExt(f, ".js", ".sjsinfo")
        if (!infoFile.exists) None
        else Some(infoFile)
      }
    }
  }

  def isScalaJSClassFile(f: File): Boolean =
    ScalaJSClassFile.unapply(f).isDefined

  def isCoreJSLibFile(f: File): Boolean =
    f.name == "scalajs-corejslib.js"

  object CoreJSLibFile {
    def unapply(f: File): Boolean = isCoreJSLibFile(f)
  }
}
