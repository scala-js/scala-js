package org.scalajs.testsuite.utils

object Platform {

  def scalaVersion: String =
    scala.util.Properties.versionNumberString

  /** Returns `true` if and only if the code is executing on a JVM.
   *  Note: Returns `false` when executing on any JS VM.
   */
  final val executingInJVM = true

  def executingInJVMOnJDK6: Boolean = jdkVersion == 6

  def executingInJVMOnJDK7OrLower: Boolean = jdkVersion <= 7

  private lazy val jdkVersion = {
    val v = System.getProperty("java.version")
    if (v.startsWith("1.")) Integer.parseInt(v.drop(2).takeWhile(_.isDigit))
    else throw new Exception("Unknown java.version format")
  }

  def executingInRhino: Boolean = false
  def executingInNodeJS: Boolean = false
  def executingInNodeJSOnJSDOM: Boolean = false
  def executingInPhantomJS: Boolean = false
  def typedArrays: Boolean = false
  def sourceMaps: Boolean = false

  def isInFastOpt: Boolean = false
  def isInFullOpt: Boolean = false
  def isInProductionMode: Boolean = false
  def isInDevelopmentMode: Boolean = true

  def hasCompliantAsInstanceOfs: Boolean = true
  def hasCompliantArrayIndexOutOfBounds: Boolean = true
  def hasCompliantModule: Boolean = true
  def hasStrictFloats: Boolean = true
}
