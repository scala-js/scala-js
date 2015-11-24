package org.scalajs.testsuite.utils

object Platform {
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
}
