/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package scala.scalajs.testsuite.utils

/** This is a partial copy of the implementation in the testSuite */
object Platform {

  def executingInRhino: Boolean = sysProp("rhino")
  def typedArrays: Boolean = sysProp("typedarray")

  private def sysProp(key: String): Boolean =
    System.getProperty("scalajs." + key, "false") == "true"
}
