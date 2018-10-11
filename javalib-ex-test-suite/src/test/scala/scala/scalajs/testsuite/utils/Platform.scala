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

package scala.scalajs.testsuite.utils

/** This is a partial copy of the implementation in the testSuite */
object Platform {

  def executingInRhino: Boolean = sysProp("rhino")
  def typedArrays: Boolean = sysProp("typedarray")

  private def sysProp(key: String): Boolean =
    System.getProperty("scalajs." + key, "false") == "true"
}
