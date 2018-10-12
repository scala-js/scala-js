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

package java.nio.charset

class CodingErrorAction private (name: String) {
  override def toString(): String = name
}

object CodingErrorAction {
  val IGNORE = new CodingErrorAction("IGNORE")
  val REPLACE = new CodingErrorAction("REPLACE")
  val REPORT = new CodingErrorAction("REPORT")
}
