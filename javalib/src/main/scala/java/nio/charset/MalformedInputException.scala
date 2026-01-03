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

class MalformedInputException(
    inputLength: Int)
    extends CharacterCodingException {
  def getInputLength(): Int = inputLength

  override def getMessage(): String =
    "Input length = " + inputLength
}
