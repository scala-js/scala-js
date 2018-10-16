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

import java.nio.charset

final class StandardCharsets private ()

object StandardCharsets {
  def US_ASCII: Charset = charset.US_ASCII
  def ISO_8859_1: Charset = charset.ISO_8859_1
  def UTF_8: Charset = charset.UTF_8
  def UTF_16BE: Charset = charset.UTF_16BE
  def UTF_16LE: Charset = charset.UTF_16LE
  def UTF_16: Charset = charset.UTF_16
}
