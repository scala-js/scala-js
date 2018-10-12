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

final class StandardCharsets private ()

object StandardCharsets {
  import scala.scalajs.niocharset.{StandardCharsets => SC}

  def ISO_8859_1: Charset = SC.ISO_8859_1
  def US_ASCII: Charset = SC.US_ASCII
  def UTF_8: Charset = SC.UTF_8
  def UTF_16BE: Charset = SC.UTF_16BE
  def UTF_16LE: Charset = SC.UTF_16LE
  def UTF_16: Charset = SC.UTF_16
}
