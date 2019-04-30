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

package scala.scalajs.niocharset

import java.nio.charset.{Charset, StandardCharsets => JStdCharsets}

/** Standard charsets. */
@deprecated("Use java.nio.charset.StandardCharsets instead.", "0.6.28")
object StandardCharsets {
  /** ISO-8859-1, aka latin1. */
  def ISO_8859_1: Charset = JStdCharsets.ISO_8859_1

  /** US-ASCII. */
  def US_ASCII: Charset = JStdCharsets.US_ASCII

  /** UTF-8. */
  def UTF_8: Charset = JStdCharsets.UTF_8

  /** UTF-16 Big Endian without BOM. */
  def UTF_16BE: Charset = JStdCharsets.UTF_16BE

  /** UTF-16 Little Endian without BOM. */
  def UTF_16LE: Charset = JStdCharsets.UTF_16LE

  /** UTF-16 with an optional BOM.
   *  When encoding, Big Endian is always used.
   *  When decoding, the BOM specifies what endianness to use. If no BOM is
   *  found, it defaults to Big Endian.
   */
  def UTF_16: Charset = JStdCharsets.UTF_16
}
