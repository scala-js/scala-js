/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.niocharset

import java.nio.charset._

/** Standard charsets.
 *  This is basically the same as [[java.nio.charset.StandardCharsets]], but
 *  it is also available when compiling with a JDK 6.
 */
object StandardCharsets {
  import scala.scalajs.niocharset

  /** ISO-8859-1, aka latin1. */
  def ISO_8859_1: Charset = niocharset.ISO_8859_1

  /** US-ASCII. */
  def US_ASCII: Charset = niocharset.US_ASCII

  /** UTF-8. */
  def UTF_8: Charset = niocharset.UTF_8

  /** UTF-16 Big Endian without BOM. */
  def UTF_16BE: Charset = niocharset.UTF_16BE

  /** UTF-16 Little Endian without BOM. */
  def UTF_16LE: Charset = niocharset.UTF_16LE

  /** UTF-16 with an optional BOM.
   *  When encoding, Big Endian is always used.
   *  When decoding, the BOM specifies what endianness to use. If no BOM is
   *  found, it defaults to Big Endian.
   */
  def UTF_16: Charset = niocharset.UTF_16
}
