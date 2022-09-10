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

package java.net

import scala.scalajs.js

import java.io.UnsupportedEncodingException
import java.nio.{CharBuffer, ByteBuffer}
import java.nio.charset.{Charset, CharsetDecoder}

object URLDecoder {

  @Deprecated
  def decode(s: String): String = decodeImpl(s, Charset.defaultCharset())

  def decode(s: String, enc: String): String = {
    if (!Charset.isSupported(enc))
      throw new UnsupportedEncodingException(enc)
    decodeImpl(s, Charset.forName(enc))
  }

  private def decodeImpl(s: String, charset: Charset): String = {
    val len = s.length
    val charBuffer = CharBuffer.allocate(len)

    // For charset-based decoding
    var decoder: CharsetDecoder = null
    var byteBuffer: ByteBuffer = null

    def throwIllegalHex() = {
      throw new IllegalArgumentException(
          "URLDecoder: Illegal hex characters in escape (%) pattern")
    }

    var i = 0
    while (i < len) {
      s.charAt(i) match {
        case '+' =>
          charBuffer.append(' ')
          i += 1

        case '%' if i + 3 > len =>
          throwIllegalHex()

        case '%' =>
          if (decoder == null) { // equivalent to `byteBuffer == null`
            decoder = charset.newDecoder()
            byteBuffer = ByteBuffer.allocate(len / 3)
          } else {
            byteBuffer.clear()
            decoder.reset()
          }

          while (i + 3 <= len && s.charAt(i) == '%') {
            val c1 = Character.digit(s.charAt(i + 1), 16)
            val c2 = Character.digit(s.charAt(i + 2), 16)

            if (c1 < 0 || c2 < 0)
              throwIllegalHex()

            byteBuffer.put(((c1 << 4) + c2).toByte)
            i += 3
          }

          byteBuffer.flip()
          val decodeResult = decoder.decode(byteBuffer, charBuffer, true)
          val flushResult = decoder.flush(charBuffer)

          if (decodeResult.isError() || flushResult.isError())
            throwIllegalHex()

      case c =>
        charBuffer.append(c)
        i += 1
      }
    }

    charBuffer.flip()
    charBuffer.toString
  }
}
