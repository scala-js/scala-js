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

import scala.annotation.switch

import java.io.UnsupportedEncodingException
import java.nio.{CharBuffer, ByteBuffer}
import java.nio.charset.{Charset, CharsetDecoder}

import java.util.ScalaOps._
import java.nio.charset.CodingErrorAction

object URLEncoder {
  private final val EncodeAsIsLength = 128

  private val EncodedAsIs: Array[Boolean] = {
    val r = new Array[Boolean](EncodeAsIsLength) // initialized with false
    r('.') = true
    r('-') = true
    r('*') = true
    r('_') = true
    for (c <- '0'.toInt to '9'.toInt)
      r(c) = true
    for (c <- 'A'.toInt to 'Z'.toInt)
      r(c) = true
    for (c <- 'a'.toInt to 'z'.toInt)
      r(c) = true
    r
  }

  private val PercentEncoded: Array[String] = {
    val hexDigits = "0123456789ABCDEF"
    val r = new Array[String](256)
    for (b <- 0 until 256)
      r(b) = "%" + hexDigits.charAt(b >>> 4) + hexDigits.charAt(b & 0xf)
    r
  }

  @Deprecated
  def encode(s: String): String = encode(s, Charset.defaultCharset())

  def encode(s: String, enc: String): String = {
    if (!Charset.isSupported(enc))
      throw new UnsupportedEncodingException(enc)
    encode(s, Charset.forName(enc))
  }

  def encode(s: String, charset: Charset): String = {
    val EncodedAsIs = this.EncodedAsIs // local copy

    @inline def encodeAsIs(c: Char): Boolean =
      c < EncodeAsIsLength && EncodedAsIs(c)

    @inline def encodeUsingCharset(c: Char): Boolean =
      c != ' ' && !encodeAsIs(c)

    var len = s.length()
    var i = 0

    while (i != len && encodeAsIs(s.charAt(i)))
      i += 1

    if (i == len) {
      s
    } else {
      val PercentEncoded = this.PercentEncoded // local copy

      val charBuffer = CharBuffer.wrap(s)
      val encoder = charset.newEncoder().onUnmappableCharacter(CodingErrorAction.REPLACE)
      val bufferArray = new Array[Byte](((len - i + 1) * encoder.maxBytesPerChar()).toInt)
      val buffer = ByteBuffer.wrap(bufferArray)

      var result = s.substring(0, i)

      while (i != len) {
        val startOfChunk = i
        val firstChar = s.charAt(startOfChunk)
        i += 1

        if (encodeAsIs(firstChar)) {
          // A chunk of characters encoded as is
          while (i != len && encodeAsIs(s.charAt(i)))
            i += 1
          result += s.substring(startOfChunk, i)
        } else if (firstChar == ' ') {
          // A single ' '
          result += "+"
        } else {
          /* A chunk of characters to encode using the charset.
           *
           * Encoding as big a chunk as possible is not only good for
           * performance. It allows us to deal with surrogate pairs without
           * additional logic.
           */
          while (i != len && encodeUsingCharset(s.charAt(i)))
            i += 1
          charBuffer.limit(i) // must be done before setting position
          charBuffer.position(startOfChunk)
          buffer.rewind()
          encoder.reset().encode(charBuffer, buffer, true)
          for (j <- 0 until buffer.position())
            result += PercentEncoded(bufferArray(j) & 0xff)
        }
      }

      result
    }
  }

}
