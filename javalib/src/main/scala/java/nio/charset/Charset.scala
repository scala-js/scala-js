package java.nio.charset

import java.nio.{ByteBuffer, CharBuffer}

import scala.scalajs.js

abstract class Charset protected (canonicalName: String,
    aliases: Array[String]) extends AnyRef with Comparable[Charset] {
  final def name(): String = canonicalName

  override final def equals(that: Any): Boolean = that match {
    case that: Charset => this.name == that.name
    case _             => false
  }

  override final def toString(): String = name()

  override final def hashCode(): Int = name.##

  override final def compareTo(that: Charset): Int =
    name.compareToIgnoreCase(that.name)

  def contains(cs: Charset): Boolean

  def newDecoder(): CharsetDecoder
  def newEncoder(): CharsetEncoder

  def canEncode(): Boolean = true

  private lazy val cachedDecoder = {
    this.newDecoder()
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)
  }

  private lazy val cachedEncoder = {
    this.newEncoder()
        .onMalformedInput(CodingErrorAction.REPLACE)
        .onUnmappableCharacter(CodingErrorAction.REPLACE)
  }

  final def decode(bb: ByteBuffer): CharBuffer =
    cachedDecoder.decode(bb)

  final def encode(cb: CharBuffer): ByteBuffer =
    cachedEncoder.encode(cb)

  final def encode(str: String): ByteBuffer =
    encode(CharBuffer.wrap(str))

  def displayName(): String = name
}

object Charset {
  import StandardCharsets._

  def defaultCharset(): Charset =
    UTF_8

  def forName(charsetName: String): Charset =
    CharsetMap.getOrElse(charsetName.toLowerCase,
        throw new UnsupportedCharsetException(charsetName))

  def isSupported(charsetName: String): Boolean =
    CharsetMap.contains(charsetName.toLowerCase)

  private lazy val CharsetMap = {
    val m = js.Dictionary.empty[Charset]

    // All these lists where obtained by experimentation on the JDK

    for (s <- Seq("iso-8859-1", "iso8859-1", "iso_8859_1", "iso8859_1",
        "iso_8859-1", "8859_1", "iso_8859-1:1987",
        "latin1", "csisolatin1", "l1",
        "ibm-819", "ibm819", "cp819", "819",
        "iso-ir-100"))
      m(s) = ISO_8859_1

    for (s <- Seq("us-ascii", "ascii7", "ascii", "csascii",
        "default",
        "cp367", "ibm367",
        "iso646-us", "646", "iso_646.irv:1983", "iso_646.irv:1991",
        "ansi_x3.4-1986", "ansi_x3.4-1968",
        "iso-ir-6"))
      m(s) = US_ASCII

    for (s <- Seq("utf-8", "utf8", "unicode-1-1-utf-8"))
      m(s) = UTF_8

    for (s <- Seq("utf-16be", "utf_16be", "x-utf-16be",
        "iso-10646-ucs-2", "unicodebigunmarked"))
      m(s) = UTF_16BE

    for (s <- Seq("utf-16le", "utf_16le", "x-utf-16le",
        "unicodelittleunmarked"))
      m(s) = UTF_16LE

    for (s <- Seq("utf-16", "utf_16", "unicode", "unicodebig"))
      m(s) = UTF_16

    m
  }
}
