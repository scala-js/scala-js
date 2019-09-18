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

import java.nio.{ByteBuffer, CharBuffer}
import java.util.{Collections, HashSet, Arrays}
import java.util.ScalaOps._

import scala.scalajs.js

abstract class Charset protected (canonicalName: String,
    private val _aliases: Array[String])
    extends AnyRef with Comparable[Charset] {

  private lazy val aliasesSet =
    Collections.unmodifiableSet(new HashSet(Arrays.asList(_aliases)))

  final def name(): String = canonicalName

  final def aliases(): java.util.Set[String] = aliasesSet

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
    for (c <- js.Array(US_ASCII, ISO_8859_1, UTF_8, UTF_16BE, UTF_16LE, UTF_16)) {
      m(c.name.toLowerCase) = c
      val aliases = c._aliases
      for (i <- 0 until aliases.length)
        m(aliases(i).toLowerCase) = c
    }
    m
  }
}
