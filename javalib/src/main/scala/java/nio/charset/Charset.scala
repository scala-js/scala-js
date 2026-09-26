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

import scala.annotation.switch

import java.lang.Utils._
import java.nio.{ByteBuffer, CharBuffer}
import java.util.{Collections, HashSet, Arrays, Objects}
import java.util.ScalaOps._

import scala.scalajs.js

abstract class Charset protected (canonicalName: String,
    private val _aliases: Array[String])
    extends AnyRef with Comparable[Charset] {

  import Charset._

  {
    // Validate the names
    validateCharsetName(Objects.requireNonNull(canonicalName))
    if (_aliases != null) {
      for (i <- 0 until _aliases.length)
        validateCharsetName(Objects.requireNonNull(_aliases(i)))
    }
  }

  private lazy val aliasesSet: java.util.Set[String] =
    if (_aliases == null) Collections.emptySet()
    else Collections.unmodifiableSet(new HashSet(Arrays.asList(_aliases)))

  final def name(): String = canonicalName

  final def aliases(): java.util.Set[String] = aliasesSet

  override final def equals(that: Any): Boolean = that match {
    case that: Charset => this.name() == that.name()
    case _             => false
  }

  override final def toString(): String = name()

  override final def hashCode(): Int = name().hashCode()

  override final def compareTo(that: Charset): Int =
    name().compareToIgnoreCase(that.name())

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

  def displayName(): String = name()
}

object Charset {
  import StandardCharsets._

  def defaultCharset(): Charset =
    UTF_8

  private def validateCharsetName(charsetName: String): Unit = {
    if (charsetName == null)
      throw new IllegalArgumentException("Null charset name")

    def fail(): Nothing =
      throw new IllegalCharsetNameException(charsetName)

    @inline def isLetter(c: Char): scala.Boolean = {
      val lowerC = c | 0x20
      lowerC >= 'a' && lowerC <= 'z'
    }

    @inline def isDigit(c: Char): scala.Boolean =
      c >= '0' && c <= '9'

    val len = charsetName.length()
    if (len == 0)
      fail()

    val first = charsetName.charAt(0)
    if (!isLetter(first) && !isDigit(first))
      fail()

    var i = 1
    while (i != len) {
      val c = charsetName.charAt(i)
      if (!isLetter(c) && !isDigit(c)) {
        (c: @switch) match {
          case '-' | '+' | '.' | ':' | '_' =>
            () // ok
          case _ =>
            fail()
        }
      }
      i += 1
    }
  }

  def forName(charsetName: String): Charset = {
    validateCharsetName(charsetName)
    dictGetOrElse(CharsetMap, charsetName.toLowerCase()) { () =>
      throw new UnsupportedCharsetException(charsetName)
    }
  }

  def isSupported(charsetName: String): Boolean = {
    validateCharsetName(charsetName)
    dictContains(CharsetMap, charsetName.toLowerCase())
  }

  def availableCharsets(): java.util.SortedMap[String, Charset] =
    availableCharsetsResult

  private lazy val availableCharsetsResult = {
    val m = new java.util.TreeMap[String, Charset](String.CASE_INSENSITIVE_ORDER)
    forArrayElems(allSJSCharsets) { c =>
      m.put(c.name(), c)
    }
    Collections.unmodifiableSortedMap(m)
  }

  private lazy val CharsetMap = {
    val m = dictEmpty[Charset]()
    forArrayElems(allSJSCharsets) { c =>
      dictSet(m, c.name().toLowerCase(), c)
      val aliases = c._aliases
      if (aliases != null) {
        for (i <- 0 until aliases.length)
          dictSet(m, aliases(i).toLowerCase(), c)
      }
    }
    m
  }

  private def allSJSCharsets =
    js.Array(US_ASCII, ISO_8859_1, UTF_8, UTF_16BE, UTF_16LE, UTF_16)
}
