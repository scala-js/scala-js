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
import java.util.{Collections, HashMap, HashSet, Arrays, Objects}
import java.util.ScalaOps._

import scala.scalajs.js
import scala.scalajs.LinkingInfo._

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

  override final def compareTo(that: Charset): Int = {
    /* Compare the names, ignoring case, as specified.
     * From https://docs.oracle.com/en/java/javase/25/docs/api/java.base/java/nio/charset/Charset.html#names
     * we know that names only contain ASCII characters. Therefore we use a
     * much simpler algorithm than the full case folding required by
     * String.compareToIgnoreCase.
     */
    _String.fromString(name()).asciiCompareToIgnoreCase(that.name())
  }

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
    theCharsetMap.get(charsetName)
  }

  def isSupported(charsetName: String): Boolean = {
    validateCharsetName(charsetName)
    theCharsetMap.contains(charsetName)
  }

  def availableCharsets(): java.util.SortedMap[String, Charset] =
    availableCharsetsResult

  private lazy val availableCharsetsResult: java.util.SortedMap[String, Charset] = {
    val m = new java.util.TreeMap[String, Charset](new java.util.Comparator[String] {
      def compare(o1: String, o2: String): Int =
        _String.fromString(o1).asciiCompareToIgnoreCase(o2)
    })

    val charsets = allSJSCharsets()
    for (i <- 0 until charsets.length) {
      val c = charsets(i)
      m.put(c.name(), c)
    }
    Collections.unmodifiableSortedMap(m)
  }

  private def allSJSCharsets(): Array[Charset] =
    Array(US_ASCII, ISO_8859_1, UTF_8, UTF_16BE, UTF_16LE, UTF_16)

  private abstract class CharsetMap {
    protected final def init(): Unit = {
      val charsets = allSJSCharsets()
      for (i <- 0 until charsets.length) {
        val c = charsets(i)
        put(c.name(), c)
        val aliases = c._aliases
        if (aliases != null) {
          for (i <- 0 until aliases.length)
            put(aliases(i), c)
        }
      }
    }

    protected def put(charsetName: String, c: Charset): Unit
    def get(charsetName: String): Charset // throws if does not exist
    def contains(charsetName: String): Boolean
  }

  @inline
  private def theCharsetMap: CharsetMap = {
    linkTimeIf[CharsetMap](moduleKind != ModuleKind.WasmModule) {
      JSCharsetMap
    } {
      WasmCharsetMap
    }
  }

  /** When we have JS interop, we use a raw js.Dictionary to minimize code size. */
  private object JSCharsetMap extends CharsetMap {
    private val dict: js.Dictionary[Charset] = dictEmpty()
    init()

    private def canonicalizeCharsetName(name: String): String =
      name.toLowerCase() // when we have JS, toLowerCase() is native and free for code size

    protected def put(charsetName: String, c: Charset): Unit =
      dictSet(dict, canonicalizeCharsetName(charsetName), c)

    def get(charsetName: String): Charset = {
      dictGetOrElse(dict, canonicalizeCharsetName(charsetName)) { () =>
        throw new UnsupportedCharsetException(charsetName)
      }
    }

    def contains(charsetName: String): Boolean =
      dictContains(dict, canonicalizeCharsetName(charsetName))
  }

  /** On Wasm-without-JS, we use a HashMap. */
  private object WasmCharsetMap extends CharsetMap {
    private val map: HashMap[String, Charset] = new HashMap[String, Charset]()
    init()

    private def canonicalizeCharsetName(name: String): String = {
      // Use a hand-made ASCII-only toLowerCase() not to reach the Unicode database
      var result = ""
      for (i <- 0 until name.length()) {
        val c = name.charAt(i)
        if (c >= 'A' && c <= 'Z')
          result += (c + 'a' - 'A').toChar
        else
          result += c
      }
      result
    }

    protected def put(charsetName: String, c: Charset): Unit =
      map.put(canonicalizeCharsetName(charsetName), c)

    def get(charsetName: String): Charset = {
      val result = map.get(canonicalizeCharsetName(charsetName))
      if (result == null)
        throw new UnsupportedCharsetException(charsetName)
      result
    }

    def contains(charsetName: String): Boolean =
      map.containsKey(canonicalizeCharsetName(charsetName))
  }
}
