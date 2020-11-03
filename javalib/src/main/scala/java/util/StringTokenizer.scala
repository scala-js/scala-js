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

package java.util

class StringTokenizer(
    str: String, private var delim: String, returnDelims: Boolean
) extends java.util.Enumeration[Object] {

  def this(str: String) = this(str, " \t\n\r\f", false)
  def this(str: String, delim: String) = this(str, delim, false)

  private var position: Int = 0
  private val length: Int = str.length

  def hasMoreTokens(): Boolean = {
    position < length && (returnDelims || !remainingAreDelims())
  }

  def nextToken(): String = {
    @inline def nextIsDelim: Boolean = isDelim(currentChar)
    @inline def currentChar: Char = str.charAt(position)

    ensureAvailable()

    if (returnDelims && nextIsDelim) {
      val ret = String.valueOf(currentChar)
      position += 1
      ret
    } else {
      // Skip consecutive delims
      while (position < length && nextIsDelim)
        position += 1

      ensureAvailable()

      val start = position
      while (position < length && !nextIsDelim)
        position += 1
      str.substring(start, position)
    }
  }

  def nextToken(delim: String): String = {
    this.delim = delim
    nextToken()
  }

  def hasMoreElements(): Boolean = hasMoreTokens()

  def nextElement(): Object = nextToken()

  def countTokens(): Int = {
    var count = 0
    var inToken = false
    var i = position

    while (i < length) {
      if (isDelim(str.charAt(i))) {
        if (returnDelims)
          count += 1

        if (inToken) {
          count += 1
          inToken = false
        }
      } else {
        inToken = true
      }

      i += 1
    }

    if (inToken)
      count += 1

    count
  }

  private def ensureAvailable(): Unit = {
    if (position >= length)
      throw new NoSuchElementException()
  }

  @inline private def isDelim(ch: Char): Boolean = delim.indexOf(ch, 0) >= 0

  private def remainingAreDelims(): Boolean = {
    var i = position
    var restAreDelims = true

    while (i < length && restAreDelims) {
      if (!isDelim(str.charAt(i)))
        restAreDelims = false
      i += 1
    }

    restAreDelims
  }
}
