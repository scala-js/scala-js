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

package org.scalajs.ir

import java.io.StringWriter

import scala.annotation.switch

private[ir] object Utils {

  // !!! BEGIN COPY-PASTED CODE with tools/.../JSUtils.scala

  private final val EscapeJSChars = "\\b\\t\\n\\v\\f\\r\\\"\\\\"

  private[ir] def printEscapeJS(str: String, out: java.lang.Appendable): Int = {
    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript. */
    val end = str.length()
    var i = 0
    var writtenChars = 0
    /* Loop prints all consecutive ASCII printable characters starting
     * from current i and one non ASCII printable character (if it exists).
     * The new i is set at the end of the appended characters.
     */
    while (i != end) {
      val start = i
      var c: Int = str.charAt(i)
      // Find all consecutive ASCII printable characters from `start`
      while (i != end && c >= 32 && c <= 126 && c != 34 && c != 92) {
        i += 1
        if (i != end)
          c = str.charAt(i)
      }
      // Print ASCII printable characters from `start`
      if (start != i) {
        out.append(str, start, i)
        writtenChars += i
      }

      // Print next non ASCII printable character
      if (i != end) {
        def escapeJSEncoded(c: Int): Unit = {
          if (7 < c && c < 14) {
            val i = 2 * (c - 8)
            out.append(EscapeJSChars, i, i + 2)
            writtenChars += 2
          } else if (c == 34) {
            out.append(EscapeJSChars, 12, 14)
            writtenChars += 2
          } else if (c == 92) {
            out.append(EscapeJSChars, 14, 16)
            writtenChars += 2
          } else {
            out.append("\\u%04x".format(c))
            writtenChars += 6
          }
        }
        escapeJSEncoded(c)
        i += 1
      }
    }
    writtenChars
  }

  // !!! END COPY-PASTED CODE with tools/.../JSUtils.scala

  /** A ByteArrayOutput stream that allows to jump back to a given
   *  position and complete some bytes. Methods must be called in the
   *  following order only:
   *  - [[markJump]]
   *  - [[jumpBack]]
   *  - [[continue]]
   */
  private[ir] class JumpBackByteArrayOutputStream
      extends java.io.ByteArrayOutputStream {
    protected var jumpBackPos: Int = -1
    protected var headPos: Int = -1

    /** Marks the current location for a jumpback */
    def markJump(): Unit = {
      assert(jumpBackPos == -1)
      assert(headPos == -1)
      jumpBackPos = count
    }

    /** Jumps back to the mark. Returns the number of bytes jumped */
    def jumpBack(): Int = {
      assert(jumpBackPos >= 0)
      assert(headPos == -1)
      val jumped = count - jumpBackPos
      headPos = count
      count = jumpBackPos
      jumpBackPos = -1
      jumped
    }

    /** Continues to write at the head. */
    def continue(): Unit = {
      assert(jumpBackPos == -1)
      assert(headPos >= 0)
      count = headPos
      headPos = -1
    }
  }

}
