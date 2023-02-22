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

package org.scalajs.linker.backend.javascript

private[javascript] object Utils {

  /* !!! BEGIN CODE VERY SIMILAR TO ir/.../Utils.scala and
   * js-envs/.../JSUtils.scala
   */

  private final val EscapeJSChars = "\\b\\t\\n\\v\\f\\r\\\"\\\\"

  def printEscapeJS(str: String, out: java.io.Writer): Int = {
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
        out.write(str, start, i - start)
        writtenChars += i
      }

      // Print next non ASCII printable character
      if (i != end) {
        def escapeJSEncoded(c: Int): Unit = {
          if (7 < c && c < 14) {
            val i = 2 * (c - 8)
            out.write(EscapeJSChars, i, 2)
            writtenChars += 2
          } else if (c == 34) {
            out.write(EscapeJSChars, 12, 2)
            writtenChars += 2
          } else if (c == 92) {
            out.write(EscapeJSChars, 14, 2)
            writtenChars += 2
          } else {
            out.write("\\u%04x".format(c))
            writtenChars += 6
          }
        }
        escapeJSEncoded(c)
        i += 1
      }
    }
    writtenChars
  }

  /* !!! END CODE VERY SIMILAR TO ir/.../Utils.scala and
   * js-envs/.../JSUtils.scala
   */

  private final val EscapeJSBytes = EscapeJSChars.toArray.map(_.toByte)

  def writeEscapeJS(str: String, out: java.io.OutputStream): Unit = {
    /* Note that Java and JavaScript happen to use the same encoding for
     * Unicode, namely UTF-16, which means that 1 char from Java always equals
     * 1 char in JavaScript. */

    val end = str.length()
    var i = 0

    while (i != end) {
      val c: Int = str.charAt(i)

      if (c >= 32 && c <= 126 && c != 34 && c != 92) {
        out.write(c)
      } else {
        def escapeJSEncoded(c: Int): Unit = {
          if (7 < c && c < 14) {
            val i = 2 * (c - 8)
            out.write(EscapeJSBytes, i, 2)
          } else if (c == 34) {
            out.write(EscapeJSBytes, 12, 2)
          } else if (c == 92) {
            out.write(EscapeJSBytes, 14, 2)
          } else {
            out.write('\\')
            out.write('u')

            def hexDigit(x: Int): Int =
              if (x < 10) x + '0' else x + ('a' - 10)

            out.write(hexDigit(c >> 12))
            out.write(hexDigit((c >> 8) & 0x0f))
            out.write(hexDigit((c >> 4) & 0x0f))
            out.write(hexDigit(c & 0x0f))
          }
        }
        escapeJSEncoded(c)
      }

      i += 1
    }
  }

}
