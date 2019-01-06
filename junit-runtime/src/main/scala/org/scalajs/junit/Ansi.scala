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

package org.scalajs.junit

private[junit] object Ansi {

  private[this] final val NORMAL = "\u001B[0m"

  def c(s: String, colorSequence: String): String =
    if (colorSequence == null) s
    else colorSequence + s + NORMAL

  def filterAnsi(s: String): String = {
    if (s == null) {
      null
    } else {
      var r: String = ""
      val len = s.length
      var i = 0
      while (i < len) {
        val c = s.charAt(i)
        if (c == '\u001B') {
          i += 1
          while (i < len && s.charAt(i) != 'm')
            i += 1
        } else {
          r += c
        }
        i += 1
      }
      r
    }
  }

  final val RED = "\u001B[31m"
  final val YELLOW = "\u001B[33m"
  final val BLUE = "\u001B[34m"
  final val MAGENTA = "\u001B[35m"
  final val CYAN = "\u001B[36m"
}
