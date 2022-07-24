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

package java.util.regex

import scala.scalajs.js
import scala.scalajs.runtime.linkingInfo
import scala.scalajs.LinkingInfo

class PatternSyntaxException(desc: String, regex: String, index: Int)
    extends IllegalArgumentException {

  def getIndex(): Int = index

  def getDescription(): String = desc

  def getPattern(): String = regex

  override def getMessage(): String = {
    // local copies, for code size
    val idx = index
    val re = regex

    val indexHint = if (idx < 0) "" else " near index " + idx
    val base = desc + indexHint + "\n" + re

    if (idx >= 0 && re != null && idx < re.length())
      base + "\n" + repeat(" ", idx) + "^"
    else
      base
  }

  @inline
  private def repeat(s: String, count: Int): String = {
    // TODO Use java.lang.String.repeat() once we can (JDK 11+ method)
    if (linkingInfo.esVersion >= LinkingInfo.ESVersion.ES2015) {
      s.asInstanceOf[js.Dynamic].repeat(count).asInstanceOf[String]
    } else {
      var result = ""
      var i = 0
      while (i != count) {
        result += s
        i += 1
      }
      result
    }
  }
}
