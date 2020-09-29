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

package java.text

import java.util.Locale

abstract class NumberFormat protected () extends Format

object NumberFormat {
  def getNumberInstance(inLocale: Locale): NumberFormat =
    new DecimalFormat(inLocale)
}
