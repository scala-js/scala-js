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

import scala.util.Try

private[junit] final class RunSettings (
    val color: Boolean,
    decodeScalaNames: Boolean,
    val verbose: Boolean,
    val logAssert: Boolean,
    val notLogExceptionClass: Boolean
) {
  def decodeName(name: String): String = {
    if (decodeScalaNames) Try(scala.reflect.NameTransformer.decode(name)).getOrElse(name)
    else name
  }
}
