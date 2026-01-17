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

import java.io.IOException

class IRVersionNotSupportedException(val version: String,
    val supported: String, message: String)
    extends IOException(message) {

  def this(version: String, supported: String, message: String,
      exception: Exception) = {
    this(version, supported, message)
    initCause(exception)
  }
}
