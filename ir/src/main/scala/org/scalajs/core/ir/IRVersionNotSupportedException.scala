package org.scalajs.core.ir

import java.io.IOException

class IRVersionNotSupportedException(val version: String,
    val supported: Set[String], message: String) extends IOException(message) {

  def this(version: String, supported: Set[String], message: String,
      exception: Exception) = {
    this(version, supported, message)
    initCause(exception)
  }
}
