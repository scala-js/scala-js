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

package java.io

class IOException(message: String) extends Exception(message) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class EOFException(s: String) extends IOException(s) {
  def this() = this(null)
}

class UTFDataFormatException(s: String) extends IOException(s) {
  def this() = this(null)
}

class UnsupportedEncodingException(s: String) extends IOException(s) {
  def this() = this(null)
}

abstract class ObjectStreamException protected (s: String) extends IOException(s) {
  protected def this() = this(null)
}

class NotSerializableException(s: String) extends ObjectStreamException(s) {
  def this() = this(null)
}
