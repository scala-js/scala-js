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

import java.security.MessageDigest

/** Wrapper around java.security.MessageDigest.getInstance("SHA-1") */
object SHA1 {
  final class DigestBuilder {
    private val digest = MessageDigest.getInstance("SHA-1")

    def update(b: Byte): Unit =
      digest.update(b)

    def update(b: Array[Byte]): Unit =
      digest.update(b)

    def update(b: Array[Byte], off: Int, len: Int): Unit =
      digest.update(b, off, len)

    def updateUTF8String(str: UTF8String): Unit =
      update(str.bytes)

    def finalizeDigest(): Array[Byte] =
      digest.digest()
  }
}
