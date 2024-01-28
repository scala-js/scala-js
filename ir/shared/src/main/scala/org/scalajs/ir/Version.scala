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

import java.util.Arrays
import java.io.OutputStream
import java.nio.ByteBuffer

/** A version of a thing
 *
 *  Versions are always optional, [[Version.Unversioned]] being the sentinel.
 *
 *  The remaining versions come in two fundamentally different flavors:
 *  - Hashes: They are stable in serialized form, [[Serializers]] will write
 *        them to IR files. The only way to create these versions is via
 *        [[Hashers]].
 *  - Non hashes: Not guaranteed to be stable / collision free across different
 *        programs. Never written to IR files.
 */
final class Version private (private val v: Array[Byte]) extends AnyVal {
  import Version.Type

  /** Checks whether two versions are known to be the same.
   *
   *  Returns false if either of the versions is [[Version.Unversioned]]
   */
  def sameVersion(that: Version): Boolean = {
    if (!this.isVersioned || !that.isVersioned) false
    else Arrays.equals(this.v, that.v)
  }

  private[ir] def isHash: Boolean = isVersioned && v(0) == Type.Hash

  private[ir] def writeHash(out: OutputStream): Unit = {
    require(isHash)
    out.write(v, 1, 20)
  }

  @inline
  private def isVersioned: Boolean = v != null

  // For debugging purposes
  override def toString(): String = {
    if (v == null) {
      "Unversioned"
    } else {
      val typeByte = v(0)
      val otherBytesStr = v.iterator.drop(1).map(b => "%02x".format(b & 0xff)).mkString
      s"Version($typeByte, $otherBytesStr)"
    }
  }
}

object Version {
  private object Type {
    val Hash: Byte = 0x00
    val Ephemeral: Byte = 0x02
    val Combined: Byte = 0x03
  }

  val Unversioned: Version = new Version(null)

  /** Create a non-hash version from the given bytes.
   *
   *  Guaranteed to differ from:
   *  - all hash versions.
   *  - versions returned from [[combine]].
   *  - versions with different bytes.
   */
  def fromBytes(bytes: Array[Byte]): Version =
    make(Type.Ephemeral, bytes)

  /** Create a non-hash version from a Byte.
   *
   *  Strictly equivalent to (but potentially more efficient):
   *  {{{
   *  fromBytes(Array[Byte](i))
   *  }}}
   */
  def fromByte(i: Byte): Version =
    new Version(Array(Type.Ephemeral, i))

  /** Create a non-hash version from an Int.
   *
   *  Strictly equivalent to (but potentially more efficient):
   *  {{{
   *  fromBytes(ByteBuffer.allocate(4).putInt(i).array())
   *  }}}
   */
  def fromInt(i: Int): Version = {
    val buf = ByteBuffer.allocate(5)
    buf.put(Type.Ephemeral)
    buf.putInt(i)
    new Version(buf.array())
  }

  /** Create a non-hash version from a Long.
   *
   *  Strictly equivalent to (but potentially more efficient):
   *  {{{
   *  fromBytes(ByteBuffer.allocate(8).putLong(i).array())
   *  }}}
   */
  def fromLong(l: Long): Version = {
    val buf = ByteBuffer.allocate(9)
    buf.put(Type.Ephemeral)
    buf.putLong(l)
    new Version(buf.array())
  }

  /** Create a non-hash version from the given [[UTF8String]].
   *
   *  Strictly equivalent to (but potentially more efficient):
   *  {{{
   *  fromBytes(Array.tabulate(utf8String.length)(utf8String(_)))
   *  }}}
   */
  def fromUTF8String(utf8String: UTF8String): Version =
    make(Type.Ephemeral, utf8String.bytes)

  /** Create a combined, non-hash version from the given bytes.
   *
   *  Returns [[Unversioned]] if at least one of versions is [[Unversioned]].
   *
   *  The returned version is to differ from:
   *  - all hash versions.
   *  - all non-hash versions created with `from` methods.
   *  - combined versions created with different (ordered) version lists
   *    (including the empty list).
   *
   *  @note This can be used to create tagged versions (for alternatives):
   *    {{{
   *    Versions.combine(Versions.fromInt(0), underlying)
   *    }}}
   */
  def combine(versions: Version*): Version = {
    if (versions.forall(_.isVersioned)) {
      val buf = ByteBuffer.allocate(1 + 4 + versions.map(_.v.length + 4).sum)

      buf.put(Type.Combined)
      buf.putInt(versions.length)

      for (version <- versions) {
        buf.putInt(version.v.length)
        buf.put(version.v)
      }

      new Version(buf.array())
    } else {
      Unversioned
    }
  }

  private[ir] def fromHash(hash: Array[Byte]): Version = {
    require(hash.length == 20)
    make(Type.Hash, hash)
  }

  private def make(tpe: Byte, bytes: Array[Byte]): Version = {
    val len = bytes.length
    val v = new Array[Byte](len + 1)
    v(0) = tpe

    System.arraycopy(bytes, 0, v, 1, len)
    new Version(v)
  }
}
