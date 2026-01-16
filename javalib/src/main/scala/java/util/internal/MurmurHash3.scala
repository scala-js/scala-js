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

package java.util.internal

import java.lang.Integer.{rotateLeft => rotl}

/** Primitives to implement MurmurHash3 hashes in data structures.
 *
 *  This is copy of parts of `scala.util.hashing.MurmurHash3`.
 */
private[java] object MurmurHash3 {

  /** Mix in a block of data into an intermediate hash value. */
  final def mix(hash: Int, data: Int): Int = {
    var h = mixLast(hash, data)
    h = rotl(h, 13)
    h * 5 + 0xe6546b64
  }

  /** May optionally be used as the last mixing step.
   *
   *  Is a little bit faster than mix, as it does no further mixing of the
   *  resulting hash. For the last element this is not necessary as the hash is
   *  thoroughly mixed during finalization anyway.
   */
  final def mixLast(hash: Int, data: Int): Int = {
    var k = data

    k *= 0xcc9e2d51
    k = rotl(k, 15)
    k *= 0x1b873593

    hash ^ k
  }

  /** Finalize a hash to incorporate the length and make sure all bits avalanche. */
  @noinline final def finalizeHash(hash: Int, length: Int): Int =
    avalanche(hash ^ length)

  /** Force all bits of the hash to avalanche. Used for finalizing the hash. */
  @inline private final def avalanche(hash: Int): Int = {
    var h = hash

    h ^= h >>> 16
    h *= 0x85ebca6b
    h ^= h >>> 13
    h *= 0xc2b2ae35
    h ^= h >>> 16

    h
  }
}
