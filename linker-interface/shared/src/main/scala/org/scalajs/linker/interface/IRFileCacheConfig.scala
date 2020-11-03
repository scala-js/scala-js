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

package org.scalajs.linker.interface

/** Configuration of an IRFileCache. */
final class IRFileCacheConfig private (
    /** The maximum number of (file) reads executed concurrently. */
    val maxConcurrentReads: Int
) {
  private def this() = {
    this(
        maxConcurrentReads = 50
    )
  }

  def withMaxConcurrentReads(maxConcurrentReads: Int): IRFileCacheConfig =
    copy(maxConcurrentReads = maxConcurrentReads)

  override def toString(): String = {
    s"""IRFileCacheConfig(
       |  maxConcurrentReads = $maxConcurrentReads,
       |)""".stripMargin
  }

  private def copy(
      maxConcurrentReads: Int = maxConcurrentReads
  ): IRFileCacheConfig = {
    new IRFileCacheConfig(
        maxConcurrentReads
    )
  }
}

object IRFileCacheConfig {

  /** Returns the default [[IRFileCacheConfig]].
   *
   *  The defaults are:
   *
   *  - `maxConcurrentReads`: 50
   */
  def apply(): IRFileCacheConfig = new IRFileCacheConfig()
}
