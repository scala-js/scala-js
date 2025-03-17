/*
 * Written by Doug Lea with assistance from members of JCP JSR-166
 * Expert Group and released to the public domain, as explained at
 * http://creativecommons.org/publicdomain/zero/1.0/
 *
 * and translated to Scala
 */

package java.util.concurrent

import java.util.Random

class ThreadLocalRandom extends Random {

  private var initialized: Boolean = _
  initialized = true

  override def setSeed(seed: Long): Unit = {
    if (initialized)
      throw new UnsupportedOperationException()

    super.setSeed(seed)
  }
}

object ThreadLocalRandom {

  private val _current =
    new ThreadLocalRandom()

  def current(): ThreadLocalRandom = _current

}
