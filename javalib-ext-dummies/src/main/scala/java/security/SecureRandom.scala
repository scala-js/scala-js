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

package java.security

/** Fake implementation of `SecureRandom` that is not actually secure at all.
 *
 *  It directly delegates to `java.util.Random`.
 *
 *  Since the class is fake anyway, we use a deterministic seed so that we can
 *  run the related tests in Wasm-without-JS.
 */
class SecureRandom extends java.util.Random(SecureRandom.nextSeed())

object SecureRandom {
  private val seeder = new java.util.SplittableRandom(6567830523310966691L)

  private def nextSeed(): Long = seeder.nextLong()
}
