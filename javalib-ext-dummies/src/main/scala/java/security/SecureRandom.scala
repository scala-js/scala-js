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
 */
class SecureRandom extends java.util.Random
