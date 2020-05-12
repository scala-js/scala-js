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

package org.scalajs.core.ir

object ScalaJSVersions {

  /* DO NOT MAKE THESE 'final val's!
   * When referring to these "constants" from separate libraries, if it is a
   * 'final val', the value will be copied in the binaries of those libraries.
   * If they are then linked to a different version of the IR artifact, their
   * copy of these constants will not be updated.
   */

  /** Scala.js version. */
  val current: String = "0.6.34-SNAPSHOT"

  /** true iff the Scala.js version is a snapshot version. */
  val currentIsSnapshot: Boolean = current endsWith "-SNAPSHOT"

  /** Version of binary IR emitted by this version of Scala.js.
   *
   *  This should be either of:
   *  - a prior release version (i.e. "0.5.0", *not* "0.5.0-SNAPSHOT")
   *  - a copy-paste of the *rhs* of `current` (*not* a reference to the val
   *    `current`, so that we notice a potential "-SNAPSHOT" suffix to be
   *    replaced on release, to avoid issues like #3865).
   */
  val binaryEmitted: String = "0.6.29"

  /** Versions whose binary files we can support (used by deserializer) */
  val binarySupported: Set[String] = {
    /* Note: 0.6.30 was never meant to be a valid IR version, but it Scala.js
     * v0.6.30 was published with accidentally advertising that it emits IR
     * version 0.6.30, while itself not being able to consume IR v0.6.29. We
     * therefore advertise that we support consuming IR v0.6.30 although we
     * emit v0.6.29.
     */
    Set("0.6.0", "0.6.3", "0.6.4", "0.6.5", "0.6.6", "0.6.8", "0.6.13",
        "0.6.14", "0.6.15", "0.6.17", "0.6.29", "0.6.30", binaryEmitted)
  }

  // Just to be extra safe
  assert(binarySupported contains binaryEmitted)

}
