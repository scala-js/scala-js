package org.scalajs.core.ir

object ScalaJSVersions {

  /* DO NOT MAKE THESE 'final val's!
   * When referring to these "constants" from separate libraries, if it is a
   * 'final val', the value will be copied in the binaries of those libraries.
   * If they are then linked to a different version of the IR artifact, their
   * copy of these constants will not be updated.
   */

  /** Scala.js version. */
  val current: String = "0.6.16"

  /** true iff the Scala.js version is a snapshot version. */
  val currentIsSnapshot: Boolean = current endsWith "-SNAPSHOT"

  /** Version of binary IR emitted by this version of Scala.js.
   *
   *  This should be either of:
   *  - a prior release version (i.e. "0.5.0", *not* "0.5.0-SNAPSHOT")
   *  - `current`
   */
  val binaryEmitted: String = "0.6.15"

  /** Versions whose binary files we can support (used by deserializer) */
  val binarySupported: Set[String] = {
    Set("0.6.0", "0.6.3", "0.6.4", "0.6.5", "0.6.6", "0.6.8", "0.6.13",
        "0.6.14", "0.6.15", binaryEmitted)
  }

  // Just to be extra safe
  assert(binarySupported contains binaryEmitted)

}
