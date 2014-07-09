package scala.scalajs.ir

object ScalaJSVersions {

  /** the Scala.js version of this build */
  final val current = "0.5.2"

  /** true iff the Scala.js version of this build is a snapshot version. */
  final val currentIsSnapshot = current endsWith "-SNAPSHOT"

  /** Version of binary IR this Scala.js version emits
   *
   *  This should be either of:
   *  - a prior release version (i.e. "0.5.0", *not* "0.5.0-SNAPSHOT")
   *  - `current`
   */
  final val binaryEmitted = current

  /** Versions whose binary files we can support (used by deserializer) */
  val binarySupported: Set[String] = Set("0.5.0", binaryEmitted)

  // Just to be extra safe
  assert(binarySupported contains binaryEmitted)

}
