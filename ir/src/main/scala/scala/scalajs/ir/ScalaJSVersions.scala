package scala.scalajs.ir

object ScalaJSVersions {

  /** the Scala.js version of this build */
  final val current = "0.5.0"

  /** true iff the Scala.js version of this build is a snapshot version. */
  final val currentIsSnapshot = current endsWith "-SNAPSHOT"

  /** Versions whose binary files we can support (used by deserializer) */
  val binarySupported: Set[String] = Set("0.5.0", current)

  // Just to be extra safe
  assert(binarySupported contains current)

}
