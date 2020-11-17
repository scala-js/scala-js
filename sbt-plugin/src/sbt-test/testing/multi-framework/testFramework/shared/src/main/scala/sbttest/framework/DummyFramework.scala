package sbttest.framework

import sbt.testing._

final class DummyFramework extends Framework {

  val name: String = "Dummy cross JVM/JS test framework"

  private object DummyFingerprint extends SubclassFingerprint {
    val isModule: Boolean = false
    val superclassName: String = "sbttest.framework.Test"
    val requireNoArgConstructor: Boolean = true
  }

  def fingerprints: Array[Fingerprint] = Array(DummyFingerprint)

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): ControllerRunner =
    new ControllerRunner(args, remoteArgs, testClassLoader)

  // Aka `workerRunner`; see the Scaladoc of `sbt.testing.Framework` about the name.
  def slaveRunner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader, send: String => Unit): WorkerRunner =
    new WorkerRunner(args, remoteArgs, testClassLoader, send)

}
