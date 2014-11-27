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
      testClassLoader: ClassLoader): MasterRunner =
    new MasterRunner(args, remoteArgs, testClassLoader)

  def slaveRunner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader, send: String => Unit): SlaveRunner =
    new SlaveRunner(args, remoteArgs, testClassLoader, send)

}
