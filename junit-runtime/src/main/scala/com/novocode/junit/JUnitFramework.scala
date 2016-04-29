package com.novocode.junit

import org.scalajs.junit.{JUnitMasterRunner, JUnitSlaveRunner}
import sbt.testing._

final class JUnitFramework extends Framework {

  val name: String = "Scala.js JUnit test framework"

  private object JUnitFingerprint extends AnnotatedFingerprint {
    override def annotationName(): String = "org.junit.Test"

    override def isModule(): Boolean = false
  }

  def fingerprints(): Array[Fingerprint] = {
    Array(JUnitFingerprint)
  }

  def runner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader): JUnitMasterRunner = {
    new JUnitMasterRunner(args, remoteArgs, testClassLoader,
        parseRunSettings(args))
  }

  def slaveRunner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader, send: String => Unit): JUnitSlaveRunner = {
    new JUnitSlaveRunner(args, remoteArgs, testClassLoader, send,
        parseRunSettings(args))
  }

  def arrayString(arr: Array[String]): String = arr.mkString("Array(", ", ", ")")

  def parseRunSettings(args: Array[String]): RunSettings = {
    var quiet = false
    var verbose = false
    var noColor = false
    var decodeScalaNames = false
    var logAssert = false
    var notLogExceptionClass = false
    var ignoreRunners = "org.junit.runners.Suite"
    var runListener: String = null
    for (str <- args) {
      str match {
        case "-q" => quiet = true
        case "-v" => verbose = true
        case "-n" => noColor = true
        case "-s" => decodeScalaNames = true
        case "-a" => logAssert = true
        case "-c" => notLogExceptionClass = true

        case s if s.startsWith("-tests=") =>
          throw new UnsupportedOperationException("-tests")

        case s if s.startsWith("--tests=") =>
            throw new UnsupportedOperationException("--tests")

        case s if s.startsWith("--ignore-runners=") =>
          ignoreRunners = s.substring(17)

        case s if s.startsWith("--run-listener=") =>
          runListener = s.substring(15)

        case s if s.startsWith("--include-categories=") =>
            throw new UnsupportedOperationException("--include-categories")

        case s if s.startsWith("--exclude-categories=") =>
            throw new UnsupportedOperationException("--exclude-categories")

        case s if s.startsWith("-D") && s.contains("=") =>
            throw new UnsupportedOperationException("-Dkey=value")

        case s if !s.startsWith("-") && !s.startsWith("+") =>
            throw new UnsupportedOperationException(s)

        case _ =>
      }
    }
    for (s <- args) {
      s match {
        case "+q" => quiet = false
        case "+v" => verbose = false
        case "+n" => noColor = false
        case "+s" => decodeScalaNames = false
        case "+a" => logAssert = false
        case "+c" => notLogExceptionClass = false
        case _    =>
      }
    }
    new RunSettings(!noColor, decodeScalaNames, quiet, verbose, logAssert,
        ignoreRunners, notLogExceptionClass)
  }
}
