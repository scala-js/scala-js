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

package org.scalajs.junit

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
      testClassLoader: ClassLoader): Runner = {
    new JUnitRunner(args, remoteArgs, parseRunSettings(args))
  }

  def slaveRunner(args: Array[String], remoteArgs: Array[String],
      testClassLoader: ClassLoader, send: String => Unit): Runner = {
    new JUnitRunner(args, remoteArgs, parseRunSettings(args))
  }

  private def parseRunSettings(args: Array[String]): RunSettings = {
    var verbose = false
    var noColor = false
    var decodeScalaNames = false
    var logAssert = false
    var notLogExceptionClass = false
    for (str <- args) {
      str match {
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
          throw new UnsupportedOperationException("--ignore-runners")

        case s if s.startsWith("--run-listener=") =>
          throw new UnsupportedOperationException("--run-listener")

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
        case "+v" => verbose = false
        case "+n" => noColor = false
        case "+s" => decodeScalaNames = false
        case "+a" => logAssert = false
        case "+c" => notLogExceptionClass = false
        case _    =>
      }
    }
    new RunSettings(!noColor, decodeScalaNames, verbose, logAssert, notLogExceptionClass)
  }
}
