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

package org.scalajs.ir

import java.util.concurrent.ConcurrentHashMap

import scala.util.matching.Regex

object ScalaJSVersions extends VersionChecks(
    current = "1.0.0-SNAPSHOT",
    binaryEmitted = "1.0-SNAPSHOT"
)

/** Helper class to allow for testing of logic. */
class VersionChecks private[ir] (
    /** Scala.js version. */
    final val current: String,
    /** Version of binary IR emitted by this version of Scala.js. */
    final val binaryEmitted: String
) {
  import VersionChecks._

  checkConsistent(current, binaryEmitted)

  private val (binaryMajor, binaryMinor, binaryPreRelease) = parseBinary(binaryEmitted)

  /** The cross binary version
   *
   *  This is the version advertised in artifacts released by Scala.js users.
   *
   *  For non-pre release versions, this is only the major version, since binary
   *  minor versions are backwards compatible.
   *
   *  For pre-release versions, the story is a bit more complicated:
   *
   *  - Any pre-release version with a minor version == 0 is ''before'' the
   *    final major version is released and typically fully breaking. Therefore,
   *    the full [[binaryEmitted]] version is used.
   *  - A SNAPSHOT pre-release version with minor version > 0 is a development
   *    version with compatible binary version. Nothing should be published
   *    using this version, but being able to ''read'' artifacts in the major
   *    line is critical for fast testing. Therefore, only the major version is
   *    used.
   *  - A non-SNAPSHOT pre-release version with minor version > 0 is a milestone
   *    or release candidate version that is published. As such, artifacts may
   *    be published with it but will not be able to be read by the main line
   *    versions. Therefore, the full [[binaryEmitted]] version is used.
   */
  final val binaryCross: String = {
    val needsFull =
      binaryPreRelease.fold(false)(_ != "SNAPSHOT" || binaryMinor == 0)
    if (needsFull) binaryEmitted
    else binaryMajor.toString
  }

  private val knownSupportedBinary = {
    val m = new ConcurrentHashMap[String, Unit]()
    m.put(binaryEmitted, ())
    m
  }

  /** Check we can support this binary version (used by deserializer) */
  final def checkSupported(version: String): Unit = {
    if (!knownSupportedBinary.containsKey(version)) {
      val (major, minor, preRelease) = parseBinary(version)
      val supported = (
          // the exact pre-release version is supported via knownSupportedBinary
          preRelease.isEmpty &&
          major == binaryMajor &&
          minor <= binaryMinor &&
          (binaryPreRelease.isEmpty || minor < binaryMinor)
      )

      if (supported) {
        knownSupportedBinary.put(version, ())
      } else {
        throw new IRVersionNotSupportedException(version, binaryEmitted,
            s"This version ($version) of Scala.js IR is not supported. " +
            s"Supported versions are up to $binaryEmitted")
      }
    }
  }
}

private object VersionChecks {
  private val fullRE = """^([0-9]+)\.([0-9]+)\.([0-9]+)(-.*)?$""".r
  private val binaryRE = """^([0-9]+)\.([0-9]+)(-.*)?$""".r

  private def parseBinary(v: String): (Int, Int, Option[String]) = {
    val m = mustMatch(binaryRE, v)
    (m.group(1).toInt, m.group(2).toInt, preRelease(m.group(3)))
  }

  private def parseFull(v: String): (Int, Int, Int, Option[String]) = {
    val m = mustMatch(fullRE, v)
    (m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, preRelease(m.group(4)))
  }

  private def mustMatch(re: Regex, v: String): Regex.Match = {
    re.findFirstMatchIn(v).getOrElse(
        throw new IllegalArgumentException("malformed version: " + v))
  }

  private def preRelease(v: String): Option[String] =
    Option(v).map(_.stripPrefix("-"))

  private def checkConsistent(current: String, binary: String) = {
    val (binaryMajor, binaryMinor, binaryPreRelease) = parseBinary(binary)
    val (currentMajor, currentMinor, currentPatch, currentPreRelease) = parseFull(current)

    require(currentMajor == binaryMajor, "major(current) != major(binaryEmitted)")

    require(currentMinor >= binaryMinor, "minor(current) < minor(binaryEmitted)")

    require(
        currentPreRelease.isEmpty ||
        currentMinor > binaryMinor ||
        currentPatch > 0 ||
        binaryPreRelease == currentPreRelease,
        "current is older than binaryEmitted through pre-release")

    require(
        binaryPreRelease.isEmpty || (
            currentMinor == binaryMinor &&
            currentPatch == 0 &&
            binaryPreRelease == currentPreRelease),
        "binaryEmitted is in pre-release but does not match current")
  }
}
