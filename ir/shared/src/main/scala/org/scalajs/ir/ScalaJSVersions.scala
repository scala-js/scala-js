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
    current = "1.20.0-SNAPSHOT",
    binaryEmitted = "1.20-SNAPSHOT"
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

  /** The cross binary version.
   *
   *  This is the version advertised in artifacts released by Scala.js users.
   *
   *  - For a pre-release version with a minor version == 0, it is the full
   *    [[binaryEmitted]]. Such a version is ''before'' the final major version
   *    is released, and as such any release is typically fully breaking.
   *  - For a non-pre-release, or the pre-release of a minor version, it is
   *    only the major version, since binary minor versions are backwards
   *    compatible.
   */
  final val binaryCross: String = {
    val needsFull = binaryPreRelease.isDefined && binaryMinor == 0
    if (needsFull) binaryEmitted
    else binaryMajor.toString
  }

  private val knownSupportedBinary = {
    val m = ConcurrentHashMap.newKeySet[String]()
    m.add(binaryEmitted)
    m
  }

  /** Check we can support this binary version (used by deserializer) */
  final def checkSupported(version: String): Unit = {
    if (!knownSupportedBinary.contains(version)) {
      val (major, minor, preRelease) = parseBinary(version)
      val supported = (
          // the exact pre-release version is supported via knownSupportedBinary
          preRelease.isEmpty &&
          major == binaryMajor &&
          minor <= binaryMinor &&
          (binaryPreRelease.isEmpty || minor < binaryMinor)
      )

      if (supported) {
        knownSupportedBinary.add(version)
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
