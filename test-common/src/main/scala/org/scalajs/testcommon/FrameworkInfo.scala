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

package org.scalajs.testcommon

import sbt.testing._

private[scalajs] final class FrameworkInfo(
    val implName: String,
    val displayName: String,
    val fingerprints: List[Fingerprint])

private[scalajs] object FrameworkInfo {
  implicit object FrameworkInfoSerializer extends Serializer[FrameworkInfo] {
    def serialize(x: FrameworkInfo, out: Serializer.SerializeState): Unit = {
      out.write(x.implName)
      out.write(x.displayName)
      out.write(x.fingerprints)
    }

    def deserialize(in: Serializer.DeserializeState): FrameworkInfo = {
      new FrameworkInfo(in.read[String](),
          in.read[String](), in.read[List[Fingerprint]]())
    }
  }
}
