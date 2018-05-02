package org.scalajs.testing.common

import sbt.testing._

private[testing] final class FrameworkInfo(
    val implName: String,
    val displayName: String,
    val fingerprints: List[Fingerprint])

private[testing] object FrameworkInfo {
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
