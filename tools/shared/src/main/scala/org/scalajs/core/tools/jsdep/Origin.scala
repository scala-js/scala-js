package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.json._

/** The place a JSDependency originated from */
final class Origin(val moduleName: String, val configuration: String) {
  import Origin._

  override def toString(): String = s"$moduleName:$configuration"

  override def equals(that: Any): Boolean = that match {
    case that: Origin =>
      this.moduleName == that.moduleName &&
      this.configuration == that.configuration
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, moduleName.##)
    acc = mixLast(acc, configuration.##)
    finalizeHash(acc, 2)
  }
}

object Origin {
  // "org.scalajs.core.tools.jsdep.Origin".##
  private final val HashSeed = -2085327609

  implicit object OriginJSONSerializer extends JSONSerializer[Origin] {
    def serialize(x: Origin): JSON = {
      new JSONObjBuilder()
        .fld("moduleName",    x.moduleName)
        .fld("configuration", x.configuration)
        .toJSON
    }
  }

  implicit object OriginDeserializer extends JSONDeserializer[Origin] {
    def deserialize(x: JSON): Origin = {
      val obj = new JSONObjExtractor(x)
      new Origin(
          obj.fld[String]("moduleName"),
          obj.fld[String]("configuration"))
    }
  }
}
