package scala.scalajs.tools

/** Some type-class lightweight wrappers around simple-json.
 *
 *  They allow to write [[xyz.toJSON]] to obtain classes that can be
 *  serialized by simple-json and [[fromJSON[T](xyz)]] to get an
 *  object back.
 */
package object json {
  implicit class JSONPimp[T : JSONSerializer](x: T) {
    def toJSON = implicitly[JSONSerializer[T]].serialize(x)
  }

  def fromJSON[T : JSONDeserializer](x: Object): T =
    implicitly[JSONDeserializer[T]].deserialize(x)
}
