package org.scalajs.core.tools

import java.io.{Reader, Writer}

/** Some type-class lightweight wrappers around simple-json.
 *
 *  They allow to write `xyz.toJSON` to obtain classes that can be
 *  serialized by simple-json and `fromJSON[T](xyz)` to get an
 *  object back.
 */
package object json {
  type JSON = Impl.Repr

  implicit class JSONPimp[T : JSONSerializer](x: T) {
    def toJSON: JSON = implicitly[JSONSerializer[T]].serialize(x)
  }

  def fromJSON[T](x: JSON)(implicit d: JSONDeserializer[T]): T =
    d.deserialize(x)

  def writeJSON(x: JSON, writer: Writer): Unit = Impl.serialize(x, writer)
  def jsonToString(x: JSON): String = Impl.serialize(x)
  def readJSON(str: String): JSON = Impl.deserialize(str)
  def readJSON(reader: Reader): JSON = Impl.deserialize(reader)

}
