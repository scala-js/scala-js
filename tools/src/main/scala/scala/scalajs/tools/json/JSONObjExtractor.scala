package scala.scalajs.tools.json

import scala.collection.mutable

class JSONObjExtractor(rawData: Object) {
  import JSONDeserializer._

  /** pseudo-deserializer to trick json api into not deserializing
    * field values */
  private object objectJSON extends IdentityDeserializer[Object]

  private val deserializer = mapJSON(stringJSON, objectJSON)
  private val data = fromJSON[Map[String,Object]](rawData)(deserializer)

  def fld[T : JSONDeserializer](name: String): T =
    fromJSON[T](data(name))

  def opt[T : JSONDeserializer](name: String): Option[T] =
    data.get(name).map(fromJSON[T] _)
}
