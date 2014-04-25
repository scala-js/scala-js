package scala.scalajs.tools.json

trait JSONDeserializer[T] {
  def deserialize(x: Object): T
}

object JSONDeserializer {
  import scala.collection.JavaConverters._

  trait IdentityDeserializer[T <: AnyRef] extends JSONDeserializer[T] {
    def deserialize(x: Object): T = x.asInstanceOf[T]
  }

  implicit object stringJSON extends IdentityDeserializer[String]
  implicit object numberJSON extends IdentityDeserializer[java.lang.Number]
  implicit object intJSON extends JSONDeserializer[Int] {
    def deserialize(x: Object): Int =
      fromJSON[java.lang.Number](x).intValue()
  }
  implicit object booleanJSON extends JSONDeserializer[Boolean] {
    def deserialize(x: Object): Boolean = x.asInstanceOf[java.lang.Boolean]
  }

  implicit def listJSON[T : JSONDeserializer] = new JSONDeserializer[List[T]] {
    def deserialize(x: Object): List[T] =
      x.asInstanceOf[java.util.List[Object]].asScala.map(fromJSON[T] _).toList
  }

  implicit def mapJSON[K : JSONDeserializer, V : JSONDeserializer] = {
    new JSONDeserializer[Map[K,V]] {
      def deserialize(x: Object): Map[K,V] = {
        val scmap = x.asInstanceOf[java.util.Map[Object, Object]].asScala
        scmap.map { case (k,v) => (fromJSON[K](k), fromJSON[V](v)) }.toMap
      }
    }
  }

}
