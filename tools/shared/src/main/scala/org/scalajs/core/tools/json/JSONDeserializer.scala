package org.scalajs.core.tools.json

trait JSONDeserializer[T] {
  def deserialize(x: JSON): T
}

object JSONDeserializer {

  implicit object stringJSON extends JSONDeserializer[String] {
    def deserialize(x: JSON): String = Impl.toString(x)
  }

  implicit object intJSON extends JSONDeserializer[Int] {
    def deserialize(x: JSON): Int = Impl.toNumber(x).intValue()
  }

  implicit object booleanJSON extends JSONDeserializer[Boolean] {
    def deserialize(x: JSON): Boolean = Impl.toBoolean(x)
  }

  implicit def listJSON[T : JSONDeserializer] = new JSONDeserializer[List[T]] {
    def deserialize(x: JSON): List[T] = Impl.toList(x).map(fromJSON[T] _)
  }

  implicit def mapJSON[V : JSONDeserializer] = new JSONDeserializer[Map[String, V]] {
    def deserialize(x: JSON): Map[String, V] =
      Impl.toMap(x).mapValues(fromJSON[V] _)
  }

}
