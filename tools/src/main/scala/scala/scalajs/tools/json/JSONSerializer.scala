package scala.scalajs.tools.json

trait JSONSerializer[T] {
  def serialize(x: T): Object
}

object JSONSerializer {
  import scala.collection.JavaConverters._

  trait IdentitySerializer[T <: AnyRef] extends JSONSerializer[T] {
    def serialize(x: T): Object = x
  }

  implicit object stringJSON extends IdentitySerializer[String]
  implicit object numberJSON extends IdentitySerializer[java.lang.Number]
  implicit object intJSON extends JSONSerializer[Int] {
    def serialize(x: Int): java.lang.Integer = x
  }
  implicit object booleanJSON extends JSONSerializer[Boolean] {
    def serialize(x: Boolean): java.lang.Boolean= x
  }

  implicit def listJSON[T : JSONSerializer] = new JSONSerializer[List[T]] {
    def serialize(x: List[T]) = x.map(_.toJSON).asJava
  }

  implicit def mapJSON[K : JSONSerializer, V : JSONSerializer] = {
    new JSONSerializer[Map[K,V]] {
      def serialize(x: Map[K,V]) =
        x.map { case (k,v) => (k.toJSON, v.toJSON) }.asJava
    }
  }

}
