package scala.scalajs.js

import scala.collection.BuildFrom
import scala.collection.mutable

trait ScalaVersionSpecificAny {

  implicit def buildFromArray[A]: BuildFrom[Array[_], A, Array[A]] = {
    @inline
    class BuildFromArray extends BuildFrom[Array[_], A, Array[A]] {
      def fromSpecificIterable(from: Array[_])(it: scala.collection.Iterable[A]): Array[A] = {
        val b = newBuilder(from)
        b.sizeHint(it)
        b ++= it
        b.result()
      }
      def newBuilder(from: Array[_]): mutable.Builder[A, Array[A]] =
        new ArrayOps[A]
    }
    new BuildFromArray
  }

}
