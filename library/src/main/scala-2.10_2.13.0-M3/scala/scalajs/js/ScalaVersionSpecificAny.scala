package scala.scalajs.js

import scala.collection.generic.CanBuildFrom
import scala.collection.mutable

trait ScalaVersionSpecificAny {

  implicit def canBuildFromArray[A]: CanBuildFrom[Array[_], A, Array[A]] = {
    @inline
    class CanBuildFromArray extends CanBuildFrom[Array[_], A, Array[A]] {
      def apply(from: Array[_]): mutable.Builder[A, Array[A]] =
        new ArrayOps[A]
      def apply(): mutable.Builder[A, Array[A]] =
        new ArrayOps[A]
    }
    new CanBuildFromArray
  }

}
