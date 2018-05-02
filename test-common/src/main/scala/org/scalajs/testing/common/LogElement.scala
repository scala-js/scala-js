package org.scalajs.testing.common

private[testing] final class LogElement[T](val index: Int, val x: T)

private[testing] object LogElement {
  implicit def logElementSerializer[T: Serializer]: Serializer[LogElement[T]] = {
    new Serializer[LogElement[T]] {
      def serialize(x: LogElement[T], out: Serializer.SerializeState): Unit = {
        out.write(x.index)
        out.write(x.x)
      }

      def deserialize(in: Serializer.DeserializeState): LogElement[T] = {
        new LogElement(in.read[Int](), in.read[T]())
      }
    }
  }
}
