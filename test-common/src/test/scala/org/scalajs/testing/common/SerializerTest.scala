package org.scalajs.testing.common

import org.junit.Test
import org.junit.Assert._

class SerializerTest {
  def roundTrip[T: Serializer](x: T): T =
    Serializer.deserialize[T](Serializer.serialize(x))

  @Test
  def serializeThrowableWithNullFields: Unit = {
    val in = new Throwable(null, null)
    val out = roundTrip(in)
    assertEquals(in.getMessage(), out.getMessage())
    assertEquals(in.getCause(), out.getCause())
    assertEquals(in.toString(), out.toString())
    assertEquals(in.getStackTrace().size, out.getStackTrace().size)
  }
}
