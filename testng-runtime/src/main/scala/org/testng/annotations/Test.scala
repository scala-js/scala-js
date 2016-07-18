package org.testng.annotations

import java.lang.annotation._

class Test(
    val dataProvider: String,
    val enabled: Boolean,
    val timeOut: Long,
    val expectedExceptions: Array[Class[_ <: Throwable]]) extends scala.annotation.StaticAnnotation with Annotation {

  def this() {
    this(null, true, -1L, Array())
  }

  def annotationType(): Class[_ <: Annotation] = classOf[Test]
}
