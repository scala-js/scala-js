package org.junit

import java.lang.annotation._

class Before
    extends scala.annotation.StaticAnnotation
    with java.lang.annotation.Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[Before]
}
