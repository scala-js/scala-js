package org.junit

import java.lang.annotation._

class Before extends scala.annotation.Annotation
    with java.lang.annotation.Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[Before]
}
