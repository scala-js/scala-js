package org.junit

import java.lang.annotation._

trait Rule extends Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[Rule]
}
