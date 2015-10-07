package org.junit

import java.lang.annotation._

trait ClassRule extends Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[ClassRule]
}
