package org.testng.annotations

import java.lang.annotation._

class BeforeClass extends scala.annotation.StaticAnnotation with Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[BeforeClass]
}
