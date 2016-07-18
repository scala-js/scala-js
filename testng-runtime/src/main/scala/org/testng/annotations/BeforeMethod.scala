package org.testng.annotations

import java.lang.annotation._

class BeforeMethod extends scala.annotation.StaticAnnotation with Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[BeforeMethod]
}
