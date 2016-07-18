package org.testng.annotations

import java.lang.annotation._

class AfterClass extends scala.annotation.StaticAnnotation with Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[AfterClass]
}
