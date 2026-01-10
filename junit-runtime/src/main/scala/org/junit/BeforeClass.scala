/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

import java.lang.annotation._

class BeforeClass
    extends scala.annotation.StaticAnnotation
    with java.lang.annotation.Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[BeforeClass]
}
