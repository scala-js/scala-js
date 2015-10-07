/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

import java.lang.annotation._

class After extends scala.annotation.Annotation
    with java.lang.annotation.Annotation {
  def annotationType(): Class[_ <: Annotation] = classOf[After]
}
