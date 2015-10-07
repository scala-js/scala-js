/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit

import java.lang.annotation._

import org.junit.runners.MethodSorters

class FixMethodOrder(val value: MethodSorters)
    extends Annotation {

  def this() = this(MethodSorters.DEFAULT)

  def annotationType(): Class[_ <: Annotation] = classOf[FixMethodOrder]
}
