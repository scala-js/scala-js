/*
 * Ported from https://github.com/junit-team/junit
 */
package org.junit.runners

object MethodSorters {

  private lazy val _NAME_ASCENDING = new MethodSorters((x, y) => x.compareTo(y))
  private lazy val _JVM = new MethodSorters((x, y) => 0)
  private lazy val _DEFAULT = _NAME_ASCENDING

  def NAME_ASCENDING: MethodSorters = _NAME_ASCENDING

  def JVM: MethodSorters = _JVM

  def DEFAULT: MethodSorters = _DEFAULT
}

class MethodSorters private (f: (String, String) => Int) {
  lazy val comparator: Ordering[String] = {
    new Ordering[String] {
      def compare(x: String, y: String): Int = f(x, y)
    }
  }
}
