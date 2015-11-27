/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */
package org.scalajs.testsuite.javalib

package object util {

  implicit private[util] class CompareNullablesOps(val self: Any) extends AnyVal {
    @inline
    def ===(that: Any): Boolean =
      if (self.asInstanceOf[AnyRef] eq null) that.asInstanceOf[AnyRef] eq null
      else self.equals(that)
  }

  private[util] final case class Box[+K](inner: K) {
    def apply(): K = inner

    override def equals(o: Any): Boolean = {
      o match {
        case o: Box[_] => inner === o.inner
        case _         => false
      }
    }

    override def hashCode(): Int =
      if (inner == null) 0
      else inner.hashCode
  }

}
