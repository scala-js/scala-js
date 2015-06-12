package org.scalajs.testsuite

package object javalib {

  implicit private[javalib] class CompareNullablesOps(val self: Any) extends AnyVal {
    @inline
    def ===(that: Any): Boolean =
      if (self.asInstanceOf[AnyRef] eq null) that.asInstanceOf[AnyRef] eq null
      else self.equals(that)
  }

  private[javalib] final case class Box[+K](inner: K) {
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
