/* JSM - JavaScript manipulation library
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.tools.jsm

object Names {
  trait Name {
    val underlying: String

    override def toString = underlying

    def quoted: String
  }

  class IdentifierName(val underlying: String) extends Name {
    override def quoted = {
      // TODO
      underlying
    }
  }

  class PropertyName(val underlying: String) extends Name {
    override def quoted = {
      // TODO
      underlying
    }
  }

  def identName(name: String) =
    new IdentifierName(name)

  def propName(name: String) =
    new PropertyName(name)
}
