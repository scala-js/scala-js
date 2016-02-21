package org.scalajs.core.compiler.util

import language.implicitConversions

class VarBox[T](var value: T)

object VarBox {
  implicit def toValue[T](box: VarBox[T]): T = box.value
}
