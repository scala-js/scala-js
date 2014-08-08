package java.lang

import scala.scalajs.js

private trait ScalaJSClassData[A] extends js.Object {
  val name: String
  val isPrimitive: scala.Boolean
  val isInterface: scala.Boolean
  val isArrayClass: scala.Boolean

  def isInstance(obj: Object): scala.Boolean
  def getFakeInstance(): Object

  def getSuperclass(): Class[_ >: A]
  def getComponentType(): Class[_]

  def newArrayOfThisClass(dimensions: js.Array[Int]): AnyRef
}

final class Class[A] private (data: ScalaJSClassData[A]) extends Object {

  override def toString(): String = {
    (if (isInterface()) "interface " else
        if (isPrimitive()) "" else "class ")+getName()
  }

  def isInstance(obj: Object): scala.Boolean =
    data.isInstance(obj)

  def isAssignableFrom(that: Class[_]): scala.Boolean =
    if (this.isPrimitive || that.isPrimitive) {
      /* This differs from the JVM specification to mimic the behavior of
       * runtime type tests of primitive numeric types.
       */
      (this eq that) || {
        if (this eq classOf[scala.Short])
          (that eq classOf[scala.Byte])
        else if (this eq classOf[scala.Int])
          (that eq classOf[scala.Byte]) || (that eq classOf[scala.Short])
        else if (this eq classOf[scala.Float])
          (that eq classOf[scala.Byte]) || (that eq classOf[scala.Short]) ||
          (that eq classOf[scala.Int])
        else if (this eq classOf[scala.Double])
          (that eq classOf[scala.Byte]) || (that eq classOf[scala.Short]) ||
          (that eq classOf[scala.Int])  || (that eq classOf[scala.Float])
        else
          false
      }
    } else {
      this.isInstance(that.getFakeInstance())
    }

  private def getFakeInstance(): Object =
    data.getFakeInstance()

  def isInterface(): scala.Boolean =
    data.isInterface

  def isArray(): scala.Boolean =
    data.isArrayClass

  def isPrimitive(): scala.Boolean =
    data.isPrimitive

  def getName(): String =
    data.name

  def getSimpleName(): String =
    data.name.split('.').last.split('$').last

  def getSuperclass(): Class[_ >: A] =
    data.getSuperclass()

  def getComponentType(): Class[_] =
    data.getComponentType()

  def getEnclosingClass(): Class[_] = null

  // java.lang.reflect.Array support

  private[lang] def newArrayOfThisClass(dimensions: js.Array[Int]): AnyRef =
    data.newArrayOfThisClass(dimensions)
}
