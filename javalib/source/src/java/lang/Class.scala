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
    if (this.isPrimitive || that.isPrimitive) this eq that
    else this.isInstance(that.getFakeInstance())

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

  def getSuperclass(): Class[_ >: A] =
    data.getSuperclass()

  def getComponentType(): Class[_] =
    data.getComponentType()

  def getEnclosingClass(): Class[_] = null

  // java.lang.reflect.Array support

  private[lang] def newArrayOfThisClass(dimensions: js.Array[Int]): AnyRef =
    data.newArrayOfThisClass(dimensions)
}
