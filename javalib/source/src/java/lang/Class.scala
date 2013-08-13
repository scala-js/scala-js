package java.lang

import scala.js

final class Class[A] private(private[lang] val data: js.Dynamic) extends Object {

  override def toString(): String = {
    (if (isInterface()) "interface " else
        if (isPrimitive()) "" else "class ")+getName()
  }

  def isInstance(obj: Object): scala.Boolean =
    js.Dynamic.global.ScalaJS.dynamicIsInstanceOf(
        obj.asInstanceOf[js.Any], data).asInstanceOf[js.Boolean]

  def isAssignableFrom(that: Class[_]): scala.Boolean =
    js.Dynamic.global.ScalaJS.dynamicIsAssignableFrom(
        this.data, that.data).asInstanceOf[js.Boolean]

  def isInterface(): scala.Boolean =
    data.isInterface.asInstanceOf[js.Boolean]

  def isArray(): scala.Boolean =
    data.isArrayClass.asInstanceOf[js.Boolean]

  def isPrimitive(): scala.Boolean =
    data.isPrimitive.asInstanceOf[js.Boolean]

  def getName(): String = {
    data.displayName.asInstanceOf[js.String]
  }

  def getSuperClass(): Class[_ >: A] =
    if (!data.parentData) null
    else data.parentData.getClassOf().asInstanceOf[Class[_ >: A]]

  def getComponentType(): Class[_] =
    if (isArray()) data.componentData.getClassOf().asInstanceOf[Class[_]]
    else null

  def getEnclosingClass(): Class[_] = null
}
