package java.lang

import scala.js

final class Class[A] private(private[lang] val env: js.Dynamic,
    private[lang] val data: js.Dynamic) extends Object {

  override def toString(): String = {
    (if (isInterface()) "interface " else
        if (isPrimitive()) "" else "class ")+getName()
  }

  def isInstance(obj: Object): scala.Boolean =
    env.isInstance(obj.asInstanceOf[js.Any], data.name).asInstanceOf[js.Boolean]

  def isAssignableFrom(that: Class[_]): scala.Boolean =
    !(!that.data.ancestors.selectDynamic(this.data.name.asInstanceOf[js.String]))

  def isInterface(): scala.Boolean =
    data.isInterface.asInstanceOf[js.Boolean]

  def isArray(): scala.Boolean =
    data.isArray.asInstanceOf[js.Boolean]

  def isPrimitive(): scala.Boolean =
    data.isPrimitive.asInstanceOf[js.Boolean]

  def getName(): String = {
    data.displayName.asInstanceOf[js.String]
  }

  def getSuperClass(): Class[_ >: A] =
    if (!data.parentData) null
    else data.parentData.cls.asInstanceOf[Class[_ >: A]]

  def getComponentType(): Class[_] =
    if (isArray()) data.componentData.cls.asInstanceOf[Class[_]]
    else null
}
