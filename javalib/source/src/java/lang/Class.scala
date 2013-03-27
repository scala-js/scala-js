package java.lang

import scala.js._

final class Class[A] private(private[lang] val env: JSDynamic,
    private[lang] val data: JSDynamic) extends Object {

  override def toString(): String = {
    (if (isInterface()) "interface " else
        if (isPrimitive()) "" else "class ")+getName()
  }

  def isInstance(obj: Object): scala.Boolean =
    env.isInstance(obj.asInstanceOf[JSAny], data.name).asInstanceOf[JSBoolean]

  def isAssignableFrom(that: Class[_]): scala.Boolean =
    !(!that.data.ancestors.selectDynamic(this.data.name.asInstanceOf[JSString]))

  def isInterface(): scala.Boolean =
    data.isInterface.asInstanceOf[JSBoolean]

  def isArray(): scala.Boolean =
    data.isArray.asInstanceOf[JSBoolean]

  def isPrimitive(): scala.Boolean =
    data.isPrimitive.asInstanceOf[JSBoolean]

  def getName(): String = {
    data.displayName.asInstanceOf[JSString]
  }

  def getSuperClass(): Class[_ >: A] =
    if (!data.parentData) null
    else data.parentData.`class`.asInstanceOf[Class[_ >: A]]

  def getComponentType(): Class[_] =
    if (isArray()) data.componentData.`class`.asInstanceOf[Class[_]]
    else null
}
