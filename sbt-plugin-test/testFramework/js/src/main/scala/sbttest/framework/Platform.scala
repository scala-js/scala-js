package sbttest.framework

import scala.scalajs.reflect.Reflect

/** Platform-specific implementations.
 *
 *  A typical testing framework would use portable-scala-reflect instead.
 */
private[framework] object Platform {
  type EnableReflectiveInstantiation =
    scala.scalajs.reflect.annotation.EnableReflectiveInstantiation

  def instantiateTestClass(fullName: String, classLoader: ClassLoader): Test = {
    val cls = Reflect.lookupInstantiatableClass(fullName).getOrElse {
      throw new ClassNotFoundException(s"Cannot find $fullName")
    }
    assert(classOf[Test].isAssignableFrom(cls.runtimeClass), fullName)
    cls.newInstance().asInstanceOf[Test]
  }
}
