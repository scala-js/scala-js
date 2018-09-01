package sbttest.framework

/** Platform-specific implementations.
 *
 *  A typical testing framework would use portable-scala-reflect instead.
 */
private[framework] object Platform {
  def instantiateTestClass(fullName: String, classLoader: ClassLoader): Test = {
    val cls = Class.forName(fullName, true, classLoader)
    assert(classOf[Test].isAssignableFrom(cls), fullName)
    cls.newInstance().asInstanceOf[Test]
  }
}
