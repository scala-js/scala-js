package java.lang

class InheritableThreadLocal[T] extends ThreadLocal[T] {
  protected def childValue(parentValue: T): T = parentValue
}
