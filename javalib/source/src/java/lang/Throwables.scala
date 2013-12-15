package java.lang

class Throwable(s: String, private var e: Throwable) extends Object with java.io.Serializable {
  def this() = this(null, null)
  def this(s: String) = this(s, null)
  def this(e: Throwable) = this(null, e)

  // def getLocalizedMessage(): String = ???
  // def printStackTrace(s: java.io.PrintStream): Unit = ???
  // def printStackTrace(s: java.io.PrintWriter): Unit = ???
  // def setStackTrace(stackTrace: scala.Array[StackTraceElement]): Unit = ???
  def initCause(cause: Throwable): Throwable = {
    e = cause
    this
  }
  
  def getStackTrace(): scala.Array[StackTraceElement] = null
  def getMessage(): String = s
  def printStackTrace(): Unit = ()
  def getCause(): Throwable = e
  def fillInStackTrace(): Throwable = this

  override def toString() = {
    val className = getClass.getName
    val message = getMessage()
    if (message eq null) className
    else className + ": " + message
  }
}

class ThreadDeath() extends Error()


/* java.lang.*Error.java */

class AbstractMethodError(s: String) extends IncompatibleClassChangeError(s) {
  def this() = this(null)
}

class AssertionError private (s: String) extends Error(s) {
  def this() = this(null)
  def this(o: Object) = this(o.toString)
  def this(b: scala.Boolean) = this(b.toString)
  def this(c: scala.Char) = this(c.toString)
  def this(i: scala.Int) = this(i.toString)
  def this(l: scala.Long) = this(l.toString)
  def this(f: scala.Float) = this(f.toString)
  def this(d: scala.Double) = this(d.toString)
}

class BootstrapMethodError(s: String, e: Throwable) extends LinkageError(s) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class ClassCircularityError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class ClassFormatError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class Error(s: String, e: Throwable) extends Throwable(s, e) {
  def this() = this(null, null)
  def this(s: String) = this(s, null)
  def this(e: Throwable) = this(null, e)
}

class ExceptionInInitializerError private (s: String, private val e: Throwable) extends LinkageError(s) {
  def this(thrown: Throwable) = this(null, thrown)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
  def getException(): Throwable = e
  override def getCause(): Throwable = e
}

class IllegalAccessError(s: String) extends IncompatibleClassChangeError(s) {
  def this() = this(null)
}

class IncompatibleClassChangeError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class InstantiationError(s: String) extends IncompatibleClassChangeError(s) {
  def this() = this(null)
}

class InternalError(s: String) extends VirtualMachineError(s) {
  def this() = this(null)
}

class LinkageError(s: String) extends Error(s) {
  def this() = this(null)
}

class NoClassDefFoundError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class NoSuchFieldError(s: String) extends IncompatibleClassChangeError(s) {
  def this() = this(null)
}

class NoSuchMethodError(s: String) extends IncompatibleClassChangeError(s) {
  def this() = this(null)
}

class OutOfMemoryError(s: String) extends VirtualMachineError(s) {
  def this() = this(null)
}

class StackOverflowError(s: String) extends VirtualMachineError(s) {
  def this() = this(null)
}

class UnknownError(s: String) extends VirtualMachineError(s) {
  def this() = this(null)
}

class UnsatisfiedLinkError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class UnsupportedClassVersionError(s: String) extends ClassFormatError(s) {
  def this() = this(null)
}

class VerifyError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

abstract class VirtualMachineError(s: String) extends Error(s) {
  def this() = this(null)
}


/* java.lang.*Exception.java */

class ArithmeticException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class ArrayIndexOutOfBoundsException(s: String) extends IndexOutOfBoundsException(s) {
  def this(index: Int) = this("Array index out of range: " + index)
  def this() = this(null)
}

class ArrayStoreException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class ClassCastException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class ClassNotFoundException(s: String,  e: Throwable) extends ReflectiveOperationException(s) {
  def this(s: String) = this(s, null)
  def this() = this(null, null)
  def getException(): Throwable = e
  override def getCause(): Throwable = e
}

class CloneNotSupportedException(s: String) extends Exception(s) {
  def this() = this(null)
}

import scala.language.existentials
class EnumConstantNotPresentException(
    e: Class[_ <: Enum[T] forSome { type T <: Enum[T] }], c: String)
    extends RuntimeException(e.getName() + "." + c) {
  def enumType() = e
  def constantName() = c
}

class Exception(s: String, e: Throwable) extends Throwable(s, e) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class IllegalAccessException(s: String) extends ReflectiveOperationException(s) {
  def this() = this(null)
}

class IllegalArgumentException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class IllegalMonitorStateException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class IllegalStateException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class IllegalThreadStateException(s: String) extends IllegalArgumentException(s) {
  def this() = this(null)
}

class IndexOutOfBoundsException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class InstantiationException(s: String) extends ReflectiveOperationException(s) {
  def this() = this(null)
}

class InterruptedException(s: String) extends Exception(s) {
  def this() = this(null)
}

class NegativeArraySizeException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class NoSuchFieldException(s: String) extends ReflectiveOperationException(s) {
  def this() = this(null)
}

class NoSuchMethodException(s: String) extends ReflectiveOperationException(s) {
  def this() = this(null)
}

class NullPointerException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class NumberFormatException(s: String) extends IllegalArgumentException(s) {
  def this() = this(null)
}

class ReflectiveOperationException(s: String, e: Throwable) extends Exception(s, e) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class RuntimeException(s: String, e: Throwable) extends Exception(s, e) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class SecurityException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this(e: Throwable) = this(null, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class StringIndexOutOfBoundsException(s: String) extends IndexOutOfBoundsException(s) {
  def this(index: Int) = this("String index out of range: " + index)
  def this() = this(null)
}

class TypeNotPresentException(t: String, e: Throwable)
  extends RuntimeException("Type " + t + " not present", e) {
  def typeName(): String = t
}

class UnsupportedOperationException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this() = this(null, null)
  def this(s: String) = this(s, null)
  def this(e: Throwable) = this(null, e)
}
