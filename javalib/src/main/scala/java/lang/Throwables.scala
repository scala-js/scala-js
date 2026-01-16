/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package java.lang

import java.util.function._

import scala.scalajs.js.annotation.JSExport

class Throwable protected (s: String, private var e: Throwable,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Object with java.io.Serializable {

  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this() = this(null, null)
  def this(s: String) = this(s, null)
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)

  private[this] var jsErrorForStackTrace: Any = _
  private[this] var stackTrace: Array[StackTraceElement] = _

  /* We use an Array rather than, say, a List, so that Throwable does not
   * depend on the Scala collections.
   */
  private[this] var suppressed: Array[Throwable] = _

  if (writableStackTrace)
    fillInStackTrace()

  def initCause(cause: Throwable): Throwable = {
    e = cause
    this
  }

  def getMessage(): String = s
  def getCause(): Throwable = e
  def getLocalizedMessage(): String = getMessage()

  def fillInStackTrace(): Throwable = {
    jsErrorForStackTrace = StackTrace.captureJSError(this)
    this
  }

  def getStackTrace(): Array[StackTraceElement] = {
    if (stackTrace eq null) {
      if (writableStackTrace)
        stackTrace = StackTrace.extract(jsErrorForStackTrace)
      else
        stackTrace = new Array[StackTraceElement](0)
    }
    stackTrace
  }

  def setStackTrace(stackTrace: Array[StackTraceElement]): Unit = {
    if (writableStackTrace) {
      var i = 0
      while (i < stackTrace.length) {
        if (stackTrace(i) eq null)
          throw new NullPointerException()
        i += 1
      }

      this.stackTrace = stackTrace.clone()
    }
  }

  def printStackTrace(): Unit = printStackTrace(System.err)

  def printStackTrace(s: java.io.PrintStream): Unit =
    printStackTraceImpl(s.println(_))

  def printStackTrace(s: java.io.PrintWriter): Unit =
    printStackTraceImpl(s.println(_))

  private[this] def printStackTraceImpl(sprintln: Consumer[String]): Unit = {
    getStackTrace() // will init it if still null

    // Message
    sprintln.accept(toString)

    // Trace
    if (stackTrace.length != 0) {
      var i = 0
      while (i < stackTrace.length) {
        sprintln.accept(s"  at ${stackTrace(i)}")
        i += 1
      }
    } else {
      sprintln.accept("  <no stack trace available>")
    }

    // Causes
    var wCause: Throwable = this
    while ((wCause ne wCause.getCause()) && (wCause.getCause() ne null)) {
      val parentTrace = wCause.getStackTrace()
      wCause = wCause.getCause()
      val thisTrace = wCause.getStackTrace()

      val thisLength = thisTrace.length
      val parentLength = parentTrace.length

      sprintln.accept(s"Caused by: $wCause")

      if (thisLength != 0) {
        /* Count how many frames are shared between this stack trace and the
         * parent stack trace, so that we can omit them when printing.
         */
        var sameFrameCount: Int = 0
        while (sameFrameCount < thisLength && sameFrameCount < parentLength &&
            thisTrace(thisLength - sameFrameCount - 1) ==
            parentTrace(parentLength - sameFrameCount - 1)) {
          sameFrameCount += 1
        }

        /* If at least one, decrement so that the first common frame is still
         * printed. According to Harmony this is spec'ed and common practice.
         */
        if (sameFrameCount > 0)
          sameFrameCount -= 1

        // Print the non-common frames
        val lengthToPrint = thisLength - sameFrameCount
        var i = 0
        while (i < lengthToPrint) {
          sprintln.accept(s"  at ${thisTrace(i)}")
          i += 1
        }

        if (sameFrameCount > 0)
          sprintln.accept(s"  ... $sameFrameCount more")
      } else {
        sprintln.accept("  <no stack trace available>")
      }
    }
  }

  /* Re-export toString() because Throwable will be disconnected from Object
   * to extend js.Error instead, and exports are not transferred.
   */
  @JSExport
  override def toString(): String = {
    val className = getClass().getName()
    val message = getMessage()
    if (message eq null) className
    else s"$className: $message"
  }

  def addSuppressed(exception: Throwable): Unit = {
    if (exception eq null)
      throw new NullPointerException
    if (exception eq this)
      throw new IllegalArgumentException

    if (enableSuppression) {
      if (suppressed eq null) {
        suppressed = Array(exception)
      } else {
        val length = suppressed.length
        val newSuppressed = new Array[Throwable](length + 1)
        System.arraycopy(suppressed, 0, newSuppressed, 0, length)
        newSuppressed(length) = exception
        suppressed = newSuppressed
      }
    }
  }

  def getSuppressed(): Array[Throwable] = {
    if (suppressed eq null)
      new Array(0)
    else
      suppressed.clone()
  }

  /* A JavaScript Error object should have a `name` property containing a
   * string representation of the class of the error.
   */
  @JSExport("name")
  @inline
  protected def js_name: String = getClass().getName()

  /* A JavaScript Error object should have a `message` property containing a
   * string representation of the message associated with the error.
   */
  @JSExport("message")
  @inline
  protected def js_message: String = {
    val m = getMessage()
    if (m eq null) "" else m
  }
}

class ThreadDeath() extends Error()

/* java.lang.*Error.java */

class AbstractMethodError(s: String) extends IncompatibleClassChangeError(s) {
  def this() = this(null)
}

class AssertionError(message: String, cause: Throwable)
    extends Error(message, cause) {

  def this() = this(null, null)

  def this(detailMessage: Object) = {
    this(
      String.valueOf(detailMessage),
      detailMessage match {
        case cause: Throwable => cause
        case _                => null
      }
    )
  }

  def this(detailMessage: scala.Boolean) = this(String.valueOf(detailMessage), null)
  def this(detailMessage: scala.Char) = this(String.valueOf(detailMessage), null)
  def this(detailMessage: scala.Int) = this(String.valueOf(detailMessage), null)
  def this(detailMessage: scala.Long) = this(String.valueOf(detailMessage), null)
  def this(detailMessage: scala.Float) = this(String.valueOf(detailMessage), null)
  def this(detailMessage: scala.Double) = this(String.valueOf(detailMessage), null)
}

class BootstrapMethodError(s: String, e: Throwable) extends LinkageError(s) {
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class ClassCircularityError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class ClassFormatError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class Error protected (s: String, e: Throwable,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Throwable(s, e, enableSuppression, writableStackTrace) {
  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this() = this(null, null)
  def this(s: String) = this(s, null)
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
}

class ExceptionInInitializerError private (s: String, private val e: Throwable)
    extends LinkageError(s) {
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

abstract class VirtualMachineError(message: String, cause: Throwable)
    extends Error(message, cause) {

  def this() = this(null, null)

  def this(message: String) = this(message, null)

  def this(cause: Throwable) =
    this(if (cause == null) null else cause.toString, cause)
}

/* java.lang.*Exception.java */

class ArithmeticException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class ArrayIndexOutOfBoundsException(s: String) extends IndexOutOfBoundsException(s) {
  def this(index: Int) = this(s"Array index out of range: $index")
  def this() = this(null)
}

class ArrayStoreException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class ClassCastException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class ClassNotFoundException(s: String, e: Throwable) extends ReflectiveOperationException(s) {
  def this(s: String) = this(s, null)
  def this() = this(null, null)
  def getException(): Throwable = e
  override def getCause(): Throwable = e
}

class CloneNotSupportedException(s: String) extends Exception(s) {
  def this() = this(null)
}

class EnumConstantNotPresentException(e: Class[_ <: Enum[_]], c: String)
    extends RuntimeException(s"${e.getName()}.$c") {
  def enumType(): Class[_ <: Enum[_]] = e
  def constantName(): String = c
}

class Exception protected (s: String, e: Throwable,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Throwable(s, e, enableSuppression, writableStackTrace) {
  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class IllegalAccessException(s: String) extends ReflectiveOperationException(s) {
  def this() = this(null)
}

class IllegalArgumentException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class IllegalMonitorStateException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class IllegalStateException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class IllegalThreadStateException(s: String) extends IllegalArgumentException(s) {
  def this() = this(null)
}

class IndexOutOfBoundsException(s: String) extends RuntimeException(s) {
  def this(index: Int) = this(s"Index out of range: $index")
  def this(index: scala.Long) = this(s"Index out of range: $index")
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
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class RejectedExecutionException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class RuntimeException protected (s: String, e: Throwable,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Exception(s, e, enableSuppression, writableStackTrace) {
  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class SecurityException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
  def this(s: String) = this(s, null)
  def this() = this(null, null)
}

class StringIndexOutOfBoundsException(s: String) extends IndexOutOfBoundsException(s) {
  def this(index: Int) = this(s"String index out of range: $index")
  def this() = this(null)
}

class TypeNotPresentException(t: String, e: Throwable)
    extends RuntimeException(s"Type $t not present", e) {
  def typeName(): String = t
}

class UnsupportedOperationException(s: String, e: Throwable) extends RuntimeException(s, e) {
  def this() = this(null, null)
  def this(s: String) = this(s, null)
  def this(e: Throwable) = this(if (e == null) null else e.toString, e)
}
