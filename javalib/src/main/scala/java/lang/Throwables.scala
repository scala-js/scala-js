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
import java.util.Objects.requireNonNull

import scala.scalajs.js.annotation.JSExport

class Throwable private[lang] (s: String,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Object with java.io.Serializable {

  protected def this(message: String, cause: Throwable,
      enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean) = {
    this(message, enableSuppression, writableStackTrace)
    initCauseUnchecked(cause)
  }

  def this() = this(null: String, true, true)
  def this(message: String) = this(message, true, true)
  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)

  private[this] var initCauseDone: scala.Boolean = _
  private[this] var e: Throwable = _
  private[this] var jsErrorForStackTrace: Any = _
  private[this] var stackTrace: Array[StackTraceElement] = _

  /* We use an Array rather than, say, a List, so that Throwable does not
   * depend on the Scala collections.
   */
  private[this] var suppressed: Array[Throwable] = _

  if (writableStackTrace)
    fillInStackTrace()

  def initCause(cause: Throwable): Throwable = {
    if (initCauseDone)
      throw new IllegalStateException("Can't overwrite cause with " + cause)
    if (cause eq this)
      throw new IllegalArgumentException("Self-causation not permitted")
    initCauseUnchecked(cause)
    this
  }

  @inline private[java] def initCauseUnchecked(cause: Throwable): Unit = {
    initCauseDone = true
    this.e = cause
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
        requireNonNull(stackTrace(i))
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
    requireNonNull(exception)
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

class AssertionError private (message: String) extends Error(message) {
  def this() = this(null: String)

  def this(detailMessage: Object) = {
    this(String.valueOf(detailMessage))
    detailMessage match {
      case cause: Throwable => initCauseUnchecked(cause)
      case _                => ()
    }
  }

  def this(detailMessage: scala.Boolean) = this(String.valueOf(detailMessage))
  def this(detailMessage: scala.Char) = this(String.valueOf(detailMessage))
  def this(detailMessage: scala.Int) = this(String.valueOf(detailMessage))
  def this(detailMessage: scala.Long) = this(String.valueOf(detailMessage))
  def this(detailMessage: scala.Float) = this(String.valueOf(detailMessage))
  def this(detailMessage: scala.Double) = this(String.valueOf(detailMessage))

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }
}

class BootstrapMethodError(message: String) extends LinkageError(message) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class ClassCircularityError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class ClassFormatError(s: String) extends LinkageError(s) {
  def this() = this(null)
}

class Error private[lang] (message: String,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Throwable(message, enableSuppression, writableStackTrace) {

  protected def this(message: String, cause: Throwable,
      enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean) = {
    this(message, enableSuppression, writableStackTrace)
    initCauseUnchecked(cause)
  }

  def this() = this(null: String, true, true)
  def this(s: String) = this(s, true, true)
  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class ExceptionInInitializerError(s: String) extends LinkageError(s) {
  def this() = this(null: String)

  def this(thrown: Throwable) = {
    this(if (thrown == null) null else thrown.toString())
    initCauseUnchecked(thrown)
  }

  def getException(): Throwable = getCause()
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
  def this() = this(null: String)

  def this(s: String, cause: Throwable) = {
    this(s)
    initCauseUnchecked(cause)
  }
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

abstract class VirtualMachineError(message: String) extends Error(message) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
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

class ClassNotFoundException(s: String) extends ReflectiveOperationException(s) {
  def this() = this(null: String)

  def this(s: String, ex: Throwable) = {
    this(s)
    initCauseUnchecked(ex)
  }

  def getException(): Throwable = getCause()
}

class CloneNotSupportedException(s: String) extends Exception(s) {
  def this() = this(null)
}

class EnumConstantNotPresentException(e: Class[_ <: Enum[_]], c: String)
    extends RuntimeException(s"${e.getName()}.$c") {
  def enumType(): Class[_ <: Enum[_]] = e
  def constantName(): String = c
}

class Exception private[lang] (message: String,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Throwable(message, enableSuppression, writableStackTrace) {

  protected def this(message: String, cause: Throwable,
      enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean) = {
    this(message, enableSuppression, writableStackTrace)
    initCauseUnchecked(cause)
  }

  def this() = this(null: String, true, true)
  def this(s: String) = this(s, true, true)
  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class IllegalAccessException(s: String) extends ReflectiveOperationException(s) {
  def this() = this(null)
}

class IllegalArgumentException(s: String) extends RuntimeException(s) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class IllegalMonitorStateException(s: String) extends RuntimeException(s) {
  def this() = this(null)
}

class IllegalStateException(s: String) extends RuntimeException(s) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
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

class ReflectiveOperationException(message: String) extends Exception(message) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class RuntimeException private[lang] (message: String,
    enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean)
    extends Exception(message, enableSuppression, writableStackTrace) {

  protected def this(message: String, cause: Throwable,
      enableSuppression: scala.Boolean, writableStackTrace: scala.Boolean) = {
    this(message, enableSuppression, writableStackTrace)
    initCauseUnchecked(cause)
  }

  def this() = this(null: String, true, true)
  def this(s: String) = this(s, true, true)
  def this(message: String, cause: Throwable) = this(message, cause, true, true)
  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class SecurityException(s: String) extends RuntimeException(s) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}

class StringIndexOutOfBoundsException(s: String) extends IndexOutOfBoundsException(s) {
  def this(index: Int) = this(s"String index out of range: $index")
  def this() = this(null)
}

class TypeNotPresentException(t: String, e: Throwable)
    extends RuntimeException(s"Type $t not present", e) {
  def typeName(): String = t
}

class UnsupportedOperationException(message: String) extends RuntimeException(message) {
  def this() = this(null: String)

  def this(message: String, cause: Throwable) = {
    this(message)
    initCauseUnchecked(cause)
  }

  def this(cause: Throwable) = this(if (cause == null) null else cause.toString(), cause)
}
