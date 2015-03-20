package scala.scalajs.runtime

import scala.util.control.ControlThrowable

/** Error thrown when an undefined behavior in Fatal mode has been detected.
 *  This error should never be caught. It indicates a severe programming bug.
 *  In Unchecked mode, the program may behave arbitrarily.
 *  The `cause` is set to the exception that would have been thrown if the
 *  given behavior was in Compliant mode.
 *  If your program relies on the proper kind of exception being thrown, as if
 *  running on the JVM, you should set the appropriate behavior to Compliant.
 *  Note that this will have (potentially major) performance impacts.
 */
class UndefinedBehaviorError(message: String, cause: Throwable)
    extends java.lang.Error(message, cause) with ControlThrowable {

  def this(message: String) = this(message, null)

  def this(cause: Throwable) =
    this("An undefined behavior was detected" +
        (if (cause == null) "" else ": "+cause.getMessage), cause)

  override def fillInStackTrace(): Throwable =
    super[Error].fillInStackTrace()
}
