package org.scalajs.testcommon

import scala.util._
import scala.concurrent._

private[scalajs] object FutureUtil {
  /** Same as Future.fromTry(x) but works in 2.10 */
  def futureFromTry[T](x: Try[T]): Future[T] = {
    val promise = Promise[T]
    promise.complete(x)
    promise.future
  }
}
