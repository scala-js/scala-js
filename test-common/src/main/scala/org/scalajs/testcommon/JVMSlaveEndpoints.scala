package org.scalajs.testcommon

import sbt.testing._

private[scalajs] object JVMSlaveEndpoints {
  val msg: MsgEndpoint.EP[String] = MsgEndpoint[String](2)

  val event: MsgEndpoint.EP[Event] = MsgEndpoint[Event](3)

  val logError: MsgEndpoint.EP[LogElement[String]] =
    MsgEndpoint[LogElement[String]](4)

  val logWarn: MsgEndpoint.EP[LogElement[String]] =
    MsgEndpoint[LogElement[String]](5)

  val logInfo: MsgEndpoint.EP[LogElement[String]] =
    MsgEndpoint[LogElement[String]](6)

  val logDebug: MsgEndpoint.EP[LogElement[String]] =
    MsgEndpoint[LogElement[String]](7)

  val logTrace: MsgEndpoint.EP[LogElement[Throwable]] =
    MsgEndpoint[LogElement[Throwable]](8)
}
