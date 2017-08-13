package org.scalajs.testcommon

import sbt.testing._

private[scalajs] object JSSlaveEndpoints {
  val newRunner: RPCEndpoint.EP[Unit, Unit] =
    RPCEndpoint[Unit, Unit](2)

  val execute: RPCEndpoint.EP[ExecuteRequest, List[TaskInfo]] =
    RPCEndpoint[ExecuteRequest, List[TaskInfo]](3)

  val stopSlave: RPCEndpoint.EP[Unit, Unit] =
    RPCEndpoint[Unit, Unit](4)

  val msg: MsgEndpoint.EP[String] = MsgEndpoint[String](5)
}
