package org.scalajs.testcommon

import sbt.testing._

private[scalajs] object JSMasterEndpoints {
  val newRunner: RPCEndpoint.EP[RunnerArgs, Unit] =
    RPCEndpoint[RunnerArgs, Unit](2)

  val runnerDone: RPCEndpoint.EP[Unit, String] =
    RPCEndpoint[Unit, String](3)

  val tasks: RPCEndpoint.EP[List[TaskDef], List[TaskInfo]] =
    RPCEndpoint[List[TaskDef], List[TaskInfo]](4)

  val msg: MsgEndpoint.EP[FrameworkMessage] = MsgEndpoint[FrameworkMessage](5)
}
