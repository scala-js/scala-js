package org.scalajs.testcommon

import sbt.testing._

private[scalajs] object JVMMasterEndpoints {
  val msg: MsgEndpoint.EP[FrameworkMessage] = MsgEndpoint[FrameworkMessage](2)
}
