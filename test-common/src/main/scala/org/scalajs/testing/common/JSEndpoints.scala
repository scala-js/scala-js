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

package org.scalajs.testing.common

import sbt.testing.TaskDef

private[testing] object JSEndpoints {
  val detectFrameworks: RPCEndpoint.EP[List[List[String]], List[Option[FrameworkInfo]]] =
    RPCEndpoint[List[List[String]], List[Option[FrameworkInfo]]](2)

  val createControllerRunner: RPCEndpoint.EP[RunnerArgs, Unit] =
    RPCEndpoint[RunnerArgs, Unit](3)

  val createWorkerRunner: RPCEndpoint.EP[RunnerArgs, Unit] =
    RPCEndpoint[RunnerArgs, Unit](4)

  val msgWorker: MsgEndpoint.EP[RunMux[String]] =
    MsgEndpoint[RunMux[String]](5)

  val msgController: MsgEndpoint.EP[RunMux[FrameworkMessage]] =
    MsgEndpoint[RunMux[FrameworkMessage]](6)

  val tasks: RPCEndpoint.EP[RunMux[List[TaskDef]], List[TaskInfo]] =
    RPCEndpoint[RunMux[List[TaskDef]], List[TaskInfo]](7)

  val execute: RPCEndpoint.EP[RunMux[ExecuteRequest], List[TaskInfo]] =
    RPCEndpoint[RunMux[ExecuteRequest], List[TaskInfo]](8)

  val done: RPCEndpoint.EP[RunMux[Unit], String] =
    RPCEndpoint[RunMux[Unit], String](9)
}
