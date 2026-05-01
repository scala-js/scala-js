/*
 * Scala.js JS Envs (https://github.com/scala-js/scala-js-js-envs)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package build

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs.NodeJSEnv

final class MinimalWasmJSEnv(config: NodeJSEnv.Config) extends JSEnv {
  val name: String = s"Node.js for MinimalWasm"

  def start(input: Seq[Input], runConfig: RunConfig): JSRun =
    new NodeJSEnv(config).start(input, runConfig)

  def startWithCom(input: Seq[Input], runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    MinimalWasmComRun.start(runConfig, onMessage) { setupFile =>
      val configWithHook =
        config.withArgs(config.args ++ List("--import", setupFile.toUri.toString))
      new NodeJSEnv(configWithHook).start(input, runConfig)
    }
  }
}
