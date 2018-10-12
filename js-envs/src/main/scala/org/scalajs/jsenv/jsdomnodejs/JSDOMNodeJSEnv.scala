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

package org.scalajs.jsenv.jsdomnodejs

class JSDOMNodeJSEnv(config: JSDOMNodeJSEnv.Config)
    extends org.scalajs.jsenv.nodejs.JSDOMNodeJSEnv(config.executable,
        config.args, config.env, internal = ()) {

  def this() = this(JSDOMNodeJSEnv.Config())
}

object JSDOMNodeJSEnv {
  final class Config private (
      val executable: String,
      val args: List[String],
      val env: Map[String, String]
  ) {
    private def this() = {
      this(
          executable = "node",
          args = Nil,
          env = Map.empty
      )
    }

    def withExecutable(executable: String): Config =
      copy(executable = executable)

    def withArgs(args: List[String]): Config =
      copy(args = args)

    def withEnv(env: Map[String, String]): Config =
      copy(env = env)

    private def copy(
        executable: String = executable,
        args: List[String] = args,
        env: Map[String, String] = env
    ): Config = {
      new Config(executable, args, env)
    }
  }

  object Config {
    /** Returns a default configuration for a [[JSDOMNodeJSEnv]].
     *
     *  The defaults are:
     *
     *  - `executable`: `"node"`
     *  - `args`: `Nil`
     *  - `env`: `Map.empty`
     */
    def apply(): Config = new Config()
  }
}
