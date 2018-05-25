/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Node.js env       **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2017, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.nodejs

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec
import scala.collection.JavaConverters._

import org.scalajs.jsenv._

import org.scalajs.io._
import org.scalajs.io.JSUtils.escapeJS
import org.scalajs.logging._

import java.io._

final class NodeJSEnv(config: NodeJSEnv.Config) extends JSEnv {
  def this() = this(NodeJSEnv.Config())

  val name: String = "Node.js"

  def start(input: Input, runConfig: RunConfig): JSRun = {
    NodeJSEnv.validator.validate(runConfig)
    internalStart(initFiles ++ inputFiles(input), runConfig)
  }

  def startWithCom(input: Input, runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    NodeJSEnv.validator.validate(runConfig)
    ComRun.start(runConfig, onMessage) { comLoader =>
      val files = initFiles ::: (comLoader :: inputFiles(input))
      internalStart(files, runConfig)
    }
  }

  private def internalStart(files: List[VirtualBinaryFile],
      runConfig: RunConfig): JSRun = {
    val command = config.executable :: config.args
    val externalConfig = ExternalJSRun.Config()
      .withEnv(env)
      .withRunConfig(runConfig)
    ExternalJSRun.start(command, externalConfig)(NodeJSEnv.write(files))
  }

  private def initFiles: List[VirtualBinaryFile] = {
    val base = List(NodeJSEnv.runtimeEnv, Support.fixPercentConsole)

    if (config.sourceMap) NodeJSEnv.installSourceMap :: base
    else base
  }

  private def inputFiles(input: Input) = input match {
    case Input.ScriptsToLoad(scripts) => scripts
    case _                            => throw new UnsupportedInputException(input)
  }

  private def env: Map[String, String] =
    Map("NODE_MODULE_CONTEXTS" -> "0") ++ config.env
}

object NodeJSEnv {
  private lazy val validator = ExternalJSRun.supports(RunConfig.Validator())

  private lazy val installSourceMap = {
    MemVirtualBinaryFile.fromStringUTF8("sourceMapSupport.js",
        "require('source-map-support').install();")
  }

  private lazy val runtimeEnv = {
    MemVirtualBinaryFile.fromStringUTF8("scalaJSEnvInfo.js",
        """
          |__ScalaJSEnv = {
          |  exitFunction: function(status) { process.exit(status); }
          |};
        """.stripMargin
    )
  }

  private def write(files: List[VirtualBinaryFile])(out: OutputStream): Unit = {
    val p = new PrintStream(out, false, "UTF8")
    try {
      files.foreach {
        case file: FileVirtualBinaryFile =>
          val fname = file.file.getAbsolutePath
          p.println(s"""require("${escapeJS(fname)}");""")
        case f =>
          val in = f.inputStream
          try {
            val buf = new Array[Byte](4096)

            @tailrec
            def loop(): Unit = {
              val read = in.read(buf)
              if (read != -1) {
                p.write(buf, 0, read)
                loop()
              }
            }

            loop()
          } finally {
            in.close()
          }

          p.println()
      }
    } finally {
      p.close()
    }
  }

  final class Config private (
      val executable: String,
      val args: List[String],
      val env: Map[String, String],
      val sourceMap: Boolean
  ) {
    private def this() = {
      this(
          executable = "node",
          args = Nil,
          env = Map.empty,
          sourceMap = true
      )
    }

    def withExecutable(executable: String): Config =
      copy(executable = executable)

    def withArgs(args: List[String]): Config =
      copy(args = args)

    def withEnv(env: Map[String, String]): Config =
      copy(env = env)

    def withSourceMap(sourceMap: Boolean): Config =
      copy(sourceMap = sourceMap)

    private def copy(
        executable: String = executable,
        args: List[String] = args,
        env: Map[String, String] = env,
        sourceMap: Boolean = sourceMap
    ): Config = {
      new Config(executable, args, env, sourceMap)
    }
  }

  object Config {
    /** Returns a default configuration for a [[NodeJSEnv]].
     *
     *  The defaults are:
     *
     *  - `executable`: `"node"`
     *  - `args`: `Nil`
     *  - `env`: `Map.empty`
     *  - `sourceMap`: `true`
     */
    def apply(): Config = new Config()
  }
}
