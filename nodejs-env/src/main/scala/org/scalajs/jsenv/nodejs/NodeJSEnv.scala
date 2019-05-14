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

package org.scalajs.jsenv.nodejs

import java.nio.charset.StandardCharsets

import scala.annotation.tailrec
import scala.collection.JavaConverters._

import org.scalajs.jsenv._
import org.scalajs.jsenv.JSUtils.escapeJS

import org.scalajs.io._
import org.scalajs.logging._

import java.io._

final class NodeJSEnv(config: NodeJSEnv.Config) extends JSEnv {
  import NodeJSEnv._

  def this() = this(NodeJSEnv.Config())

  val name: String = "Node.js"

  def start(input: Input, runConfig: RunConfig): JSRun = {
    NodeJSEnv.validator.validate(runConfig)
    validateInput(input)
    internalStart(initFiles, input, runConfig)
  }

  def startWithCom(input: Input, runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    NodeJSEnv.validator.validate(runConfig)
    validateInput(input)
    ComRun.start(runConfig, onMessage) { comLoader =>
      internalStart(initFiles :+ comLoader, input, runConfig)
    }
  }

  private def validateInput(input: Input): Unit = {
    input match {
      case _:Input.ScriptsToLoad | _:Input.ESModulesToLoad |
          _:Input.CommonJSModulesToLoad =>
        // ok
      case _ =>
        throw new UnsupportedInputException(input)
    }
  }

  private def internalStart(initFiles: List[VirtualBinaryFile], input: Input,
      runConfig: RunConfig): JSRun = {
    val command = config.executable :: config.args
    val externalConfig = ExternalJSRun.Config()
      .withEnv(env)
      .withRunConfig(runConfig)
    ExternalJSRun.start(command, externalConfig)(
        NodeJSEnv.write(initFiles, input))
  }

  private def initFiles: List[VirtualBinaryFile] = config.sourceMap match {
    case SourceMap.Disable           => Nil
    case SourceMap.EnableIfAvailable => installSourceMapIfAvailable :: Nil
    case SourceMap.Enable            => installSourceMap :: Nil
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

  private lazy val installSourceMapIfAvailable = {
    MemVirtualBinaryFile.fromStringUTF8("sourceMapSupport.js",
        """
          |try {
          |  require('source-map-support').install();
          |} catch (e) {
          |};
        """.stripMargin
    )
  }

  private lazy val installSourceMap = {
    MemVirtualBinaryFile.fromStringUTF8("sourceMapSupport.js",
        "require('source-map-support').install();")
  }

  private def write(initFiles: List[VirtualBinaryFile], input: Input)(
      out: OutputStream): Unit = {
    val p = new PrintStream(out, false, "UTF8")
    try {
      def writeRunScript(file: VirtualBinaryFile): Unit = {
        file match {
          case file: FileVirtualBinaryFile =>
            val pathJS = "\"" + escapeJS(file.file.getAbsolutePath) + "\""
            p.println(s"""
              require('vm').runInThisContext(
                require('fs').readFileSync($pathJS, { encoding: "utf-8" }),
                { filename: $pathJS, displayErrors: true }
              );
            """)

          case _ =>
            val code = readInputStreamToString(file.inputStream)
            val codeJS = "\"" + escapeJS(code) + "\""
            val pathJS = "\"" + escapeJS(file.path) + "\""
            p.println(s"""
              require('vm').runInThisContext(
                $codeJS,
                { filename: $pathJS, displayErrors: true }
              );
            """)
        }
      }

      def writeRequire(file: VirtualBinaryFile): Unit = {
        file match {
          case file: FileVirtualBinaryFile =>
            p.println(s"""require("${escapeJS(file.file.getAbsolutePath)}")""")

          case _ =>
            val f = tmpFile(file.path, file.inputStream)
            p.println(s"""require("${escapeJS(f.getAbsolutePath)}")""")
        }
      }

      for (initFile <- initFiles)
        writeRunScript(initFile)

      input match {
        case Input.ScriptsToLoad(scripts) =>
          for (script <- scripts)
            writeRunScript(script)

        case Input.CommonJSModulesToLoad(modules) =>
          for (module <- modules)
            writeRequire(module)

        case Input.ESModulesToLoad(modules) =>
          if (modules.nonEmpty) {
            val uris = modules.map {
              case module: FileVirtualBinaryFile =>
                module.file.toURI
              case module =>
                tmpFile(module.path, module.inputStream).toURI
            }

            val imports = uris.map { uri =>
              s"""import("${escapeJS(uri.toASCIIString)}")"""
            }
            val importChain = imports.reduceLeft { (prev, imprt) =>
              s"""$prev.then(_ => $imprt)"""
            }

            val importerFileContent = {
              s"""
                |$importChain.catch(e => {
                |  console.error(e);
                |  process.exit(1);
                |});
              """.stripMargin
            }
            val f = tmpFile("importer.js", importerFileContent)
            p.println(s"""require("${escapeJS(f.getAbsolutePath)}");""")
          }
      }
    } finally {
      p.close()
    }
  }

  private def readInputStreamToString(inputStream: InputStream): String = {
    val baos = new java.io.ByteArrayOutputStream
    val in = inputStream
    try {
      val buf = new Array[Byte](4096)

      @tailrec
      def loop(): Unit = {
        val read = in.read(buf)
        if (read != -1) {
          baos.write(buf, 0, read)
          loop()
        }
      }

      loop()
    } finally {
      in.close()
    }
    new String(baos.toByteArray(), StandardCharsets.UTF_8)
  }

  private def tmpFile(path: String, content: String): File = {
    import java.nio.file.{Files, StandardOpenOption}

    val f = createTmpFile(path)
    val contentList = new java.util.ArrayList[String]()
    contentList.add(content)
    Files.write(f.toPath(), contentList, StandardCharsets.UTF_8,
        StandardOpenOption.TRUNCATE_EXISTING)
    f
  }

  private def tmpFile(path: String, content: InputStream): File = {
    import java.nio.file.{Files, StandardCopyOption}

    try {
      val f = createTmpFile(path)
      Files.copy(content, f.toPath(), StandardCopyOption.REPLACE_EXISTING)
      f
    } finally {
      content.close()
    }
  }

  // tmpSuffixRE and createTmpFile copied from HTMLRunnerBuilder.scala

  private val tmpSuffixRE = """[a-zA-Z0-9-_.]*$""".r

  private def createTmpFile(path: String): File = {
    /* - createTempFile requires a prefix of at least 3 chars
     * - we use a safe part of the path as suffix so the extension stays (some
     *   browsers need that) and there is a clue which file it came from.
     */
    val suffix = tmpSuffixRE.findFirstIn(path).orNull

    val f = File.createTempFile("tmp-", suffix)
    f.deleteOnExit()
    f
  }

  /** Requirements for source map support. */
  sealed abstract class SourceMap

  object SourceMap {
    /** Disable source maps. */
    case object Disable extends SourceMap

    /** Enable source maps if `source-map-support` is available. */
    case object EnableIfAvailable extends SourceMap

    /** Always enable source maps.
     *
     *  If `source-map-support` is not available, loading the .js code will
     *  fail.
     */
    case object Enable extends SourceMap
  }

  final class Config private (
      val executable: String,
      val args: List[String],
      val env: Map[String, String],
      val sourceMap: SourceMap
  ) {
    private def this() = {
      this(
          executable = "node",
          args = Nil,
          env = Map.empty,
          sourceMap = SourceMap.EnableIfAvailable
      )
    }

    def withExecutable(executable: String): Config =
      copy(executable = executable)

    def withArgs(args: List[String]): Config =
      copy(args = args)

    def withEnv(env: Map[String, String]): Config =
      copy(env = env)

    def withSourceMap(sourceMap: SourceMap): Config =
      copy(sourceMap = sourceMap)

    /** Forces enabling (true) or disabling (false) source maps.
     *
     *  `sourceMap = true` maps to [[SourceMap.Enable]]. `sourceMap = false`
     *  maps to [[SourceMap.Disable]]. [[SourceMap.EnableIfAvailable]] is never
     *  used by this method.
     */
    def withSourceMap(sourceMap: Boolean): Config =
      withSourceMap(if (sourceMap) SourceMap.Enable else SourceMap.Disable)

    private def copy(
        executable: String = executable,
        args: List[String] = args,
        env: Map[String, String] = env,
        sourceMap: SourceMap = sourceMap
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
     *  - `sourceMap`: [[SourceMap.EnableIfAvailable]]
     */
    def apply(): Config = new Config()
  }
}
