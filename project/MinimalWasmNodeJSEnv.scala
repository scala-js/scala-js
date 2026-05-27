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

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardCopyOption}

import org.scalajs.jsenv._
import org.scalajs.jsenv.JSUtils.escapeJS
import org.scalajs.jsenv.MinimalWasmInput.MinimalWasmModule
import org.scalajs.jsenv.nodejs.NodeJSEnv

/** Node.js based environment for `ModuleKind.MinimalWasmModule`.
 *
 *  This JSEnv accepts only `MinimalWasmModule` inputs and generates a
 *  small JavaScript runner that instantiates the Wasm module and provides the
 *  host imports.
 */
final class MinimalWasmNodeJSEnv(config: NodeJSEnv.Config) extends JSEnv {
  import MinimalWasmNodeJSEnv._

  def this() = this(NodeJSEnv.Config())

  val name: String = s"Node.js for MinimalWasm"

  def start(input: Seq[Input], runConfig: RunConfig): JSRun = {
    MinimalWasmNodeJSEnv.validator.validate(runConfig)
    validateInput(input)
    internalStart(input, runConfig, None)
  }

  def startWithCom(input: Seq[Input], runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    MinimalWasmNodeJSEnv.validator.validate(runConfig)
    validateInput(input)

    MinimalWasmNodeJSComRun.start(runConfig, onMessage) { port =>
      internalStart(input, runConfig, Some(port))
    }
  }

  private def validateInput(input: Seq[Input]): Unit = input match {
    case Seq(_: MinimalWasmModule) =>
      // ok
    case _ =>
      throw new UnsupportedInputException(input)
  }

  private def internalStart(input: Seq[Input], runConfig: RunConfig,
      comPort: Option[Int]): JSRun = {
    val command = config.executable :: config.args
    val externalConfig = ExternalJSRun.Config()
      .withEnv(env)
      .withRunConfig(runConfig)
    ExternalJSRun.start(command, externalConfig)(MinimalWasmNodeJSEnv.write(input, comPort))
  }

  private def env: Map[String, String] =
    Map("NODE_MODULE_CONTEXTS" -> "0") ++ config.env
}

object MinimalWasmNodeJSEnv {
  private lazy val validator = ExternalJSRun.supports(RunConfig.Validator())

  private def write(input: Seq[Input], comPort: Option[Int])(out: OutputStream): Unit = {
    assert(input.size == 1)
    assert(input.head.isInstanceOf[MinimalWasmModule])

    def requireRunner(module: Path): String = {
      val runnerFileContent = runnerContent(module, comPort)
      val f = createTmpFile("runner.js")
      Files.write(f.toPath, runnerFileContent.getBytes(StandardCharsets.UTF_8))
      s"""require("${escapeJS(f.getAbsolutePath)}")"""
    }

    val p = new PrintStream(out, false, "UTF8")
    try {
      val module = input.head.asInstanceOf[MinimalWasmModule].module
      p.println(requireRunner(module) + ";")
    } finally {
      p.close()
    }
  }

  private def runnerContent(wasmPath: Path, comPort: Option[Int]): String = {
    val wasmPathJS = "\"" + escapeJS(toFile(wasmPath).getAbsolutePath) + "\""
    val comSetup = comPort.fold("")(MinimalWasmNodeJSComRun.setupContent)

    s"""
       |(async function() {
       |  const fs = require("node:fs/promises");
       |
       |  const wasmI8ArrayBytes = new Uint8Array([$i8ArrayModuleBytesContent]);
       |  const wasmI8Array =
       |      (await WebAssembly.instantiate(wasmI8ArrayBytes)).instance.exports;
       |  const wasmI16ArrayBytes = new Uint8Array([$i16ArrayModuleBytesContent]);
       |  const wasmI16Array =
       |      (await WebAssembly.instantiate(wasmI16ArrayBytes)).instance.exports;
       |
       |  function wasmI8ArrayToJSString(array) {
       |    var len = wasmI8Array.length(array);
       |    var bytes = new Uint8Array(len);
       |    for (var i = 0; i !== len; i++)
       |      bytes[i] = wasmI8Array.get(array, i);
       |    return new TextDecoder("utf-8").decode(bytes);
       |  }
       |
       |  const importsObj = {
       |    "scalajs:core": {
       |      currentTimeMillis: () => BigInt(Math.trunc(Date.now())),
       |      nanoTime: () => BigInt(Math.trunc(performance.now() * 1000000)),
       |      doWriteLine: (isErr, line) => {
       |        const str = wasmI8ArrayToJSString(line);
       |        if (isErr)
       |          console.error(str);
       |        else
       |          console.log(str);
       |      },
       |    },
       |  };
       |
       |$comSetup
       |
       |  const buffer = await fs.readFile($wasmPathJS);
       |  const result = await WebAssembly.instantiate(buffer, importsObj);
       |  if (typeof afterInstantiate === "function")
       |    afterInstantiate(result);
       |})();
       |""".stripMargin
  }

  private[build] lazy val i8ArrayModuleBytesContent: String =
    arrayModuleBytesContent("minimal-wasm-i8array.wasm")

  private[build] lazy val i16ArrayModuleBytesContent: String =
    arrayModuleBytesContent("minimal-wasm-i16array.wasm")

  private def toFile(path: Path): File = {
    try {
      path.toFile
    } catch {
      case _: UnsupportedOperationException =>
        val f = createTmpFile(path.toString)
        Files.copy(path, f.toPath(), StandardCopyOption.REPLACE_EXISTING)
        f
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

  private def arrayModuleBytesContent(resourceName: String): String = {
    val stream = getClass.getClassLoader.getResourceAsStream(resourceName)
    if (stream == null)
      throw new IllegalStateException(s"Missing resource: $resourceName")
    try {
      val out = new ByteArrayOutputStream()
      val buf = new Array[Byte](1024)
      var len = stream.read(buf)
      while (len != -1) {
        out.write(buf, 0, len)
        len = stream.read(buf)
      }
      out.toByteArray().map(_ & 0xff).mkString(",")
    } finally {
      stream.close()
    }
  }
}
