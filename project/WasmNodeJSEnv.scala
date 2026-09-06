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

import scala.collection.mutable

import org.scalajs.jsenv._
import org.scalajs.jsenv.JSUtils.escapeJS
import org.scalajs.jsenv.nodejs.NodeJSEnv

import WasmInput.WasmModule

/** Node.js based environment for `ModuleKind.WasmModule`.
 *
 *  This JSEnv accepts only `WasmModule` inputs and generates a small
 *  JavaScript runner that instantiates the Wasm module and provides the
 *  host imports.
 */
final class WasmNodeJSEnv(config: NodeJSEnv.Config) extends JSEnv {
  import WasmNodeJSEnv._

  def this() = this(NodeJSEnv.Config())

  val name: String = s"Node.js for WasmModule"

  def start(input: Seq[Input], runConfig: RunConfig): JSRun = {
    WasmNodeJSEnv.validator.validate(runConfig)
    validateInput(input)
    internalStart(input, runConfig, None)
  }

  def startWithCom(input: Seq[Input], runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    WasmNodeJSEnv.validator.validate(runConfig)
    validateInput(input)

    WasmNodeJSComRun.start(runConfig, onMessage) { port =>
      internalStart(input, runConfig, Some(port))
    }
  }

  private def validateInput(input: Seq[Input]): Unit = input match {
    case Seq(_: WasmModule) =>
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
    ExternalJSRun.start(command, externalConfig)(WasmNodeJSEnv.write(input, comPort))
  }

  private def env: Map[String, String] =
    Map("NODE_MODULE_CONTEXTS" -> "0") ++ config.env
}

object WasmNodeJSEnv {
  private lazy val validator = ExternalJSRun.supports(RunConfig.Validator())

  private def write(input: Seq[Input], comPort: Option[Int])(out: OutputStream): Unit = {
    assert(input.size == 1)
    assert(input.head.isInstanceOf[WasmModule])

    def requireRunner(module: Path): String = {
      val runnerFileContent = runnerContent(module, comPort)
      val f = createTmpFile("runner.js")
      Files.write(f.toPath, runnerFileContent.getBytes(StandardCharsets.UTF_8))
      s"""require("${escapeJS(f.getAbsolutePath)}")"""
    }

    val p = new PrintStream(out, false, "UTF8")
    try {
      val module = input.head.asInstanceOf[WasmModule].module
      p.println(requireRunner(module) + ";")
    } finally {
      p.close()
    }
  }

  private def runnerContent(wasmPath: Path, comPort: Option[Int]): String = {
    val wasmPathJS = "\"" + escapeJS(toFile(wasmPath).getAbsolutePath) + "\""
    val comSetup = comPort.fold("")(WasmNodeJSComRun.setupContent)

    s"""
       |(async function() {
       |  const fs = require("node:fs/promises");
       |
       |  const wasmI16ArrayBytes = new Uint8Array([$i16ArrayModuleBytesContent]);
       |  const wasmI16Array =
       |      (await WebAssembly.instantiate(wasmI16ArrayBytes)).instance.exports;
       |
       |  function wasmI16ArrayToJSString(array) {
       |    var len = wasmI16Array.length(array);
       |    var result = "";
       |    for (var i = 0; i !== len; i++)
       |      result += String.fromCharCode(wasmI16Array.get(array, i) & 0xffff);
       |    return result;
       |  }
       |
       |  const importsObj = {
       |    "scalajs:non-standard": {
       |      currentTimeMillis: () => BigInt(Math.trunc(Date.now())),
       |      nanoTime: () => BigInt(Math.trunc(performance.now() * 1000000)),
       |      println: (line) => console.log(wasmI16ArrayToJSString(line)),
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

  private lazy val i16ArrayModuleBytesContent: String =
    WasmGCArrayAccessModules.i16ArrayModuleBytes.map(java.lang.Byte.toUnsignedInt(_)).mkString(",")

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
}
