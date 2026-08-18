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

import MinimalWasmInput.MinimalWasmModule

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
       |        if (isErr !== 0)
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

  private lazy val i8ArrayModuleBytesContent: String =
    makeArrayModuleBytesContent(storageTypeCode = 0x78, valueTypeCode = 0x7f) // i8, i32

  private lazy val i16ArrayModuleBytesContent: String =
    makeArrayModuleBytesContent(storageTypeCode = 0x77, valueTypeCode = 0x7f) // i16, i32

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

  /** Generates (the bytes of) a WebAssembly that exposes functions to manipulate
   *  one type of GC array.
   *
   *  The generated module exports 4 functions:
   *
   *  - `create: [i32] -> [(ref arrayType)]`
   *  - `length: [(ref arrayType)] -> [i32]`
   *  - `get: [(ref arrayType) i32] -> [valueType]`
   *  - `set: [(ref arrayType) i32 valueType] -> []`
   *
   *  where `arrayType` is an `(array (mut storageType))` and `valueType` is
   *  the value type corresponding to `storageType`.
   */
  private def makeArrayModuleBytesContent(storageTypeCode: Byte, valueTypeCode: Byte): String = {
    val buf = new mutable.ArrayBuffer[Byte]()

    // magic header: null char + "asm"
    buf ++= Seq(0, 'a', 's', 'm')

    // version
    buf ++= Seq(1, 0, 0, 0)

    val i32Code = 0x7f.toByte

    val i32Type = Seq(i32Code)
    val storageType = Seq(storageTypeCode)
    val valueType = Seq(valueTypeCode)

    val NonNullableHeapTypePrefix = 0x64.toByte
    val arrayTypeID = 0.toByte
    val arrayType = Seq(NonNullableHeapTypePrefix, arrayTypeID)

    val arrayGet2ndByte: Byte =
      if (storageTypeCode == 0x77 || storageTypeCode == 0x78) 0x0c // array.get_s
      else 0x0b // array.get

    final case class FuncDesc(
      exportName: String,
      paramTypes: Seq[Seq[Byte]],
      resultTypes: Seq[Seq[Byte]],
      finalInstr: Seq[Byte]
    )

    val functions: Seq[FuncDesc] = Seq(
      FuncDesc(
        "create",
        Seq(i32Type),
        Seq(arrayType),
        Seq(0xfb.toByte, 0x07, arrayTypeID), // array.new_default $arrayTypeID
      ),
      FuncDesc(
        "length",
        Seq(arrayType),
        Seq(i32Type),
        Seq(0xfb.toByte, 0x0f), // array.length
      ),
      FuncDesc(
        "get",
        Seq(arrayType, i32Type),
        Seq(valueType),
        Seq(0xfb.toByte, arrayGet2ndByte, arrayTypeID), // array.get(_s) $arrayTypeID
      ),
      FuncDesc(
        "set",
        Seq(arrayType, i32Type, valueType),
        Seq(),
        Seq(0xfb.toByte, 0x0e, arrayTypeID), // array.set $arrayTypeID
      )
    )

    def writeSection(sectionID: Byte)(sectionContent: => Unit): Unit = {
      buf += sectionID
      byteLengthSubSection(buf)(sectionContent)
    }

    // Types section
    writeSection(sectionID = 0x01) {
      // number of types
      buf += (1 + functions.size).toByte

      // (type $theArray (array (mut $storageType))
      buf += 0x5e // array
      buf += storageTypeCode // $storageType
      buf += 1 // mut

      // function types
      for ((func, index) <- functions.zipWithIndex) {
        buf += 0x60 // func
        buf += func.paramTypes.size.toByte
        for (paramType <- func.paramTypes)
          buf ++= paramType
        buf += func.resultTypes.size.toByte
        for (resultType <- func.resultTypes)
          buf ++= resultType
      }
    }

    // Function section
    writeSection(sectionID = 0x03) {
      buf += functions.size.toByte

      for (index <- 0 until functions.size) {
        buf += (1 + index).toByte // the function's type ID
      }
    }

    // Export section
    writeSection(sectionID = 0x07) {
      buf += functions.size.toByte

      for ((func, index) <- functions.zipWithIndex) {
        // name
        buf += func.exportName.length().toByte
        buf ++= func.exportName.map(_.toByte)

        buf += 0x00 // func
        buf += index.toByte // func ID
      }
    }

    // Code section
    writeSection(sectionID = 0x0a) {
      buf += functions.size.toByte

      for (func <- functions) {
        byteLengthSubSection(buf) {
          buf += 0 // no locals

          // Load all the params
          for (index <- 0 until func.paramTypes.size) {
            buf += 0x20 // local.get
            buf += index.toByte
          }

          // Insert the final instruction bytes
          buf ++= func.finalInstr

          buf += 0x0b // end
        }
      }
    }

    buf.map(java.lang.Byte.toUnsignedInt(_)).mkString(",")
  }

  // !!! Adapted from webassembly.BinaryWriter
  private def byteLengthSubSection(buf: mutable.ArrayBuffer[Byte])(subSectionContent: => Unit): Unit = {
    // Reserve 4 bytes at the current offset to store the byteLength later
    val byteLengthOffset = buf.size
    for (_ <- 0 until 4) // write fake bytes for now
      buf += 0
    val startOffset = buf.size

    subSectionContent

    // Compute byteLength
    val endOffset = buf.size
    val byteLength = endOffset - startOffset

    /* Write the byteLength in the reserved slot. Note that we *always* use
     * 4 bytes to store the byteLength, even when less bytes are necessary in
     * the unsigned LEB encoding. The WebAssembly spec specifically calls out
     * this choice as valid. We leverage it to have predictable total offsets
     * when we write the code section, which is important to efficiently
     * generate source maps.
     */
    buf(byteLengthOffset) = ((byteLength & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 1) = (((byteLength >>> 7) & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 2) = (((byteLength >>> 14) & 0x7f) | 0x80).toByte
    buf(byteLengthOffset + 3) = ((byteLength >>> 21) & 0x7f).toByte
  }
}
