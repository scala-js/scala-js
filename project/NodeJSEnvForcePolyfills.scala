package build

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs._

import org.scalajs.io._

final class NodeJSEnvForcePolyfills(config: NodeJSEnv.Config) extends JSEnv {
  def this() = this(NodeJSEnv.Config())

  val name: String = "Node.js forcing polyfills"

  private val nodeJSEnv = new NodeJSEnv(config)

  def start(input: Input, runConfig: RunConfig): JSRun =
    nodeJSEnv.start(patchInput(input), runConfig)

  def startWithCom(input: Input, runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    nodeJSEnv.startWithCom(patchInput(input), runConfig, onMessage)
  }

  private def patchInput(input: Input): Input = input match {
    case Input.ScriptsToLoad(scripts) => Input.ScriptsToLoad(forcePolyfills +: scripts)
    case _                            => throw new UnsupportedInputException(input)
  }

  /** File to force all our ES 2015 polyfills to be used, by deleting the
   *  native functions.
   */
  private def forcePolyfills(): VirtualBinaryFile = {
    val f = new MemVirtualBinaryFile("scalaJSEnvInfo.js").withStringUTF8(
      """
        |delete Math.fround;
        |delete Math.imul;
        |delete Math.clz32;
        |delete Math.log10;
        |delete Math.log1p;
        |delete Math.cbrt;
        |delete Math.hypot;
        |delete Math.expm1;
        |delete Math.sinh;
        |delete Math.cosh;
        |delete Math.tanh;
        |
        |delete global.Promise;
        |
        |delete global.Int8Array;
        |delete global.Int16Array;
        |delete global.Int32Array;
        |delete global.Uint8Array;
        |delete global.Uint16Array;
        |delete global.Uint32Array;
        |delete global.Float32Array;
        |delete global.Float64Array;
      """.stripMargin
    )
    f
  }
}
