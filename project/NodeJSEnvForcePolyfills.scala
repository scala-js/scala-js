package build

import java.nio.charset.StandardCharsets
import java.nio.file._

import com.google.common.jimfs.Jimfs

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs._

final class NodeJSEnvForcePolyfills(config: NodeJSEnv.Config) extends JSEnv {
  def this() = this(NodeJSEnv.Config())

  val name: String = "Node.js forcing polyfills"

  private val nodeJSEnv = new NodeJSEnv(config)

  def start(input: Seq[Input], runConfig: RunConfig): JSRun =
    nodeJSEnv.start(forcePolyfills +: input, runConfig)

  def startWithCom(input: Seq[Input], runConfig: RunConfig,
      onMessage: String => Unit): JSComRun = {
    nodeJSEnv.startWithCom(forcePolyfills +: input, runConfig, onMessage)
  }

  /** File to force all our ES 2015 polyfills to be used, by deleting the
   *  native functions.
   */
  private def forcePolyfills(): Input = {
    val p = Files.write(
        Jimfs.newFileSystem().getPath("scalaJSEnvInfo.js"),
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
        """.stripMargin.getBytes(StandardCharsets.UTF_8))
    Input.Script(p)
  }
}
