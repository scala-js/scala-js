package build

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs._

import org.scalajs.core.tools.io._

class NodeJSEnvForcePolyfills(config: NodeJSEnv.Config)
    extends NodeJSEnv(config) {

  def this() = this(NodeJSEnv.Config())

  override protected def vmName: String = "Node.js forcing polyfills"

  /** File(s) to force all our ES 2015 polyfills to be used, by deleting the
   *  native functions.
   */
  protected def forcePolyfills(): Seq[VirtualJSFile] = {
    val f = new MemVirtualJSFile("scalaJSEnvInfo.js").withContent(
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
        |delete global.ArrayBuffer;
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
    Seq(f)
  }

  /** Custom initialization scripts. */
  override protected def customInitFiles(): Seq[VirtualJSFile] =
    super.customInitFiles() ++ forcePolyfills()

}
