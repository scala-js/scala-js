package build

import java.nio.charset.StandardCharsets
import java.nio.file._

import com.google.common.jimfs.Jimfs

import org.scalajs.jsenv._
import org.scalajs.jsenv.nodejs._

import org.scalajs.linker.interface.ESVersion

final class NodeJSEnvForcePolyfills(esVersion: ESVersion, config: NodeJSEnv.Config) extends JSEnv {
  def this(esVersion: ESVersion) = this(esVersion, NodeJSEnv.Config())

  val name: String = s"Node.js forcing polyfills for $esVersion"

  // Deactivate source maps if esVersion < ES2015 because source-map-support requires `Map`
  private val nodeJSEnv = {
    val config1 =
      if (esVersion >= ESVersion.ES2015) config
      else config.withSourceMap(false)
    new NodeJSEnv(config1)
  }

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
    import ESVersion._

    def cond(version: ESVersion, stat: String): String =
      if (esVersion < version) stat
      else ""

    var script = ""

    if (esVersion < ES2015) {
      script += """
        |delete Object.is;
        |
        |delete Reflect.ownKeys;
        |
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
        |delete global.Map;
        |delete global.Promise;
        |delete global.Set;
        |delete global.Symbol;
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
    }

    if (esVersion < ES2017) {
      script += """
        |delete Object.getOwnPropertyDescriptors;
      """.stripMargin
    }

    if (esVersion < ES2018) {
      script += s"""
        |global.RegExp = (function(OrigRegExp) {
        |  return function RegExp(pattern, flags) {
        |    if (typeof flags === 'string') {
        |${cond(ES2015, """
        |      if (flags.indexOf('u') >= 0)
        |        throw new SyntaxError("unsupported flag 'u'");
        |      if (flags.indexOf('y') >= 0)
        |        throw new SyntaxError("unsupported flag 'y'");
        |""".stripMargin)}
        |      if (flags.indexOf('s') >= 0)
        |        throw new SyntaxError("unsupported flag 's'");
        |    }
        |
        |    if (typeof pattern === 'string') {
        |      if (pattern.indexOf('(?<=') >= 0 || pattern.indexOf('(?<!') >= 0)
        |        throw new SyntaxError("unsupported look-behinds");
        |      if (pattern.indexOf('(?<') >= 0)
        |        throw new SyntaxError("unsupported named capture groups");
        |      if (pattern.indexOf('\\\\p{') >= 0 || pattern.indexOf('\\\\P{') >= 0)
        |        throw new SyntaxError("unsupported Unicode character classes");
        |    }
        |
        |    return new OrigRegExp(pattern, flags);
        |  }
        |})(global.RegExp);
      """.stripMargin
    }

    val p = Files.write(
        Jimfs.newFileSystem().getPath("scalaJSEnvInfo.js"),
        script.getBytes(StandardCharsets.UTF_8))
    Input.Script(p)
  }
}
