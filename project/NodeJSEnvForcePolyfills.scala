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
    import ESVersion._

    def cond(version: ESVersion, stat: String): String =
      if (esVersion < version) stat
      else ""

    var script = ""

    if (esVersion < ES2017) {
      script += """
        |delete Object.getOwnPropertyDescriptors;
      """.stripMargin
    }

    if (true) { // esVersion < ES2022 ('d' flag)
      script += s"""
        |global.RegExp = (function(OrigRegExp) {
        |  return function RegExp(pattern, flags) {
        |    if (typeof flags === 'string') {
        |${cond(ES2018, """
        |      if (flags.indexOf('s') >= 0)
        |        throw new SyntaxError("unsupported flag 's'");
        |""".stripMargin)}
        |      if (flags.indexOf('d') >= 0)
        |        throw new SyntaxError("unsupported flag 'd'");
        |    }
        |
        |${cond(ES2018, """
        |    if (typeof pattern === 'string') {
        |      if (pattern.indexOf('(?<=') >= 0 || pattern.indexOf('(?<!') >= 0)
        |        throw new SyntaxError("unsupported look-behinds");
        |      if (pattern.indexOf('(?<') >= 0)
        |        throw new SyntaxError("unsupported named capture groups");
        |      if (pattern.indexOf('\\\\p{') >= 0 || pattern.indexOf('\\\\P{') >= 0)
        |        throw new SyntaxError("unsupported Unicode character classes");
        |    }
        |""".stripMargin)}
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
