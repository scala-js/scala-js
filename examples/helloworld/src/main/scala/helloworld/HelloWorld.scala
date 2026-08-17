/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

object HelloWorld {
  def main(args: Array[String]): Unit = {
    // Comment so that scalafmt keeps the braces

    println("Hello world!")
  }

  @noinline
  def println(x: Any): Unit = {
    import java.nio.charset.StandardCharsets
    import scala.scalajs.LinkingInfo

    LinkingInfo.linkTimeIf(LinkingInfo.moduleKind == LinkingInfo.ModuleKind.MinimalWasmModule) {
      doWriteLine(0, x.toString().getBytes(StandardCharsets.UTF_8))
    } {
      System.out.println(x)
    }
  }

  @scala.scalajs.wasm.minimal.annotation.WasmImport("scalajs:core", "doWriteLine")
  def doWriteLine(isErr: scala.Int, line: Array[scala.Byte]): Unit =
    scala.scalajs.wasm.native
}
