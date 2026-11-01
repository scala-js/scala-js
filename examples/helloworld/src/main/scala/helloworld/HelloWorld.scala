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
    import scala.scalajs.LinkingInfo

    LinkingInfo.linkTimeIf(LinkingInfo.moduleKind == LinkingInfo.ModuleKind.WasmModule) {
      val s = String.valueOf(x)
      val len = s.length()
      var i = 0
      while (i != len) {
        wasmPrintc(s.charAt(i).toInt)
        i += 1
      }
      wasmPrintc('\n'.toInt)
    } {
      System.out.println(x)
    }
  }

  @scala.scalajs.wasm.annotation.WasmImport("./../../../../wasm-module-support.mjs", "printc")
  def wasmPrintc(c: Int): Unit =
    scala.scalajs.wasm.native
}
