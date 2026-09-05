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
      val codeUnits = new Array[Short](len)
      var i = 0
      while (i != len) {
        codeUnits(i) = s.charAt(i).toShort
        i += 1
      }
      doWriteLine(0, codeUnits)
    } {
      System.out.println(x)
    }
  }

  @scala.scalajs.wasm.annotation.WasmImport("scalajs:core", "doWriteLine")
  def doWriteLine(isErr: scala.Int, line: Array[Short]): Unit =
    scala.scalajs.wasm.native
}
