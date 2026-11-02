/* Scala.js example code
 * Public domain
 * @author  Sébastien Doeraene
 */

package helloworld

import java.lang.{Float => JFloat, Double => JDouble}

object HelloWorld {
  def main(args: Array[String]): Unit = {
    // Comment so that scalafmt keeps the braces

    println("Hello world!")
    val x = hide(54.5f)
    val bits = JFloat.floatToIntBits(x)
    println(bits)
    println(JFloat.intBitsToFloat(bits))

    val y = hide(54.5)
    val bits2 = JDouble.doubleToLongBits(y)
    println(bits2)
    println(JDouble.longBitsToDouble(bits2))
    bench()
  }

  @noinline def bench(): Unit = {
    val rnd = new java.util.SplittableRandom(9049941608525453806L)
    val xs = Array.fill(32000)(rnd.nextFloat())
    val out = new Array[Int](xs.length)

    val start = System.nanoTime()
    for (i <- 0 until 2000) {
      work(xs, out)
      if (out(300) != 0)
        throw new AssertionError("fail: " + out(300))
    }
    val end = System.nanoTime()
    println((end - start) / 1000000L)
  }

  @noinline def work(xs: Array[Float], out: Array[Int]): Unit = {
    for (i <- 0 until xs.length)
      //out(i) = JFloat.floatToIntBits(xs(i) + 1.5f)
      //out(i) = ((Long.MaxValue / Math.nextUp(xs(i).toDouble).toLong) >>> 32).toInt
      out(i) = Math.nextUp(xs(i).toDouble).toInt
  }

  @noinline def hide[A](x: A): A = x

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
      wasmPrintln(codeUnits)
    } {
      System.out.println(x)
    }
  }

  @scala.scalajs.wasm.annotation.WasmImport("scalajs:non-standard", "println")
  def wasmPrintln(line: Array[Short]): Unit =
    scala.scalajs.wasm.native
}
