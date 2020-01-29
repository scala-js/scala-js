package org.scalajs.sbtplugin.test

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
@JSImport("foo.js", "JustImport")
object JustImport extends js.Object

@js.native
@JSImport("foo.js", "Another")
object AnotherInFoo extends js.Object

@js.native
@JSImport("bar.js", JSImport.Default, globalFallback = "GlobalFallback")
object ImportWithGlobalFallback extends js.Object

@js.native
@JSImport("unused.js", "Unreachable")
object Unreachable extends js.Object

@js.native
@JSGlobal
object JustGlobal extends js.Object

object Main {
  def main(args: Array[String]): Unit = {
    JustImport
    AnotherInFoo
    ImportWithGlobalFallback
    JustGlobal
  }
}
