package org.scalajs.core.compiler.test

import org.scalajs.core.compiler.test.util._
import org.junit.Test

// scalastyle:off line.size.limit

class JSExportDeprecationsTest extends DirectTest with TestHelpers {

  override def extraArgs: List[String] =
    super.extraArgs :+ "-deprecation"

  override def preamble: String =
    """import scala.scalajs.js, js.annotation._
    """

  @Test
  def warnJSExportClass: Unit = {
    """
    @JSExport
    class A

    @JSExport("Foo")
    class B
    """ hasWarns
    """
      |newSource1.scala:3: warning: @JSExport on classes is deprecated and will be removed in 1.0.0. Use @JSExportTopLevel instead (which does exactly the same thing on classes).
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressExportDeprecations` to scalac)
      |    @JSExport
      |     ^
      |newSource1.scala:6: warning: @JSExport on classes is deprecated and will be removed in 1.0.0. Use @JSExportTopLevel instead (which does exactly the same thing on classes).
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressExportDeprecations` to scalac)
      |    @JSExport("Foo")
      |     ^
    """
  }

  @Test
  def warnJSExportObject: Unit = {
    """
    @JSExport
    object A

    @JSExport("Foo")
    object B
    """ hasWarns
    """
      |newSource1.scala:3: warning: @JSExport on objects is deprecated and will be removed in 1.0.0. Use @JSExportTopLevel instead. Note that it exports the object itself (rather than a 0-arg function returning the object), so the calling JavaScript code must be adapted.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressExportDeprecations` to scalac)
      |    @JSExport
      |     ^
      |newSource1.scala:6: warning: @JSExport on objects is deprecated and will be removed in 1.0.0. Use @JSExportTopLevel instead. Note that it exports the object itself (rather than a 0-arg function returning the object), so the calling JavaScript code must be adapted.
      |  (you can suppress this warning in 0.6.x by passing the option `-P:scalajs:suppressExportDeprecations` to scalac)
      |    @JSExport("Foo")
      |     ^
    """
  }

}
