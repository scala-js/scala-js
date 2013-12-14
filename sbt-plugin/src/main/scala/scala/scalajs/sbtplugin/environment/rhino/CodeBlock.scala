package scala.scalajs.sbtplugin.environment.rhino

import org.mozilla.javascript.Scriptable
import org.mozilla.javascript.Context

abstract class CodeBlock(
  protected val context: Context,
  protected val scope: Scriptable)
  