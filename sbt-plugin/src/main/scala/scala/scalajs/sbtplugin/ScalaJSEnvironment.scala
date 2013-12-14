/* Scala.js sbt plugin
 * Copyright 2013 LAMP/EPFL
 */

package scala.scalajs.sbtplugin

import org.mozilla.javascript.Context
import org.mozilla.javascript.ScriptableObject

trait ScalaJSEnvironment {

  def runInContextAndScope(code: (Context, ScriptableObject) => Unit): Unit
}
