/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv

import org.mozilla.javascript._

import org.scalajs.core.tools.io._

package object rhino {

  private[rhino] implicit class ContextOps(val self: Context) extends AnyVal {
    def evaluateFile(scope: Scriptable, file: VirtualJSFile,
        securityDomain: AnyRef = null): Any = {
      self.evaluateString(scope, file.content, file.path, 1, securityDomain)
    }
  }

  private[rhino] implicit class ScriptableObjectOps(val self: Scriptable) {
    def addFunction(name: String, function: Array[AnyRef] => Any): Unit = {
      val rhinoFunction =
        new BaseFunction {
          ScriptRuntime.setFunctionProtoAndParent(this, self)
          override def call(context: Context, scope: Scriptable,
              thisObj: Scriptable, args: Array[AnyRef]): AnyRef = {
            function(args) match {
              case () => Undefined.instance
              case r => r.asInstanceOf[AnyRef]
            }
          }
        }

      ScriptableObject.putProperty(self, name, rhinoFunction)
    }
  }
}
