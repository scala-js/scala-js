/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.sbtplugin.environment

import java.io.File

import org.mozilla.javascript.Scriptable
import org.mozilla.javascript.Context
import org.mozilla.javascript.BaseFunction
import org.mozilla.javascript.Undefined
import org.mozilla.javascript.ScriptableObject

package object rhino {

  implicit class ContextOps(val self: Context) extends AnyVal {
    def evaluateFile(scope: Scriptable, file: File,
      securityDomain: AnyRef = null): Any = {
      val reader = new java.io.FileReader(file)
      try {
        self.evaluateReader(scope, reader,
          file.getAbsolutePath, 1, securityDomain)
      } finally {
        reader.close()
      }
    }
  }

  implicit class ScriptableObjectOps(val self: ScriptableObject) {
    def addFunction(name: String, function: Array[AnyRef] => Unit) = {
      val rhinoFunction =
        new BaseFunction {
          override def call(context: Context, scope: Scriptable,
              thisObj: Scriptable, args: Array[AnyRef]): AnyRef = {
            function(args)
            Undefined.instance
          }
        }

      ScriptableObject.putProperty(self, name, rhinoFunction)
    }
  }
}
