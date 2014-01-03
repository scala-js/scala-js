package scala.scalajs.sbtplugin.environment

import org.mozilla.javascript.Scriptable
import java.io.File
import org.mozilla.javascript.Context
import org.mozilla.javascript.BaseFunction
import org.mozilla.javascript.Undefined
import org.mozilla.javascript.ScriptableObject

package object rhino {

  implicit class ContextOps(val self: Context) {
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
          override def call(context: Context, scope: Scriptable, thisObj: Scriptable, args: Array[AnyRef]): AnyRef = {
            function(args)
            Undefined.instance
          }
        }

      ScriptableObject.putProperty(self, name, rhinoFunction)
    }
  }
}
