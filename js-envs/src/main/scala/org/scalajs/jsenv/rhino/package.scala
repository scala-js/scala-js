/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

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
