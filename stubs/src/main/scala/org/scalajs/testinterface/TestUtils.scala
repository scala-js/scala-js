package org.scalajs.testinterface

import language.experimental.macros

/** Dummy object to get the right shadowing for 2.10 / 2.11 cross compilation */
private object Compat210 {
  object blackbox {
    type Context = scala.reflect.macros.Context
  }
}

import Compat210._

@deprecated(
    "Use https://github.com/portable-scala/portable-scala-reflect instead.",
    "0.6.25")
object TestUtils {
  import scala.reflect.macros._ // shadows blackbox from above
  import blackbox.Context

  def newInstance(name: String, loader: ClassLoader, paramTypes: Seq[Class[_]])(
      args: Seq[AnyRef]): AnyRef =
    macro newInstance_impl

  def newInstance_impl(c: Context)(name: c.Expr[String],
      loader: c.Expr[ClassLoader], paramTypes: c.Expr[Seq[Class[_]]])(
      args: c.Expr[Seq[AnyRef]]): c.Expr[AnyRef] = c.universe.reify {

    val clazz = loader.splice.loadClass(name.splice)
    val ctor = clazz.getConstructor(paramTypes.splice.toArray[Class[_]]: _*)
    ctor.newInstance(args.splice: _*).asInstanceOf[AnyRef]
  }

  def loadModule(name: String, loader: ClassLoader): AnyRef =
    macro loadModule_impl

  def loadModule_impl(c: Context)(name: c.Expr[String],
      loader: c.Expr[ClassLoader]): c.Expr[AnyRef] = c.universe.reify {
    val clazz = loader.splice.loadClass(name.splice + "$")
    clazz.getField("MODULE$").get(null)
  }

}
