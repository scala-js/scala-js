package org.scalajs.testinterface

import language.experimental.macros

/** Dummy object to get the right shadowing for 2.10 / 2.11 cross compilation */
private object Compat210 {
  object blackbox { // scalastyle:ignore
    type Context = scala.reflect.macros.Context
  }
}

import Compat210._

object TestUtils {
  import scala.reflect.macros._ // shadows blackbox from above
  import blackbox.Context

  def newInstance(name: String, loader: ClassLoader)(args: Seq[AnyRef]): AnyRef =
    macro newInstance_impl

  def newInstance_impl(c: Context)(name: c.Expr[String],
      loader: c.Expr[ClassLoader])(
      args: c.Expr[Seq[AnyRef]]): c.Expr[AnyRef] = c.universe.reify {

    val clazz = loader.splice.loadClass(name.splice)
    val ctors = clazz.getConstructors()

    if (ctors.size != 1) {
      throw new IllegalArgumentException(
          "You may only call newInstance with single-ctor classes")
    }

    val ctor = ctors.head
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
