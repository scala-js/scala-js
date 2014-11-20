package org.scalajs.testinterface

import scala.scalajs.js

object TestUtils {

  def newInstance(name: String, loader: ClassLoader)(args: Seq[AnyRef]): AnyRef = {
    val ctor = deepSelect(namespace(loader), name)
    js.Dynamic.newInstance(ctor)(args.asInstanceOf[Seq[js.Any]]: _*)
  }

  def loadModule(name: String, loader: ClassLoader): AnyRef = {
    val accessor = deepSelect(namespace(loader), name)
    accessor()
  }

  private def namespace(loader: ClassLoader): js.Dynamic = {
    loader match {
      case loader: ScalaJSClassLoader => loader.namespace
      case _ => throw new IllegalArgumentException(
          "Need a ScalaJSClassLoader.")
    }
  }

  private def deepSelect(receiver: js.Dynamic, name: String) =
    name.split('.').foldLeft(receiver)((obj, n) => obj.selectDynamic(n))

}
