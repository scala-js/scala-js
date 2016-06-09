/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js sbt plugin        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.sbtplugin.cross

import scala.reflect.macros.Context

private[cross] object MacroUtils {

  // Copied from sbt.std.KeyMacros

  def definingValName(c: Context, invalidEnclosingTree: String => String): String = {
    import c.universe._
    val methodName = c.macroApplication.symbol.name

    // trim is not strictly correct, but macros don't expose the API necessary
    def processName(n: Name): String = n.decoded.trim

    def enclosingVal(trees: List[c.Tree]): String = trees match {
      case vd @ ValDef(_, name, _, _) :: ts =>
        processName(name)

      case (_: Apply | _: Select | _: TypeApply) :: xs =>
        enclosingVal(xs)

      // lazy val x: X = <methodName> has this form for some reason
      // (only when the explicit type is present, though)
      case Block(_, _) :: DefDef(mods, name, _, _, _, _) :: xs if mods.hasFlag(Flag.LAZY) =>
        processName(name)
      case _ =>
        c.error(c.enclosingPosition, invalidEnclosingTree(methodName.decoded))
        "<error>"
    }

    enclosingVal(enclosingTrees(c).toList)
  }

  def enclosingTrees(c: Context): Seq[c.Tree] =
    c.asInstanceOf[reflect.macros.runtime.Context].callsiteTyper.
      context.enclosingContextChain.map(_.tree.asInstanceOf[c.Tree])
}
