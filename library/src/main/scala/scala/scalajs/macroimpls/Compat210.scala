/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.macroimpls

@deprecated("Not actually deprecated, makes warnings go away", "")
private[macroimpls] object Compat210 {
  object blackbox { // scalastyle:ignore
    type Context = scala.reflect.macros.Context
  }
}

import Compat210._

@deprecated("Not actually deprecated, makes warnings go away", "")
private[macroimpls] trait Compat210Component {
  // Import macros only here, otherwise we collide with the above
  import scala.reflect.macros._
  import blackbox.Context

  val c: Context

  import c.universe._

  implicit final class ContextCompat(self: c.type) {
    def typecheck(tree: Tree): Tree = c.typeCheck(tree)
  }

  implicit final class TypeCompat(self: Type) {
    def dealias: Type = self.normalize
    def decls: MemberScope = self.declarations
  }

  implicit final class SymbolCompat(self: Symbol) {
    def isConstructor: Boolean = self.isMethod && self.asMethod.isConstructor
    def info: Type = self.typeSignature
  }

  implicit final class AnnotationCompat(self: Annotation) {
    def tree: Tree = {
      // Taken from AnnotationInfos.scala (in 2.11.x)
      // Assume that we only have scalaArgs
      val ctorSelection = Select(New(TypeTree(self.tpe)), nme.CONSTRUCTOR)
      c.typecheck(Apply(ctorSelection, self.scalaArgs))
    }
  }
}
