/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.macroimpls

import Compat210._

/** JSMember is an ADT more or less equivalent to Scala's MethodType
 *  that allows to distinguish setters and getters from methods.
 *
 *  It also allows to check method conformance based on Scala.js' JavaScript
 *  calling conventions.
 *
 *  Currently does not support polymorphic method types.
 *
 *  @author Tobias Schlatter
 */
private[macroimpls] trait JSMembers {
  // Import macros only here, otherwise we collide with Compat210._
  import scala.reflect.macros._
  import blackbox.Context

  val c: Context

  import c.universe._

  sealed trait JSMember {
    /** Whether this JSMember conforms to that JSMember */
    def conformsTo(that: JSMember): Boolean

    /** Create a display string of this member with a given name */
    def displayStr(name: String): String
  }

  case class JSMethodParam(info: Type, isDefault: Boolean) {
    def conformsTo(that: JSMethodParam): Boolean =
      (!that.isDefault || this.isDefault) && that.info <:< this.info

    override def toString(): String =
      if (isDefault) s"$info = ???"
      else info.toString
  }

  case class JSMethod(params: List[JSMethodParam],
      resultType: Type) extends JSMember {

    def conformsTo(that: JSMember): Boolean = that match {
      case JSMethod(thatParams, thatResultType) =>
        val (used, unused) = params.splitAt(thatParams.size)

        params.size >= thatParams.size &&
        resultType <:< thatResultType &&
        unused.forall(_.isDefault) &&
        (used zip thatParams).forall { case (x, y) => x.conformsTo(y) }

      case _ =>
        false
    }

    def displayStr(name: String): String =
      s"method $name(${params.mkString(", ")}): $resultType"
  }

  case class JSGetter(tpe: Type) extends JSMember {
    def conformsTo(that: JSMember): Boolean = that match {
      case JSGetter(thatTpe) => tpe <:< thatTpe
      case _                 => false
    }

    def displayStr(name: String): String = s"getter $name: $tpe"
  }

  case class JSSetter(tpe: Type) extends JSMember {
    def conformsTo(that: JSMember): Boolean = that match {
      case JSSetter(thatTpe) => thatTpe <:< tpe
      case _                 => false
    }

    def displayStr(name: String): String = s"setter $name: $tpe"
  }

  /** Place holder for unsupported members.
   *
   *  In source type position, these members can be ignored.
   *  In target type position, these members will trigger errors.
   */
  case class UnsupportedMember(sym: Symbol, tpe: Type) extends JSMember {
    def conformsTo(that: JSMember): Boolean = false
    def displayStr(name: String): String = s"unsupported $name ($sym)"
  }
}
