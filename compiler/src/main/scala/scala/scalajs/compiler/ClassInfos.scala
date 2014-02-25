/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.language.implicitConversions

import scala.tools.nsc._

import java.io.{ File, PrintWriter, BufferedOutputStream, FileOutputStream }

trait ClassInfos extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._

  /* This component uses our js.Tree API to build JSON data to be output in
   * the .sjsinfo file. The JSONBuilder object below provides helpers to do so.
   */
  import JSONBuilder._

  class ClassInfoBuilder(val symbol: ClassSymbol) {
    val isStaticModule = ClassInfos.this.isStaticModule(symbol)
    val isInterface = symbol.isInterface
    val isImplClass = symbol.isImplClass
    val isRawJSType = ClassInfos.this.isRawJSType(symbol.tpe)
    val name = symbol.fullName + (
        if (isStaticModule) nme.MODULE_SUFFIX_STRING else "")
    val ancestorCount = symbol.ancestors.count(!_.isInterface)

    def toJSON: js.Tree = {
      obj(
          "name" -> name,
          "ancestorCount" -> ancestorCount,
          "isStaticModule" -> isStaticModule,
          "isInterface" -> isInterface,
          "isImplClass" -> isImplClass,
          "isRawJSType" -> isRawJSType
      )
    }
  }

  /** Helper methods and implicits to build js.Trees in a JSON way. */
  private object JSONBuilder {
    /* JSON trees do not have/need positions, since we don't emit source maps
     * for them. Since all js.Tree constructors require their position as an
     * implicit argument, we put NoPosition in the implicit scope for JSON
     * building.
     */
    implicit val dummyPos = NoPosition

    /** Object construction. */
    def obj(fields: (String, js.Tree)*): js.Tree =
      js.ObjectConstr(fields.map(f => (js.StringLiteral(f._1), f._2)).toList)

    implicit def string2lit(s: String): js.StringLiteral =
      js.StringLiteral(s)

    implicit def int2lit(i: Int): js.IntLiteral =
      js.IntLiteral(i)

    implicit def bool2lit(b: Boolean): js.BooleanLiteral =
      js.BooleanLiteral(b)
  }
}
