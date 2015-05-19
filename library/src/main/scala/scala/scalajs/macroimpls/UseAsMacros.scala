/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js API               **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013-2015, LAMP/EPFL   **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-lang.org/     **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package scala.scalajs.macroimpls

import scala.annotation.tailrec
import scala.scalajs.js

import Compat210._

/** Macros for `js.use(x).as[T]`.
 *
 *  This implements a structural typechecker conforming to the JavaScript calling
 *  convention in Scala.js' export and facade system.
 *
 *  @author Tobias Schlatter
 */
private[scalajs] object UseAsMacros {
  // Import macros only here, otherwise we collide with Compat210._
  import scala.reflect.macros._
  import blackbox.Context

  def as_impl[A: c.WeakTypeTag, B <: js.Any: c.WeakTypeTag](
      c: Context { type PrefixType = js.Using[_] }): c.Expr[B] = {
    (new Macros[c.type](c)).as[A, B]
  }

  private class Macros[C <: Context { type PrefixType = js.Using[_] }](val c: C)
      extends JSMembers with Compat210Component {

    import c.universe._

    private val JSNameAnnotation = typeOf[js.annotation.JSName].typeSymbol
    private val JSBracketAccessAnnotation = typeOf[js.annotation.JSBracketAccess].typeSymbol
    private val JSBracketCallAnnotation = typeOf[js.annotation.JSBracketCall].typeSymbol
    private val JSExportAnnotation = typeOf[js.annotation.JSExport].typeSymbol
    private val JSExportAllAnnotation = typeOf[js.annotation.JSExportAll].typeSymbol

    /** Base classes that are allowed in a target type.
     *  These are also the classes whose methods do not need to be provided.
     */
    private val JSObjectAncestors = typeOf[js.Object].baseClasses.toSet

    def as[A: WeakTypeTag, B <: js.Any: WeakTypeTag]: Expr[B] = {
      val trgTpe = verifyTargetType(weakTypeOf[B])
      val srcTpe = weakTypeOf[A]

      val srcSym = srcTpe.typeSymbol

      // Nothing and Null have everything
      if (srcSym != definitions.NothingClass &&
          srcSym != definitions.NullClass) {
        check(srcTpe, trgTpe)
      }

      reify { c.prefix.splice.x.asInstanceOf[B] }
    }

    /** Perform the actual structural typechecking.
     *
     *  Checks if [[srcTpe]] conforms to [[trgTpe]]. Reports errors otherwise.
     */
    private def check(srcTpe: Type, trgTpe: Type): Unit = {
      val rawMembers = rawJSMembers(trgTpe)
      val exports = exportedMembers(srcTpe)

      for ((jsMemberSelection, jsMember) <- rawMembers) {
        jsMemberSelection match {
          case JSNamedMember(name) =>
            exports.get(name).fold {
              val msg = s"$srcTpe does not export a member named $name"
              c.error(c.enclosingPosition, msg)
            } { candidates =>
              if (!candidates.exists(_.conformsTo(jsMember))) {
                val msg = s"$srcTpe does not export " + jsMember.displayStr(name)
                c.error(c.enclosingPosition, msg)
              }
            }

          case JSMemberCall =>
            c.abort(c.enclosingPosition, s"$trgTpe defines an apply method. " +
                "This cannot be implemented by any Scala exported type, since " +
                "it would need to chain Function's prototype.")

          case JSMemberBracketAccess =>
            c.abort(c.enclosingPosition, s"$trgTpe defines a " +
                "@JSMemberBracketAccess method. Existence of such a method " +
                "cannot be statically checked for any Scala exported type.")

          case JSMemberBracketCall =>
            c.abort(c.enclosingPosition, s"$trgTpe defines a " +
                "@JSMemberBracketCall method. Existence of such a method " +
                "cannot be statically checked for any Scala exported type.")
        }
      }
    }

    /** Members that a facade type defines */
    private def rawJSMembers(
        tpe: Type): Iterable[(JSMemberSelection, JSMember)] = {

      def isAPIMember(member: Symbol) = {
        !JSObjectAncestors(member.owner) &&
        !member.isConstructor &&
        member.isMethod &&
        !member.asTerm.isParamWithDefault
      }

      for (member <- tpe.members if isAPIMember(member)) yield {
        val memberMethod = member.asMethod
        (jsMemberSelection(memberMethod), jsMemberFor(tpe, memberMethod))
      }
    }

    /** Returns the way a member of a raw JS type is selected in JS */
    private def jsMemberSelection(sym: MethodSymbol): JSMemberSelection = {
      val annots = memberAnnotations(sym)

      def hasAnnot(annot: Symbol) = annots.exists(annotIs(_, annot))

      if (hasAnnot(JSBracketAccessAnnotation)) {
        JSMemberBracketAccess
      } else if (hasAnnot(JSBracketCallAnnotation)) {
        JSMemberBracketCall
      } else {
        val optAnnot = annots.find(annotIs(_, JSNameAnnotation))
        val optName = optAnnot.flatMap(annotStringArg)

        optName.fold {
          val name = defaultName(sym)
          if (name == "apply") JSMemberCall
          else JSNamedMember(name)
        } { name => JSNamedMember(name) }
      }
    }

    /** Returns all exported members of a type */
    private def exportedMembers(tpe: Type): Map[String, List[JSMember]] = {
      val exports = tpe.baseClasses.flatMap(exportedDecls(tpe, _))

      // Group exports by name
      for {
        (name, elems) <- exports.groupBy(_._1)
      } yield {
        (name, elems.map(_._2).toList)
      }
    }

    /** All exported declarations of a class.
     *  (both @JSExportAll and @JSExport)
     */
    private def exportedDecls(origTpe: Type, sym: Symbol) = {
      require(sym.isClass)

      val exportAll = sym.annotations.exists(annotIs(_, JSExportAllAnnotation))

      for {
        decl <- sym.info.decls if decl.isMethod && !decl.isConstructor
        name <- exportNames(decl.asMethod, exportAll)
      } yield {
        (name, jsMemberFor(origTpe, decl.asMethod))
      }
    }

    /** Get the JS member for a method in [[origTpe]] */
    private def jsMemberFor(origTpe: Type, sym: MethodSymbol): JSMember = {
      sym.info.asSeenFrom(origTpe, sym.owner) match {
        case MethodType(List(param), resultType)
            if resultType.typeSymbol == definitions.UnitClass &&
               sym.name.decodedName.toString.endsWith("_=") =>
          JSSetter(param.info)

        case NullaryMethodType(returnType) =>
          JSGetter(returnType)

        case info: MethodType =>
          @tailrec
          def flatParams(tpe: Type, acc: List[JSMethodParam]): JSMethod = {
            tpe match {
              case MethodType(params, returnTpe) =>
                val ps = params map { p =>
                  JSMethodParam(p.info, p.asTerm.isParamWithDefault)
                }
                flatParams(returnTpe, ps reverse_::: acc)
              case tpe =>
                JSMethod(acc.reverse, tpe)
            }
          }

          flatParams(info, Nil)

        case PolyType(_, _) =>
          c.abort(c.enclosingPosition, "Polymorphic methods are currently " +
              s"not supported. Offending method: ${sym.fullName}")

        case tpe =>
          sys.error(s"Unexpected method type: $tpe for $sym. Report this as a bug.")
      }
    }

    /** Names a method is exported to */
    private def exportNames(sym: MethodSymbol, exportAll: Boolean) = {
      lazy val default = defaultName(sym)

      val explicitNames = for {
        annot <- memberAnnotations(sym)
        if annotIs(annot, JSExportAnnotation)
      } yield {
        annotStringArg(annot).getOrElse(default)
      }

      if (exportAll && sym.isPublic) default :: explicitNames
      else explicitNames
    }

    /** Default JavaScript name of a method */
    private def defaultName(sym: MethodSymbol): String =
      sym.name.decodedName.toString.stripSuffix("_=")

    /** Verifies that the given type is a class type or a refined type,
     *  has no class ancestors lower than js.Object and does only
     *  refine type members.
     *  @returns dealiased tpe
     */
    private def verifyTargetType(tpe: Type): Type = {
      tpe.dealias match {
        case tpe @ TypeRef(_, sym0, _) if sym0.isClass =>
          val sym = sym0.asClass

          if (!sym.isTrait)
            c.abort(c.enclosingPosition, "Only traits can be used with as")

          def allowedParent(sym: Symbol) =
            sym.asClass.isTrait || JSObjectAncestors(sym)

          for (base <- sym.baseClasses if !allowedParent(base)) {
            c.abort(c.enclosingPosition, s"Supertype ${base.fullName} of $sym " +
                "is a class. Cannot be used with as.")
          }

          tpe

        case tpe @ RefinedType(parents, decls) =>
          parents.foreach(verifyTargetType)

          for (decl <- decls if !decl.isType) {
            c.abort(c.enclosingPosition, s"Refinement ${decl.name} " +
                "is not a type. Only types may be refined with as.")
          }

          tpe

        case tpe =>
          c.abort(c.enclosingPosition, "Only class types can be used with as")
      }
    }

    /** Annotations of a member symbol.
     *  Looks on accessed field if this is an accessor
     */
    private def memberAnnotations(sym: MethodSymbol): List[Annotation] = {
      val trgSym = if (sym.isAccessor) sym.accessed else sym

      // Force typeSignature to calculate annotations
      trgSym.typeSignature

      trgSym.annotations
    }

    /** Retrieve first argument to the annotation as literal string */
    private def annotStringArg(annot: Annotation): Option[String] = {
      val args = annot.tree.children.tail
      args match {
        case List(Literal(Constant(s: String))) => Some(s)
        case _                                  => None
      }
    }

    /** Checks if [[annot]] is of class [[clsSym]] */
    private def annotIs(annot: Annotation, clsSym: Symbol) =
      annot.tree.tpe.typeSymbol == clsSym

  }

}
