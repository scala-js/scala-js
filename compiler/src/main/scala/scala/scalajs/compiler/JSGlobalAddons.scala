/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

import scala.collection.mutable

/** Additions to Global meaningful for the JavaScript backend
 *
 *  @author Sébastien Doeraene
 */
trait JSGlobalAddons extends JSDefinitions
                        with Compat210Component {
  val global: Global

  import global._
  import jsDefinitions._
  import definitions._

  /** JavaScript primitives, used in jscode */
  object jsPrimitives extends JSPrimitives {
    val global: JSGlobalAddons.this.global.type = JSGlobalAddons.this.global
    val jsAddons: ThisJSGlobalAddons =
      JSGlobalAddons.this.asInstanceOf[ThisJSGlobalAddons]
  }

  /** global javascript interop related helpers */
  object jsInterop {
    import scala.reflect.NameTransformer
    import scala.reflect.internal.Flags

    private val exportPrefix = "$js$exported$"
    private val methodExportPrefix = exportPrefix + "meth$"
    private val propExportPrefix = exportPrefix + "prop$"

    case class ExportInfo(jsName: String, pos: Position,
        isNamed: Boolean, inferredShortName: Boolean)

    /** retrieves the names a sym should be exported to from its annotations
     *
     *  Note that for accessor symbols, the annotations of the accessed symbol
     *  are used, rather than the annotations of the accessor itself.
     */
    def exportsOf(sym: Symbol): List[ExportInfo] = {
      val exports = directExportsOf(sym) ++ inheritedExportsOf(sym)

      // Calculate the distinct exports for this symbol (eliminate double
      // occurrences of (name, isNamed) pairs).
      val buf = new mutable.ListBuffer[ExportInfo]
      val seen = new mutable.HashSet[(String, Boolean)]
      for (exp <- exports) {
        if (!seen.contains((exp.jsName, exp.isNamed))) {
          buf += exp
          seen += ((exp.jsName, exp.isNamed))
        }
      }

      buf.toList
    }

    private def directExportsOf(sym: Symbol): List[ExportInfo] = {
      val trgSym = {
        // For accessors, look on the val/var def
        if (sym.isAccessor) sym.accessed
        // For primary class constructors, look on the class itself
        else if (sym.isPrimaryConstructor && !sym.owner.isModuleClass) sym.owner
        else sym
      }

      // Annotations that are directly on the member
      val directAnnots = for {
        annot <- trgSym.annotations
        if annot.symbol == JSExportAnnotation ||
           annot.symbol == JSExportNamedAnnotation
      } yield annot

      // Annotations for this member on the whole unit
      val unitAnnots = {
        if (sym.isMethod && sym.isPublic && !sym.isConstructor)
          sym.owner.annotations.filter(_.symbol == JSExportAllAnnotation)
        else
          Nil
      }

      for {
        annot <- directAnnots ++ unitAnnots
      } yield {
        // Is this a named export or a normal one?
        val named = annot.symbol == JSExportNamedAnnotation

        def explicitName = annot.stringArg(0).getOrElse {
          reporter.error(annot.pos,
            s"The argument to ${annot.symbol.name} must be a literal string")
          "dummy"
        }

        val name =
          if (annot.args.nonEmpty) explicitName
          else if (sym.isConstructor) sym.owner.unexpandedName.decoded
          else if (sym.isModuleClass) sym.unexpandedName.decoded
          else sym.unexpandedName.decoded.stripSuffix("_=")

        // Enforce that methods ending with _= are exported as setters
        if (sym.isMethod && !sym.isConstructor &&
          sym.name.decoded.endsWith("_=") && !isJSSetter(sym)) {
          reporter.error(annot.pos, "A method ending in _= will be exported " +
              s"as setter. But ${sym.name.decoded} does not have the right " +
              "signature to do so (single argument, unit return type).")
        }

        // Enforce no __ in name
        if (name.contains("__")) {
          // Get position for error message
          val pos = if (annot.stringArg(0).isDefined)
            annot.args.head.pos
          else trgSym.pos

          reporter.error(pos,
              "An exported name may not contain a double underscore (`__`)")
        }

        // Make sure we do not override the default export of toString
        if (!sym.isConstructor && name == "toString" && !named &&
            sym.name != nme.toString_ && sym.tpe.params.isEmpty &&
            !isJSGetter(sym)) {
          reporter.error(annot.pos, "You may not export a zero-argument " +
              "method named other than 'toString' under the name 'toString'")
        }

        if (named && isJSProperty(sym)) {
          reporter.error(annot.pos,
              "You may not export a getter or a setter as a named export")
        }

        ExportInfo(name, annot.pos, named,
            inferredShortName = annot.args.isEmpty)
      }
    }

    private def inheritedExportsOf(sym: Symbol): List[ExportInfo] = {
      // The symbol from which we (potentially) inherit exports. It also
      // gives the exports their name
      val trgSym = {
        if (sym.isModuleClass)
          sym
        else if (sym.isConstructor && sym.isPublic &&
            sym.owner.isConcreteClass && !sym.owner.isModuleClass)
          sym.owner
        else NoSymbol
      }

      if (trgSym == NoSymbol) {
        Nil
      } else {
        val trgAnnot =
          if (sym.isModuleClass) JSExportDescendentObjectsAnnotation
          else JSExportDescendentClassesAnnotation

        val forcingSym =
          trgSym.ancestors.find(_.annotations.exists(_.symbol == trgAnnot))

        val name = decodedFullName(trgSym)

        forcingSym.map { fs =>
          // Enfore no __ in name
          if (name.contains("__")) {
            // Get all annotation positions for error message
            reporter.error(sym.pos,
                s"""${trgSym.name} may not have a double underscore (`__`) in its fully qualified
                   |name, since it is forced to be exported by a @${trgAnnot.name} on ${fs}""".stripMargin)
          }

          ExportInfo(name, sym.pos, isNamed = false, inferredShortName = false)
        }.toList
      }
    }

    /** Just like sym.fullName, but does not encode components */
    private def decodedFullName(sym: Symbol): String = {
      if (sym.isRoot || sym.isRootPackage || sym == NoSymbol) sym.name.decoded
      else if (sym.owner.isEffectiveRoot) sym.name.decoded
      else decodedFullName(sym.effectiveOwner.enclClass) + '.' + sym.name.decoded
    }

    /** creates a name for an export specification */
    def scalaExportName(jsName: String, isProp: Boolean): TermName = {
      val pref = if (isProp) propExportPrefix else methodExportPrefix
      val encname = NameTransformer.encode(jsName)
      newTermName(pref + encname)
    }

    /** checks if the given symbol is a JSExport */
    def isExport(sym: Symbol): Boolean =
      sym.unexpandedName.startsWith(exportPrefix) &&
      !sym.hasFlag(Flags.DEFAULTPARAM)

    /** retrieves the originally assigned jsName of this export and whether it
     *  is a property
     */
    def jsExportInfo(name: Name): (String, Boolean) = {
      def dropPrefix(prefix: String) ={
        if (name.startsWith(prefix)) {
          // We can't decode right away due to $ separators
          val enc = name.encoded.substring(prefix.length)
          Some(NameTransformer.decode(enc))
        } else None
      }

      dropPrefix(methodExportPrefix).map((_,false)) orElse
      dropPrefix(propExportPrefix).map((_,true)) getOrElse
      sys.error("non-exported name passed to jsInfoSpec")
    }

    def isJSProperty(sym: Symbol): Boolean = isJSGetter(sym) || isJSSetter(sym)

    /** has this symbol to be translated into a JS getter (both directions)? */
    def isJSGetter(sym: Symbol): Boolean = {
      sym.tpe.params.isEmpty && enteringPhase(currentRun.uncurryPhase) {
        sym.tpe.isInstanceOf[NullaryMethodType]
      }
    }

    /** has this symbol to be translated into a JS setter (both directions)? */
    def isJSSetter(sym: Symbol) = {
      sym.unexpandedName.decoded.endsWith("_=") &&
      sym.tpe.resultType.typeSymbol == UnitClass &&
      enteringPhase(currentRun.uncurryPhase) {
        sym.tpe.paramss match {
          case List(List(arg)) => !isScalaRepeatedParamType(arg.tpe)
          case _ => false
        }
      }
    }

    /** has this symbol to be translated into a JS bracket access (JS to Scala) */
    def isJSBracketAccess(sym: Symbol) =
      sym.hasAnnotation(JSBracketAccessAnnotation)

  }

}
