/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.tools.nsc._

/** Additions to Global meaningful for the JavaScript backend
 *
 *  @author Sébastien Doeraene
 */
trait JSGlobalAddons extends JSTrees
                        with JSPrinters
                        with JSDefinitions
                        with JSTreeExtractors
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

    private val exportPrefix = "$js$exported$"
    private val methodExportPrefix = exportPrefix + "meth$"
    private val propExportPrefix = exportPrefix + "prop$"

    /** retrieves the names a sym should be exported to from its annotations
     *
     *  Note that for accessor symbols, the annotations of the accessed symbol
     *  are used, rather than the annotations of the accessor itself.
     */
    def exportsOf(sym: Symbol): List[(String, Position)] = {
      val trgSym = {
        // For accessors, look on the val/var def
        if (sym.isAccessor) sym.accessed
        // For primary class constructors, look on the class itself
        else if (sym.isPrimaryConstructor && !sym.owner.isModuleClass) sym.owner
        else sym
      }

      def defaultName = {
        val nmeSym = if (sym.isConstructor) sym.owner else sym
        val decN = nmeSym.unexpandedName.decoded
        if (isJSSetter(sym))
          decN.stripSuffix("_=")
        else
          decN
      }

      val directExports = for {
        annot <- trgSym.annotations
        if annot.symbol == JSExportAnnotation
      } yield {
        val name = annot.stringArg(0).getOrElse(defaultName)
        (name, annot.pos)
      }

      val inheritedExports = if (sym.isModuleClass) {
        if (sym.ancestors.exists(_.annotations.exists(
            _.symbol == JSExportDescendentObjectsAnnotation)))
          List((sym.fullName, sym.pos))
        else
          Nil
      } else Nil

      directExports ::: inheritedExports
    }

    /** creates a name for an export specification */
    def scalaExportName(jsName: String, isProp: Boolean): Name = {
      val pref = if (isProp) propExportPrefix else methodExportPrefix
      val encname = NameTransformer.encode(jsName)
      newTermName(pref + encname)
    }

    /** checks if the given name is a JSExport */
    def isExportName(name: Name): Boolean = name.startsWith(exportPrefix)

    /** checks if the given symbol is a JSExport */
    def isExport(sym: Symbol): Boolean = isExportName(sym.unexpandedName)

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
