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
    import scala.reflect.internal.Flags

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

      val directExports = for {
        annot <- trgSym.annotations
        if annot.symbol == JSExportAnnotation
      } yield {
        // Symbol we use to get name from (constructors take name of class)
        val nmeSym = if (sym.isConstructor) sym.owner else sym
        // The actual name of the symbol
        val symNme = nmeSym.unexpandedName.decoded

        // Enforce that methods ending with _= are exported as setters
        if (symNme.endsWith("_=") && !isJSSetter(sym)) {
          currentUnit.error(annot.pos, "A method ending in _= will be " +
              s"exported as setter. But $symNme does not have the right " +
              "signature to do so (single argument, unit return type).")
        }

        val name = annot.stringArg(0).getOrElse(symNme.stripSuffix("_="))

        // Enforce no __ in name
        if (name.contains("__")) {
          // Get position for error message
          val pos = if (annot.stringArg(0).isDefined)
            annot.args.head.pos
          else trgSym.pos

          currentUnit.error(pos,
              "An exported name may not contain a double underscore (`__`)")
        }

        (name, annot.pos)
      }

      val inheritedExports = if (sym.isModuleClass) {
        val forcingSym = sym.ancestors.find(_.annotations.exists(
            _.symbol == JSExportDescendentObjectsAnnotation))

        forcingSym map { fs =>
          val name = sym.fullName

          // Enfore no __ in name
          if (name.contains("__")) {
            // Get all annotation positions for error message
            currentUnit.error(sym.pos,
                s"""${sym.name} may not have a double underscore (`__`) in its fully qualified
                   |name, since it is forced to be exported by a @JSExportDescendentObjects on ${fs}""".stripMargin)
          }

          List((sym.fullName, sym.pos))

        } getOrElse Nil
      } else Nil

      directExports ::: inheritedExports
    }

    /** creates a name for an export specification */
    def scalaExportName(jsName: String, isProp: Boolean): Name = {
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
