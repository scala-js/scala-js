/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.nscplugin

import scala.reflect.internal.Flags

import scala.tools.nsc
import nsc._
import nsc.transform.{InfoTransform, TypingTransformers}

import scala.collection.immutable.ListMap
import scala.collection.mutable

/** Makes the references to inner JS class values explicit.
 *
 *  Roughly, for every inner JS class of the form:
 *  {{{
 *  class Outer {
 *    class Inner extends ParentJSClass
 *  }
 *  }}}
 *  this phase creates a field `Inner\$jsclass` in `Outer` to hold the JS class
 *  value for `Inner`. The rhs of that field is a call to a magic method, used
 *  to retain information that the back-end will need.
 *  {{{
 *  class Outer {
 *    <synthetic> val Inner\$jsclass: AnyRef =
 *      createJSClass(classOf[Inner], js.constructorOf[ParentJSClass])
 *
 *    class Inner extends ParentJSClass
 *  }
 *  }}}
 *
 *  These fields will be read by code generated in `ExplicitLocalJS`.
 *
 *  Note that this field must also be added to outer classes and traits coming
 *  from separate compilation, therefore this phase is an `InfoTransform`.
 *  Since the `transformInfo` also applies to classes defined in the current
 *  compilation unit, the tree traversal must not create the field symbols a
 *  second time when synthesizing the `ValDef`. Instead, it must reuse the same
 *  symbols that `transformInfo` will create.
 *
 *  It seems the easiest way to do that is to run the entire `transform` "in
 *  the future", with `exitingPhase(ExplicitInnerJS)`. This design is similar
 *  to how `explicitouter` works.
 */
abstract class ExplicitInnerJS[G <: Global with Singleton](val global: G)
    extends plugins.PluginComponent with InfoTransform with TypingTransformers
    with CompatComponent {

  val jsAddons: JSGlobalAddons {
    val global: ExplicitInnerJS.this.global.type
  }

  import global._
  import jsAddons._
  import jsInterop.jsclassAccessorFor
  import definitions._
  import rootMirror._
  import jsDefinitions._

  /* The missing 'e' is intentional so that the name of the phase is not longer
   * than the longest standard phase (packageobjects/superaccessors). This
   * avoids destroying the nice formatting of `-Xshow-phases`.
   */
  val phaseName: String = "xplicitinnerjs"

  override def description: String =
    "make references to inner JS classes explicit"

  /** This class does not change linearization. */
  override protected def changesBaseClasses: Boolean = false

  /** Whether vals in traits are represented by their getter.
   *  This is true in 2.12+, since the addition of the `fields` phase.
   *  @see https://github.com/scala/scala/pull/5141
   */
  private lazy val traitValsHoldTheirGetterSymbol =
    !scala.util.Properties.versionNumberString.startsWith("2.11.")

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ExplicitInnerJSTransformer(unit)

  /** Is the given symbol an owner for which this transformation applies?
   *
   *  This applies if it is not a static owner.
   */
  private def isApplicableOwner(sym: Symbol): Boolean =
    !sym.isStaticOwner

  /** Is the given symbol a JS class (that is not a trait nor an object)? */
  private def isJSClass(sym: Symbol): Boolean = {
    sym.isClass &&
    !sym.hasFlag(Flags.TRAIT | Flags.MODULE) &&
    sym.hasAnnotation(JSTypeAnnot)
  }

  /** Transforms the info of types to add the `Inner\$jsclass` fields.
   *
   *  This method was inspired by `ExplicitOuter.transformInfo`.
   */
  def transformInfo(sym: Symbol, tp: Type): Type = tp match {
    case ClassInfoType(parents, decls, clazz) if !clazz.isJava && isApplicableOwner(clazz) =>
      val innerJSClasses = decls.filter(isJSClass)
      if (innerJSClasses.isEmpty) {
        tp
      } else {
        val clazzIsJSClass = clazz.hasAnnotation(JSTypeAnnot)

        val decls1 = decls.cloneScope
        for (innerJSClass <- innerJSClasses) {
          def addAnnotsIfInJSClass(sym: Symbol): Unit = {
            if (clazzIsJSClass) {
              innerJSClass.getAnnotation(JSNameAnnotation).fold {
                sym.addAnnotation(JSNameAnnotation,
                    Literal(Constant(jsInterop.defaultJSNameOf(innerJSClass))))
              } { annot =>
                sym.addAnnotation(annot)
              }
              sym.addAnnotation(ExposedJSMemberAnnot)
            }
          }

          val accessorName: TermName =
            innerJSClass.name.append("$jsclass").toTermName
          val accessorFlags =
            Flags.SYNTHETIC | Flags.ARTIFACT | Flags.STABLE | Flags.ACCESSOR
          val accessor =
            clazz.newMethod(accessorName, innerJSClass.pos, accessorFlags)
          accessor.setInfo(NullaryMethodType(AnyRefTpe))
          addAnnotsIfInJSClass(accessor)
          decls1.enter(accessor)

          if (!clazz.isTrait || !traitValsHoldTheirGetterSymbol) {
            val fieldName = accessorName.append(nme.LOCAL_SUFFIX_STRING)
            val fieldFlags =
              Flags.SYNTHETIC | Flags.ARTIFACT | Flags.PrivateLocal
            val field = clazz
              .newValue(fieldName, innerJSClass.pos, fieldFlags)
              .setInfo(AnyRefTpe)
            addAnnotsIfInJSClass(field)
            decls1.enter(field)
          }
        }
        ClassInfoType(parents, decls1, clazz)
      }

    case PolyType(tparams, restp) =>
      val restp1 = transformInfo(sym, restp)
      if (restp eq restp1) tp else PolyType(tparams, restp1)

    case _ =>
      tp
  }

  class ExplicitInnerJSTransformer(unit: CompilationUnit)
      extends TypingTransformer(unit) {

    /** Execute the whole transformation in the future, exiting this phase. */
    override def transformUnit(unit: CompilationUnit): Unit = {
      global.exitingPhase(currentRun.phaseNamed(phaseName)) {
        super.transformUnit(unit)
      }
    }

    /** The main transformation method. */
    override def transform(tree: Tree): Tree = {
      tree match {
        // Add the ValDefs for inner JS class values
        case Template(parents, self, decls) if isApplicableOwner(currentOwner) =>
          val newDecls = mutable.ListBuffer.empty[Tree]
          atOwner(tree, currentOwner) {
            for (decl <- decls) {
              val declSym = decl.symbol
              if ((declSym ne null) && isJSClass(declSym)) {
                val jsclassAccessor = jsclassAccessorFor(declSym)

                val rhs = if (currentOwner.hasAnnotation(JSNativeAnnotation)) {
                  gen.mkAttributedRef(JSPackage_native)
                } else {
                  val clazzValue = gen.mkClassOf(declSym.tpe_*)
                  val parentTpe =
                    extractSuperTpeFromImpl(decl.asInstanceOf[ClassDef].impl)
                  val superClassCtor = gen.mkNullaryCall(
                      JSPackage_constructorOf, List(parentTpe))
                  gen.mkMethodCall(Runtime_createInnerJSClass,
                      List(clazzValue, superClassCtor))
                }

                if (!currentOwner.isTrait || !traitValsHoldTheirGetterSymbol) {
                  val jsclassField = jsclassAccessor.accessed
                  assert(jsclassField != NoSymbol, jsclassAccessor.fullName)
                  newDecls += localTyper.typedValDef(ValDef(jsclassField, rhs))
                  newDecls += localTyper.typed {
                    val rhs = Select(This(currentClass), jsclassField)
                    DefDef(jsclassAccessor, rhs)
                  }
                } else {
                  newDecls += localTyper.typedValDef(ValDef(jsclassAccessor, rhs))
                }
              }

              newDecls += decl
            }
          }

          val newTemplate =
            treeCopy.Template(tree, parents, self, newDecls.result())
          super.transform(newTemplate)

        case _ =>
          // Taken from ExplicitOuter
          val x = super.transform(tree)
          if (x.tpe eq null) x
          else x.setType(transformInfo(currentOwner, x.tpe))
      }
    }

  }

}
