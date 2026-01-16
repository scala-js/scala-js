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
import nsc.transform.{Transform, TypingTransformers}

import scala.collection.immutable.ListMap
import scala.collection.mutable

/** Makes the references to local JS classes explicit and desugars calls to
 *  `js.constructorOf`.
 *
 *  It also makes explicit all references to inner JS classes, using the
 *  pointers created by `ExplicitInnerJS`, and otherwise makes sure the
 *  back-end will receive all the information it needs to translate inner- and
 *  local JS classes and objects.
 *
 *  Note that in this comment, by "class" we mean *only* `class`es. `trait`s
 *  and `object`s are not implied.
 *
 *  Similarly to how `ExplicitInnerJS` creates explicit fields in the enclosing
 *  templates of inner JS classes to hold the JS class values, this phase
 *  creates local vals for local JS classes in the enclosing statement list.
 *
 *  For every local JS class of the form:
 *  {{{
 *  def outer() = {
 *    class Local extends ParentJSClass
 *  }
 *  }}}
 *  this phase creates a local `val Local\$jslass` in the body of `outer()` to
 *  hold the JS class value for `Local`. The rhs of that val is a call to a
 *  magic method, used to retain information that the back-end will need:
 *
 *  - A reified reference to `class Local`, in the form of a `classOf`
 *  - An explicit reference to the super JS class value, i.e., the desugaring
 *    of `js.constructorOf[ParentJSClass]`
 *  - An array of fake `new` expressions for all overloaded constructors.
 *
 *  The latter will be augmented by `lambdalift` with the appropriate actual
 *  parameters for the captures of `Local`, which will be needed by the
 *  back-end. In code, this looks like:
 *  {{{
 *  def outer() = {
 *    class Local extends ParentJSClass
 *    val Local\$jsclass: AnyRef = createLocalJSClass(
 *        classOf[Local],
 *        js.constructorOf[ParentJSClass],
 *        Array[AnyRef](new Local(), ...))
 *  }
 *  }}}
 *
 *  Since we need to insert fake `new Inner()`s, this scheme does not work for
 *  abstract local classes. We therefore reject them as implementation
 *  restriction.
 *
 *  If the body of `Local` references itself, then the `val Local\$jsclass` is
 *  instead declared as a `var` to work around the cyclic dependency:
 *  {{{
 *  def outer() = {
 *    var Local\$jsclass: AnyRef = null
 *    class Local extends ParentJSClass {
 *      ...
 *    }
 *    Local\$jsclass = createLocalJSClass(...)
 *  }
 *  }}}
 *
 *  In addition to the above, `ExplicitLocalJS` transforms all *call sites* of
 *  local JS classes *and* inner JS classes, so that they refer to the
 *  synthesized local vals and fields.
 *
 *  The primary transformation is the desugaring of `js.constructorOf[C]`,
 *  which depends on the nature of `C`:
 *
 *  - If `C` is a statically accessible class, desugar to
 *    `runtime.constructorOf(classOf[C])` so that the reified symbol survives
 *    erasure and reaches the back-end.
 *  - If `C` is an inner JS class, it must be of the form `path.D` for some
 *    pair (`path`, `D`), and we desugar it to `path.D\$jsclass`, using the
 *    field created by `ExplicitInnerJS` (it is an error if `C` is of the form
 *    `Enclosing#D`).
 *  - If `C` is a local JS class, desugar to `C\$jsclass`, using the local val
 *    created by this phase.
 *
 *  The other transformations build on top of the desugaring of
 *  `js.constructorOf[C]`, and apply only to inner JS classes and local JS
 *  classes (not for statically accessible classes):
 *
 *  - `x.isInstanceOf[C]` desugars into
 *    `js.special.instanceof(x, js.constructorOf[C])`.
 *  - `new C(...args)` desugars into
 *    `withContextualJSClassValue(js.constructorOf[C], new C(...args))`, so
 *    that the back-end receives a reified reference to the JS class value.
 *  - In the same spirit, for `D extends C`, `D.super.m(...args)` desugars into
 *    `withContextualJSClassValue(js.constructorOf[C], D.super.m(...args))`.
 *
 *  Finally, for inner- and local JS *objects*, their (only) instantiation
 *  point of the form `new O.type()` is rewritten as
 *  `withContextualJSClassValue(js.constructorOf[ParentClassOfO], new O.type())`,
 *  so that the back-end receives a reified reference to the parent class of
 *  `O`. A similar treatment is applied on anonymous JS classes, which
 *  basically define something very similar to an `object`, although without
 *  its own JS class.
 */
abstract class ExplicitLocalJS[G <: Global with Singleton](val global: G)
    extends plugins.PluginComponent with Transform with TypingTransformers
    with CompatComponent {

  val jsAddons: JSGlobalAddons {
    val global: ExplicitLocalJS.this.global.type
  }

  import global._
  import jsAddons._
  import jsInterop.{jsclassAccessorFor, JSCallingConvention}
  import definitions._
  import rootMirror._
  import jsDefinitions._

  /* The missing 'e' is intentional so that the name of the phase is not longer
   * than the longest standard phase (packageobjects/superaccessors). This
   * avoids destroying the nice formatting of `-Xshow-phases`.
   */
  val phaseName: String = "xplicitlocaljs"

  override def description: String =
    "make references to local JS classes explicit"

  protected def newTransformer(unit: CompilationUnit): Transformer =
    new ExplicitLocalJSTransformer(unit)

  /** Is the gen clazz an inner or local JS class? */
  private def isInnerOrLocalJSClass(sym: Symbol): Boolean =
    isInnerJSClass(sym) || isLocalJSClass(sym)

  /** Is the given clazz an inner JS class? */
  private def isInnerJSClass(clazz: Symbol): Boolean =
    isInnerJSClassOrObject(clazz) && !clazz.isModuleClass

  /** Is the given clazz a local JS class? */
  private def isLocalJSClass(clazz: Symbol): Boolean = {
    isLocalJSClassOrObject(clazz) &&
    !clazz.isModuleClass && !clazz.isAnonymousClass
  }

  /** Is the gen clazz an inner or local JS class or object? */
  private def isInnerOrLocalJSClassOrObject(sym: Symbol): Boolean =
    isInnerJSClassOrObject(sym) || isLocalJSClassOrObject(sym)

  /** Is the given clazz an inner JS class or object? */
  private def isInnerJSClassOrObject(clazz: Symbol): Boolean = {
    clazz.hasAnnotation(JSTypeAnnot) &&
    !clazz.isPackageClass && !clazz.outerClass.isStaticOwner &&
    !clazz.isLocalToBlock && !clazz.isTrait
  }

  /** Is the given clazz a local JS class or object? */
  private def isLocalJSClassOrObject(clazz: Symbol): Boolean = {
    def isJSLambda: Boolean = {
      // See GenJSCode.isJSFunctionDef
      clazz.isAnonymousClass &&
      clazz.superClass == JSFunctionClass &&
      clazz.info.decl(nme.apply).filter(JSCallingConvention.isCall(_)).exists
    }

    clazz.isLocalToBlock &&
      !clazz.isTrait && clazz.hasAnnotation(JSTypeAnnot) &&
      !isJSLambda
  }

  class ExplicitLocalJSTransformer(unit: CompilationUnit)
      extends TypingTransformer(unit) {

    private val nestedObject2superClassTpe = mutable.Map.empty[Symbol, Type]
    private val localClass2jsclassVal = mutable.Map.empty[Symbol, TermSymbol]
    private val notYetSelfReferencingLocalClasses = mutable.Set.empty[Symbol]

    override def transformUnit(unit: CompilationUnit): Unit = {
      try {
        super.transformUnit(unit)
      } finally {
        nestedObject2superClassTpe.clear()
        localClass2jsclassVal.clear()
        notYetSelfReferencingLocalClasses.clear()
      }
    }

    /** The main transformation method. */
    override def transform(tree: Tree): Tree = {
      val sym = tree.symbol
      tree match {
        /* Populate `nestedObject2superClassTpe` for inner objects at the start
         * of a `Template`, so that they are visible even before their
         * definition (in their enclosing scope).
         */
        case Template(_, _, decls) =>
          for (decl <- decls) {
            decl match {
              case ClassDef(_, _, _, impl)
                  if decl.symbol.isModuleClass && isInnerJSClassOrObject(decl.symbol) =>
                nestedObject2superClassTpe(decl.symbol) =
                  extractSuperTpeFromImpl(impl)
              case _ =>
            }
          }
          super.transform(tree)

        // Create local `val`s for local JS classes
        case Block(stats, expr) =>
          val newStats = mutable.ListBuffer.empty[Tree]
          for (stat <- stats) {
            stat match {
              case ClassDef(mods, name, tparams, impl) if isLocalJSClass(stat.symbol) =>
                val clazz = stat.symbol
                val jsclassVal = currentOwner
                  .newValue(unit.freshTermName(name.toString() + "$jsname"), stat.pos)
                  .setInfo(AnyRefTpe)
                localClass2jsclassVal(clazz) = jsclassVal
                notYetSelfReferencingLocalClasses += clazz
                val newClassDef = transform(stat)
                val rhs = {
                  val clazzValue = gen.mkClassOf(clazz.tpe_*)
                  val superClassCtor =
                    genJSConstructorOf(tree, extractSuperTpeFromImpl(impl))
                  val fakeNewInstances = {
                    val elems = for {
                      ctor <- clazz.info.decl(nme.CONSTRUCTOR).alternatives
                    } yield {
                      assert(ctor.tpe.paramss.nonEmpty,
                          s"Constructor ${ctor.fullName} has no param list")
                      val argss = ctor.tpe.paramss.map { params =>
                        List.fill(params.size)(gen.mkAttributedRef(Predef_???))
                      }
                      argss.tail.foldLeft(
                          global.NewFromConstructor(ctor, argss.head: _*))(
                          Apply(_, _))
                    }
                    typer.typed(ArrayValue(TypeTree(AnyRefTpe), elems))
                  }
                  gen.mkMethodCall(Runtime_createLocalJSClass,
                      List(clazzValue, superClassCtor, fakeNewInstances))
                }
                if (notYetSelfReferencingLocalClasses.remove(clazz)) {
                  newStats += newClassDef
                  newStats += localTyper.typedValDef {
                    ValDef(jsclassVal, rhs)
                  }
                } else {
                  /* We are using `jsclassVal` inside the definition of the
                   * class. We need to declare it as var before and initialize
                   * it after the class definition.
                   */
                  jsclassVal.setFlag(Flags.MUTABLE)
                  newStats += localTyper.typedValDef {
                    ValDef(jsclassVal, Literal(gen.mkConstantZero(AnyRefTpe)))
                  }
                  newStats += newClassDef
                  newStats += localTyper.typed {
                    Assign(Ident(jsclassVal), rhs)
                  }
                }

              case ClassDef(_, _, _, impl)
                  if isLocalJSClassOrObject(stat.symbol) =>
                nestedObject2superClassTpe(stat.symbol) =
                  extractSuperTpeFromImpl(impl)
                newStats += transform(stat)

              case _ =>
                newStats += transform(stat)
            }
          }
          val newExpr = transform(expr)
          treeCopy.Block(tree, newStats.toList, newExpr)

        /* Wrap `new`s to inner and local JS classes and objects with
         * `withContextualJSClassValue`, to preserve a reified reference to
         * the necessary JS class value (the class itself for classes, or the
         * super class for objects).
         * Anonymous classes are considered as "objects" for this purpose.
         */
        case Apply(sel @ Select(New(tpt), nme.CONSTRUCTOR), args)
            if isInnerOrLocalJSClassOrObject(sel.symbol.owner) =>
          val newCall = super.transform(tree)
          val newTpt = transform(tpt)
          val classSym = sel.symbol.owner
          if (!classSym.isModuleClass && !classSym.isAnonymousClass) {
            val jsclassValue = genJSConstructorOf(newTpt, newTpt.tpe)
            wrapWithContextualJSClassValue(jsclassValue) {
              newCall
            }
          } else {
            wrapWithContextualJSClassValue(nestedObject2superClassTpe(classSym)) {
              newCall
            }
          }

        /* Wrap `super` calls to inner and local JS classes with
         * `withContextualJSClassValue`, to preserve a reified reference to the
         * necessary JS class value (that of the super class).
         */
        case Apply(fun @ Select(sup: Super, _), _)
            if !fun.symbol.isConstructor &&
              isInnerOrLocalJSClass(sup.symbol.superClass) =>
          wrapWithContextualSuperJSClassValue(sup.symbol.superClass) {
            super.transform(tree)
          }

        // Same for a super call with type parameters
        case Apply(TypeApply(fun @ Select(sup: Super, _), _), _)
            if !fun.symbol.isConstructor &&
              isInnerOrLocalJSClass(sup.symbol.superClass) =>
          wrapWithContextualSuperJSClassValue(sup.symbol.superClass) {
            super.transform(tree)
          }

        // Translate js.constructorOf[T]
        case Apply(TypeApply(ctorOfTree, List(tpeArg)), Nil)
            if ctorOfTree.symbol == JSPackage_constructorOf =>
          val newTpeArg = transform(tpeArg)
          gen.mkAttributedCast(genJSConstructorOf(tree, newTpeArg.tpe),
              JSDynamicClass.tpe)

        // Translate x.isInstanceOf[T] for inner and local JS classes
        case Apply(TypeApply(fun @ Select(obj, _), List(tpeArg)), Nil)
            if fun.symbol == Any_isInstanceOf &&
              isInnerOrLocalJSClass(tpeArg.tpe.typeSymbol) =>
          val newObj = transform(obj)
          val newTpeArg = transform(tpeArg)
          val jsCtorOf = genJSConstructorOf(tree, newTpeArg.tpe)
          atPos(tree.pos) {
            localTyper.typed {
              gen.mkMethodCall(Special_instanceof, List(newObj, jsCtorOf))
            }
          }

        case _ =>
          super.transform(tree)
      }
    }

    /** Generates the desugared version of `js.constructorOf[tpe]`.
     */
    private def genJSConstructorOf(tree: Tree, tpe: Type): Tree = {
      val clazz = tpe.typeSymbol

      // This should not have passed the checks in PrepJSInterop
      assert(!clazz.isTrait && !clazz.isModuleClass,
          s"non-trait class type required but $tpe found for " +
          s"genJSConstructorOf at ${tree.pos}")

      localTyper.typed {
        atPos(tree.pos) {
          if (isInnerJSClass(clazz)) {
            // Use the $jsclass field in the outer instance
            val prefix = tpe.prefix match {
              case NoPrefix => clazz.outerClass.thisType
              case x        => x
            }
            if (prefix.isStable) {
              val qual = gen.mkAttributedQualifier(prefix)
              gen.mkAttributedSelect(qual, jsclassAccessorFor(clazz))
            } else {
              reporter.error(tree.pos,
                  s"stable reference to a JS class required but $tpe found")
              gen.mkAttributedRef(Predef_???)
            }
          } else if (isLocalJSClass(clazz)) {
            // Use the local `val` that stores the JS class value
            val jsclassVal = localClass2jsclassVal(clazz)
            notYetSelfReferencingLocalClasses.remove(clazz)
            gen.mkAttributedIdent(jsclassVal)
          } else {
            // Defer translation to `LoadJSConstructor` to the back-end
            val classValue = gen.mkClassOf(tpe)
            gen.mkMethodCall(Runtime_constructorOf, List(classValue))
          }
        }
      }
    }

    /** Wraps with the contextual super JS class value for super calls. */
    private def wrapWithContextualSuperJSClassValue(superClass: Symbol)(
        tree: Tree): Tree = {
      /* #4801 We need to interpret the superClass type as seen from the
       * current class' thisType.
       *
       * For example, in the test NestedJSClassTest.extendInnerJSClassInClass,
       * the original `superClass.tpe_*` is
       *
       *   OuterNativeClass_Issue4402.this.InnerClass
       *
       * because `InnerClass` is path-dependent. However, the path
       * `OuterNativeClass.this` is only valid within `OuterNativeClass`
       * itself. In the context of the current local class `Subclass`, this
       * path must be replaced by the actual path `outer.`. This is precisely
       * the role of `asSeenFrom`. We tell it to replace any `superClass.this`
       * by `currentClass.this`, and it also transitively replaces paths for
       * outer classes of `superClass`, matching them with the corresponding
       * outer paths of `currentClass.thisType` if necessary. The result for
       * that test case is
       *
       *   outer.InnerClass
       */
      val jsClassTypeInSuperClass = superClass.tpe_*
      val jsClassTypeAsSeenFromThis =
        jsClassTypeInSuperClass.asSeenFrom(currentClass.thisType, superClass)

      wrapWithContextualJSClassValue(jsClassTypeAsSeenFromThis) {
        tree
      }
    }

    private def wrapWithContextualJSClassValue(jsClassType: Type)(
        tree: Tree): Tree = {
      wrapWithContextualJSClassValue(genJSConstructorOf(tree, jsClassType)) {
        tree
      }
    }

    private def wrapWithContextualJSClassValue(jsClassValue: Tree)(
        tree: Tree): Tree = {
      atPos(tree.pos) {
        localTyper.typed {
          gen.mkMethodCall(
              Runtime_withContextualJSClassValue,
              List(tree.tpe),
              List(jsClassValue, tree))
        }
      }
    }

  }

}
