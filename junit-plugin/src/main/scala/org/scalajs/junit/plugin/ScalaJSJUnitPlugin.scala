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

package org.scalajs.junit.plugin

import scala.language.reflectiveCalls

import scala.annotation.tailrec

import scala.reflect.internal.Flags
import scala.tools.nsc._
import scala.tools.nsc.plugins.{
  Plugin => NscPlugin, PluginComponent => NscPluginComponent
}

/** The Scala.js JUnit plugin replaces reflection based test lookup.
 *
 *  For each JUnit test `my.pkg.X`, it generates a bootstrapper module/object
 *  `my.pkg.X\$scalajs\$junit\$bootstrapper` implementing
 *  `org.scalajs.junit.Bootstrapper`.
 *
 *  The test runner uses these objects to obtain test metadata and dispatch to
 *  relevant methods.
 */
class ScalaJSJUnitPlugin(val global: Global) extends NscPlugin {

  val name: String = "Scala.js JUnit plugin"

  val components: List[NscPluginComponent] =
    List(ScalaJSJUnitPluginComponent)

  val description: String = "Makes JUnit test classes invokable in Scala.js"

  object ScalaJSJUnitPluginComponent
      extends plugins.PluginComponent with transform.Transform
      with CompatComponent {

    val global: Global = ScalaJSJUnitPlugin.this.global
    import global._
    import definitions._
    import rootMirror.getRequiredClass

    val phaseName: String = "junit-inject"
    val runsAfter: List[String] = List("mixin")
    override val runsBefore: List[String] = List("jscode")

    protected def newTransformer(unit: CompilationUnit): Transformer =
      new ScalaJSJUnitPluginTransformer

    private object JUnitAnnots {
      val Test = getRequiredClass("org.junit.Test")
      val Before = getRequiredClass("org.junit.Before")
      val After = getRequiredClass("org.junit.After")
      val BeforeClass = getRequiredClass("org.junit.BeforeClass")
      val AfterClass = getRequiredClass("org.junit.AfterClass")
      val Ignore = getRequiredClass("org.junit.Ignore")
    }

    private object Names {
      val beforeClass = newTermName("beforeClass")
      val afterClass = newTermName("afterClass")
      val before = newTermName("before")
      val after = newTermName("after")
      val tests = newTermName("tests")
      val invokeTest = newTermName("invokeTest")
      val newInstance = newTermName("newInstance")

      val instance = newTermName("instance")
      val name = newTermName("name")
    }

    private lazy val BootstrapperClass =
      getRequiredClass("org.scalajs.junit.Bootstrapper")

    private lazy val TestMetadataClass =
      getRequiredClass("org.scalajs.junit.TestMetadata")

    class ScalaJSJUnitPluginTransformer extends Transformer {
      override def transform(tree: Tree): Tree = tree match {
        case tree: PackageDef =>
          @tailrec
          def hasTests(sym: Symbol): Boolean = {
            sym.info.members.exists(m => m.isMethod && m.hasAnnotation(JUnitAnnots.Test)) ||
            sym.superClass.exists && hasTests(sym.superClass)
          }

          def isTest(sym: Symbol) = {
            sym.isClass &&
            !sym.isModuleClass &&
            !sym.isAbstract &&
            !sym.isTrait &&
            hasTests(sym)
          }

          val bootstrappers = tree.stats.collect {
            case clDef: ClassDef if isTest(clDef.symbol) =>
              genBootstrapper(clDef.symbol.asClass)
          }

          val newStats = tree.stats.map(transform) ++ bootstrappers
          treeCopy.PackageDef(tree, tree.pid, newStats)

        case tree =>
          super.transform(tree)
      }

      def genBootstrapper(testClass: ClassSymbol): ClassDef = {
        val bootSym = testClass.owner.newModuleClass(
            newTypeName(testClass.name.toString + "$scalajs$junit$bootstrapper"))

        val bootInfo =
          ClassInfoType(List(ObjectTpe, BootstrapperClass.toType), newScope, bootSym)

        bootSym.setInfo(bootInfo)

        val testMethods = annotatedMethods(testClass, JUnitAnnots.Test)

        val defs = List(
            genConstructor(bootSym),
            genCallOnModule(bootSym, Names.beforeClass, testClass.companionModule, JUnitAnnots.BeforeClass),
            genCallOnModule(bootSym, Names.afterClass, testClass.companionModule, JUnitAnnots.AfterClass),
            genCallOnParam(bootSym, Names.before, testClass, JUnitAnnots.Before),
            genCallOnParam(bootSym, Names.after, testClass, JUnitAnnots.After),
            genTests(bootSym, testMethods),
            genInvokeTest(bootSym, testClass, testMethods),
            genNewInstance(bootSym, testClass)
        )

        ClassDef(bootSym, defs)
      }

      private def genConstructor(owner: ClassSymbol): DefDef = {
        val rhs = gen.mkMethodCall(
            Super(owner, tpnme.EMPTY), ObjectClass.primaryConstructor, Nil, Nil)

        val sym = owner.newClassConstructor(NoPosition)
        sym.setInfoAndEnter(MethodType(Nil, owner.tpe))
        typer.typedDefDef(newDefDef(sym, rhs)())
      }

      private def genCallOnModule(owner: ClassSymbol, name: TermName, module: Symbol, annot: Symbol): DefDef = {
        val sym = owner.newMethodSymbol(name)
        sym.setInfoAndEnter(MethodType(Nil, definitions.UnitTpe))

        val calls = annotatedMethods(module, annot)
          .map(gen.mkMethodCall(Ident(module), _, Nil, Nil))
          .toList

        typer.typedDefDef(newDefDef(sym, Block(calls: _*))())
      }

      private def genCallOnParam(owner: ClassSymbol, name: TermName, testClass: Symbol, annot: Symbol): DefDef = {
        val sym = owner.newMethodSymbol(name)

        val instanceParam = sym.newValueParameter(Names.instance).setInfo(ObjectTpe)

        sym.setInfoAndEnter(MethodType(List(instanceParam), definitions.UnitTpe))

        val instance = castParam(instanceParam, testClass)
        val calls = annotatedMethods(testClass, annot)
          .map(gen.mkMethodCall(instance, _, Nil, Nil))
          .toList

        typer.typedDefDef(newDefDef(sym, Block(calls: _*))())
      }

      private def genTests(owner: ClassSymbol, tests: Scope): DefDef = {
        val sym = owner.newMethodSymbol(Names.tests)
        sym.setInfoAndEnter(MethodType(Nil,
            typeRef(NoType, ArrayClass, List(TestMetadataClass.tpe))))

        val metadata = for (test <- tests) yield {
          val reifiedAnnot = New(
              JUnitAnnots.Test, test.getAnnotation(JUnitAnnots.Test).get.args: _*)

          val name = Literal(Constant(test.name.toString))
          val ignored = Literal(Constant(test.hasAnnotation(JUnitAnnots.Ignore)))

          New(TestMetadataClass, name, ignored, reifiedAnnot)
        }

        val rhs = ArrayValue(TypeTree(TestMetadataClass.tpe), metadata.toList)

        typer.typedDefDef(newDefDef(sym, rhs)())
      }

      private def genInvokeTest(owner: ClassSymbol, testClass: Symbol, tests: Scope): DefDef = {
        val sym = owner.newMethodSymbol(Names.invokeTest)

        val instanceParam = sym.newValueParameter(Names.instance).setInfo(ObjectTpe)
        val nameParam = sym.newValueParameter(Names.name).setInfo(StringTpe)

        sym.setInfo(MethodType(List(instanceParam, nameParam), UnitTpe))

        val instance = castParam(instanceParam, testClass)
        val rhs = tests.foldRight[Tree] {
          Throw(New(typeOf[NoSuchMethodException], Ident(nameParam)))
        } { (sym, next) =>
          val cond = gen.mkMethodCall(Ident(nameParam), Object_equals, Nil,
              List(Literal(Constant(sym.name.toString))))

          val call = gen.mkMethodCall(instance, sym, Nil, Nil)

          If(cond, call, next)
        }

        typer.typedDefDef(newDefDef(sym, rhs)())
      }

      private def genNewInstance(owner: ClassSymbol, testClass: ClassSymbol): DefDef = {
        val sym = owner.newMethodSymbol(Names.newInstance)
        sym.setInfoAndEnter(MethodType(Nil, ObjectTpe))
        typer.typedDefDef(newDefDef(sym, New(testClass))())
      }

      private def castParam(param: Symbol, clazz: Symbol): Tree =
        gen.mkAsInstanceOf(Ident(param), clazz.tpe, any = false)

      private def annotatedMethods(owner: Symbol, annot: Symbol): Scope =
        owner.info.members.filter(m => m.isMethod && !m.isBridge && m.hasAnnotation(annot))
    }
  }
}
