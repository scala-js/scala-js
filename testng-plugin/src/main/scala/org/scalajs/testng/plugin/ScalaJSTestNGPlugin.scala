package org.scalajs.testng.plugin

import scala.language.reflectiveCalls

import scala.reflect.internal.Flags
import scala.tools.nsc._
import scala.tools.nsc.plugins.{
  Plugin => NscPlugin, PluginComponent => NscPluginComponent
}

/** The Scala.js TestNG plugin is a way to overcome the lack of annotation
 *  information of any test class (usually accessed through reflection).
 *  This is all the information required by the Scala.js testing framework to
 *  execute the tests.
 *
 *  As an example we take the following test class:
 *  {{{
 *  class Foo {
 *    @BeforeMethod def before(): Unit = {
 *      // Initialize the instance before the tests
 *    }
 *    @Test def bar(): Unit = {
 *      // assert some stuff
 *    }
 *    @Test(enabled = false) def baz(): Unit = {
 *      // assert some other stuff
 *    }
 *  }
 *
 *  object Foo {
 *    @BeforeClass def beforeClass(): Unit = {
 *      // Initialize some global state for the tests.
 *    }
 *  }
 *  }}}
 *
 *  Will generate the following bootstrapper module:
 *
 *  {{{
 *  object Foo\$scalajs\$testng\$bootstrapper extends org.scalajs.testng.TestNGTestBootstrapper {
 *
 *    def metadata(): TestNGClassMetadata = {
 *      new TestNGClassMetadata(
 *        classAnnotations = List(),
 *        moduleAnnotations = List(),
 *        classMethods = List(
 *            new TestNGMethodMetadata(name = "before",
 *                annotations = List(new BeforeMethod)),
 *            new TestNGMethodMetadata(name = "bar",
 *                annotations = List(new Test)),
 *            new TestNGMethodMetadata(name = "baz",
 *                annotations = List(new Test(enabled = false)))
 *        ),
 *        moduleMethods(
 *            new TestNGMethodMetadata(name = "beforeClass",
 *                annotations = List(new BeforeClass)))
 *      )
 *    }
 *
 *    def newInstance(): AnyRef = new Foo()
 *
 *    def invoke(methodName: String): Unit = {
 *      if (methodName == "0") Foo.beforeClass()
 *      else throw new NoSuchMethodException(methodId)
 *    }
 *
 *    def invoke(instance: AnyRef, methodName: String): Unit = {
 *      if (methodName == "before") instance.asInstanceOf[Foo].before()
 *      else if (methodName == "bar") instance.asInstanceOf[Foo].bar()
 *      else if (methodName == "baz") instance.asInstanceOf[Foo].baz()
 *      else throw new NoSuchMethodException(methodId)
 *    }
 *  }
 *  }}}
 *  The test framework will identify `Foo\$scalajs\$testng\$bootstrapper` as a test module
 *  because it extends `TestNGTestBootstrapper`. It will know which methods to run based
 *  on the info returned by Foo\$scalajs\$testng\$bootstrapper.metadata,
 *  it will create new test instances using `Foo\$scalajs\$testng\$bootstrapper.newInstance()`
 *  and it will invoke test methods using `invoke` on the bootstrapper.
 */
class ScalaJSTestNGPlugin(val global: Global) extends NscPlugin {

  val name: String = "Scala.js TestNG plugin"

  val components: List[NscPluginComponent] =
    List(ScalaJSTestNGPluginComponent)

  val description: String = "Makes TestNG test classes invokable in Scala.js"

  // `ScalaJSPlugin` instance reference. Only `registerModuleExports` is accessible.
  private lazy val scalaJSPlugin = {
    type ScalaJSPlugin = NscPlugin {
      def registerModuleExports(sym: ScalaJSTestNGPluginComponent.global.Symbol): Unit
    }
    global.plugins.collectFirst {
      case pl if pl.getClass.getName == "org.scalajs.core.compiler.ScalaJSPlugin" =>
        pl.asInstanceOf[ScalaJSPlugin]
    }.getOrElse {
      throw new Exception(
          "The Scala.js TestNG plugin only works with the Scala.js plugin enabled.")
    }
  }

  object ScalaJSTestNGPluginComponent
      extends plugins.PluginComponent with transform.Transform with Compat210Component {

    val global: Global = ScalaJSTestNGPlugin.this.global
    import global._

    val phaseName: String = "testng-inject"
    val runsAfter: List[String] = List("mixin")
    override val runsBefore: List[String] = List("jscode")

    protected def newTransformer(unit: CompilationUnit): Transformer =
      new ScalaJSTestNGPluginTransformer

    class ScalaJSTestNGPluginTransformer extends Transformer {

      import rootMirror.getRequiredClass

      private val TestClass =
        getRequiredClass("org.testng.annotations.Test")

      private val annotationWhiteList = List(
        TestClass,
        getRequiredClass("org.testng.annotations.DataProvider"),
        getRequiredClass("org.testng.annotations.BeforeMethod"),
        getRequiredClass("org.testng.annotations.AfterMethod"),
        getRequiredClass("org.testng.annotations.BeforeClass"),
        getRequiredClass("org.testng.annotations.AfterClass")
      )

      private val testNGClassMetadataType =
        getRequiredClass("org.scalajs.testng.TestNGClassMetadata").toType

      private val testNGTestMetadataType =
        getRequiredClass("org.scalajs.testng.TestNGTestBootstrapper").toType

      private def testNGMethodMetadataTypeTree =
        TypeTree(getRequiredClass("org.scalajs.testng.TestNGMethodMetadata").toType)

      override def transform(tree: Tree): Tree = tree match {
        case tree: PackageDef =>
          def isClassWithTestNGAnnotation(sym: Symbol): Boolean = sym match {
            case _:ClassSymbol | _:ModuleSymbol =>
              val hasAnnotationInClass = sym.selfType.members.exists {
                case mtdSym: MethodSymbol => hasAnnotation(mtdSym, TestClass)
                case _ => false
              }
              if (hasAnnotationInClass) true
              else sym.parentSymbols.headOption.fold(false)(isClassWithTestNGAnnotation)

            case _ => false
          }

          val bootstrappers = tree.stats.groupBy { // Group the class with its module
            case clDef: ClassDef => Some(clDef.name)
            case _               => None
          }.iterator.flatMap {
            case (Some(_), xs) if xs.exists(x => isClassWithTestNGAnnotation(x.symbol)) =>
              def isModule(cDef: ClassDef): Boolean =
                cDef.mods.hasFlag(Flags.MODULE)
              def isTestClass(cDef: ClassDef): Boolean = {
                !cDef.mods.hasFlag(Flags.MODULE) &&
                !cDef.mods.hasFlag(Flags.ABSTRACT) &&
                !cDef.mods.hasFlag(Flags.TRAIT)
              }
              // Get the class definition and do the transformation
              xs.collectFirst {
                case clDef: ClassDef if isTestClass(clDef) =>
                  // Get the module definition
                  val modDefOption = xs collectFirst {
                    case clDef: ClassDef if isModule(clDef) => clDef
                  }
                  // Create a new module for the TestNG entry point.
                  mkBootstrapperClass(clDef, modDefOption)
              }

            case (_, xs) => None
          }

          val newStats = tree.stats.map(transform) ++ bootstrappers

          treeCopy.PackageDef(tree: Tree, tree.pid, newStats.toList)

        case _ =>
          super.transform(tree)
      }

      def mkBootstrapperClass(clazz: ClassDef, modDefOption: Option[ClassDef]): ClassDef = {
        val bootSym = clazz.symbol.cloneSymbol
        val getTestNGMetadataDef = mkGetTestNGMetadataDef(clazz.symbol,
            modDefOption.map(_.symbol))
        val newInstanceDef = genNewInstanceDef(clazz.symbol, bootSym)
        val invokeTestNGMethodDef = {
          val annotatedMethods = modDefOption.fold(List.empty[MethodSymbol]) { mod =>
            testNGAnnotatedMethods(mod.symbol.asClass)
          }
          mkInvokeTestNGMethodOnModuleDef(annotatedMethods, bootSym,
              modDefOption.map(_.symbol))
        }
        val invokeTestNGMethodOnInstanceDef = {
          val annotatedMethods = testNGAnnotatedMethods(clazz.symbol.asClass)
          mkInvokeTestNGMethodOnInstanceDef(annotatedMethods, bootSym,
              clazz.symbol)
        }

        val bootBody = {
          List(getTestNGMetadataDef, newInstanceDef, invokeTestNGMethodDef,
              invokeTestNGMethodOnInstanceDef)
        }
        val bootParents = List(
          TypeTree(definitions.ObjectTpe),
          TypeTree(testNGTestMetadataType)
        )
        val bootImpl =
          treeCopy.Template(clazz.impl, bootParents, clazz.impl.self, bootBody)

        val bootName = newTypeName(clazz.name.toString + "$scalajs$testng$bootstrapper")
        val bootClazz = gen.mkClassDef(Modifiers(Flags.MODULE),
            bootName, Nil, bootImpl)
        bootSym.flags += Flags.MODULE
        bootSym.withoutAnnotations
        bootSym.setName(bootName)
        val newClazzInfo = {
          val newParentsInfo = List(
            definitions.ObjectTpe,
            testNGTestMetadataType
          )
          val decls = bootSym.info.decls
          decls.enter(getTestNGMetadataDef.symbol)
          decls.enter(newInstanceDef.symbol)
          decls.enter(invokeTestNGMethodDef.symbol)
          decls.enter(invokeTestNGMethodOnInstanceDef.symbol)
          ClassInfoType(newParentsInfo, decls, bootSym.info.typeSymbol)
        }
        bootSym.setInfo(newClazzInfo)
        scalaJSPlugin.registerModuleExports(bootSym)
        bootClazz.setSymbol(bootSym)

        currentRun.symSource(bootSym) = clazz.symbol.sourceFile

        bootClazz
      }

      def testNGAnnotatedMethods(sym: Symbol): List[MethodSymbol] = {
        sym.selfType.members.collect {
          case m: MethodSymbol if hasTestNGMethodAnnotation(m) => m
        }.toList
      }

      /** This method generates a method that invokes a test method in the module
       *  given its name. These methods have no parameters.
       *
       *  Example:
       *  {{{
       *  object Foo {
       *    @BeforeClass def bar(): Unit
       *    @AfterClass def baz(): Unit
       *  }
       *  object Foo\$scalajs\$testng\$bootstrapper {
       *    // This is the method generated by mkInvokeTestNGMethodOnModuleDef
       *    def invoke(methodName: String): Unit = {
       *      if (methodName == "bar") Foo.bar()
       *      else if (methodName == "baz") Foo.baz()
       *      else throw new NoSuchMethodException(methodName + " not found")
       *    }
       *  }
       *  }}}
       */
      def mkInvokeTestNGMethodOnModuleDef(methods: List[MethodSymbol],
          bootSym: Symbol, modClassSym: Option[Symbol]): DefDef = {
        val invokeTestNGMethodSym = bootSym.newMethod(newTermName("invoke"))

        val paramSyms = {
          val params = List(("methodName", definitions.StringTpe))
          mkParamSymbols(invokeTestNGMethodSym, params)
        }

        invokeTestNGMethodSym.setInfo(MethodType(paramSyms, definitions.UnitTpe))

        def callLocally(methodSymbol: Symbol): Tree = {
          val methodSymbolLocal = {
            modClassSym.fold(methodSymbol) { sym =>
              methodSymbol.cloneSymbol(newOwner = sym)
            }
          }
          gen.mkMethodCall(methodSymbolLocal, Nil)
        }

        val invokeTestNGMethodRhs = mkMethodResolutionAndCall(invokeTestNGMethodSym,
            methods, paramSyms.head, callLocally)

        mkMethod(invokeTestNGMethodSym, invokeTestNGMethodRhs, paramSyms)
      }

      /** This method generates a method that invokes a test method in the class
       *  given its name. These methods have no parameters.
       *
       *  Example:
       *  {{{
       *  class Foo {
       *    @Test def bar(): Unit
       *    @Test def baz(): Unit
       *  }
       *  object Foo\$scalajs\$testng\$bootstrapper {
       *    // This is the method generated by mkInvokeTestNGMethodOnInstanceDef
       *    def invoke(instance: AnyRef, methodName: String): Unit = {
       *      if (methodName == "bar") instance.asInstanceOf[Foo].bar()
       *      else if (methodName == "baz") instance.asInstanceOf[Foo].baz()
       *      else throw new NoSuchMethodException(methodName + " not found")
       *    }
       *  }
       *  }}}
       */
      def mkInvokeTestNGMethodOnInstanceDef(methods: List[MethodSymbol],
          classSym: Symbol, refClassSym: Symbol): DefDef = {
        val invokeTestNGMethodSym = classSym.newMethod(newTermName("invoke"))

        val paramSyms = {
          val params = List(("instance", definitions.ObjectTpe),
            ("methodName", definitions.StringTpe))
          mkParamSymbols(invokeTestNGMethodSym, params)
        }

        val instanceParamSym :: idParamSym :: Nil = paramSyms

        invokeTestNGMethodSym.setInfo(MethodType(paramSyms, definitions.UnitTpe))

        def callLocally(methodSymbol: Symbol): Tree = {
          val instance = gen.mkAttributedIdent(instanceParamSym)
          val castedInstance = gen.mkAttributedCast(instance, refClassSym.tpe)
          gen.mkMethodCall(castedInstance, methodSymbol, Nil, Nil)
        }

        val invokeTestNGMethodRhs = mkMethodResolutionAndCall(invokeTestNGMethodSym,
          methods, idParamSym, callLocally)

        mkMethod(invokeTestNGMethodSym, invokeTestNGMethodRhs, paramSyms)
      }

      def mkGetTestNGMetadataDef(clSym: Symbol,
          modSymOption: Option[Symbol]): DefDef = {
        val methods = testNGAnnotatedMethods(clSym)
        val modMethods = modSymOption.map(testNGAnnotatedMethods)

        def liftAnnotations(methodSymbol: Symbol): List[Tree] = {
          val annotations = methodSymbol.annotations

          // Find and report unsupported TestNG annotations
          annotations.foreach {
            case ann if ann.atp.typeSymbol == TestClass && ann.original.isInstanceOf[Block] =>
              reporter.error(ann.pos, "@Test(timeout = ...) is not " +
                "supported in Scala.js TestNG Framework")

            case _ => // all is well
          }

          // Collect lifted representations of the TestNG annotations
          annotations.collect {
            case ann if annotationWhiteList.contains(ann.tpe.typeSymbol) =>
              val args = if (ann.args != null) ann.args else Nil
              mkNewInstance(TypeTree(ann.tpe), args)
          }
        }

        def defaultMethodMetadata(tpe: TypeTree)(mtdSym: MethodSymbol): Tree = {
          val annotations = liftAnnotations(mtdSym)
          mkNewInstance(tpe, List(
              Literal(Constant(mtdSym.name.toString)),
              mkList(annotations)))
        }

        def mkList(elems: List[Tree]): Tree = {
          val array = ArrayValue(TypeTree(definitions.ObjectTpe), elems)
          val wrappedArray = gen.mkMethodCall(
              definitions.PredefModule,
              definitions.wrapArrayMethodName(definitions.ObjectTpe),
              Nil, List(array))
          gen.mkMethodCall(definitions.List_apply, List(wrappedArray))
        }

        def mkMethodList(tpe: TypeTree)(testMethods: List[MethodSymbol]): Tree =
          mkList(testMethods.map(defaultMethodMetadata(tpe)))

        val getTestNGMethodRhs = {
          mkNewInstance(
              TypeTree(testNGClassMetadataType),
              List(
                mkList(liftAnnotations(clSym)),
                gen.mkNil,
                mkMethodList(testNGMethodMetadataTypeTree)(methods),
                modMethods.fold(gen.mkNil)(mkMethodList(testNGMethodMetadataTypeTree))
          ))
        }

        val getTestNGMetadataSym = clSym.newMethod(newTermName("metadata"))
        getTestNGMetadataSym.setInfo(MethodType(Nil, testNGClassMetadataType))

        typer.typedDefDef(newDefDef(getTestNGMetadataSym, getTestNGMethodRhs)())
      }

      private def hasTestNGMethodAnnotation(mtd: MethodSymbol): Boolean =
        annotationWhiteList.exists(hasAnnotation(mtd, _))

      private def hasAnnotation(mtd: MethodSymbol, tpe: TypeSymbol): Boolean =
        mtd.annotations.exists(_.atp.typeSymbol == tpe)

      private def mkNewInstance[T: TypeTag](params: List[Tree]): Apply =
        mkNewInstance(TypeTree(typeOf[T]), params)

      private def mkNewInstance(tpe: TypeTree, params: List[Tree]): Apply =
        Apply(Select(New(tpe), nme.CONSTRUCTOR), params)

      /* Generate a method that creates a new instance of the test class, this
       * method will be located in the bootstrapper class.
       */
      private def genNewInstanceDef(classSym: Symbol, bootSymbol: Symbol): DefDef = {
        val mkNewInstanceDefRhs =
          mkNewInstance(TypeTree(classSym.typeConstructor), Nil)
        val mkNewInstanceDefSym = bootSymbol.newMethodSymbol(newTermName("newInstance"))
        mkNewInstanceDefSym.setInfo(MethodType(Nil, definitions.ObjectTpe))

        typer.typedDefDef(newDefDef(mkNewInstanceDefSym, mkNewInstanceDefRhs)())
      }

      private def mkParamSymbols(method: MethodSymbol,
          params: List[(String, Type)]): List[Symbol] = {
        params.map {
          case (pName, tpe) =>
            val sym = method.newValueParameter(newTermName(pName))
            sym.setInfo(tpe)
            sym
        }
      }

      private def mkMethod(methodSym: MethodSymbol, methodRhs: Tree,
          paramSymbols: List[Symbol]): DefDef = {
        val paramValDefs = List(paramSymbols.map(newValDef(_, EmptyTree)()))
        typer.typedDefDef(newDefDef(methodSym, methodRhs)(vparamss = paramValDefs))
      }

      private def mkMethodResolutionAndCall(methodSym: MethodSymbol,
          methods: List[Symbol], idParamSym: Symbol, genCall: Symbol => Tree): Tree = {
        val tree = methods.foldRight[Tree](mkMethodNotFound(idParamSym)) { (methodSymbol, acc) =>
            val mName = Literal(Constant(methodSymbol.name.toString))
            val paramIdent = gen.mkAttributedIdent(idParamSym)
            val cond = gen.mkMethodCall(paramIdent, definitions.Object_equals, Nil, List(mName))
            val call = genCall(methodSymbol)
            If(cond, call, acc)
        }
        atOwner(methodSym)(typer.typed(tree))
      }

      private def mkMethodNotFound(paramSym: Symbol) = {
        val paramIdent = gen.mkAttributedIdent(paramSym)
        val msg = gen.mkMethodCall(paramIdent, definitions.String_+, Nil,
          List(Literal(Constant(" not found"))))
        val exception = mkNewInstance[NoSuchMethodException](List(msg))
        Throw(exception)
      }
    }
  }
}
