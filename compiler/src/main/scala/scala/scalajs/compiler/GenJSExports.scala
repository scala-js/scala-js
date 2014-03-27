/* Scala.js compiler
 * Copyright 2013 LAMP/EPFL
 * @author  Sébastien Doeraene
 */

package scala.scalajs.compiler

import scala.collection.mutable

import scala.tools.nsc._
import scala.math.PartialOrdering
import scala.reflect.internal.Flags

/** Generation of exports for JavaScript
 *
 *  @author Sébastien Doeraene
 */
trait GenJSExports extends SubComponent { self: GenJSCode =>
  import global._
  import jsAddons._
  import definitions._
  import jsDefinitions._

  trait JSExportsPhase { this: JSCodePhase =>

    /**
     * Generate exporter methods for a class
     * @param classSym symbol of class we export for
     * @param decldExports symbols exporter methods that have been encountered in
     *   the class' tree. This is not the same as classSym.info.delcs since
     *   inherited concrete methods from traits should be in this param, too
     */
    def genMemberExports(
        classSym: Symbol,
        decldExports: List[Symbol]): List[js.Tree] = {

      val newlyDecldExports = decldExports.filterNot { isOverridingExport _ }
      val newlyDecldExportNames =
        newlyDecldExports.map(_.name.toTermName).toList.distinct

      newlyDecldExportNames map { genMemberExport(classSym, _) }
    }

    def genConstructorExports(classSym: Symbol): List[js.Tree] = {
      val constructors = classSym.tpe.member(nme.CONSTRUCTOR).alternatives

      // Generate exports from constructors and their annotations
      val ctorExports = for {
        ctor          <- constructors
        (jsName, pos) <- jsInterop.exportsOf(ctor)
      } yield (jsName, ctor)

      val exports = for {
        (jsName, specs) <- ctorExports.groupBy(_._1) // group by exported name
      } yield {
        import js.TreeDSL._

        val ctors = specs.map(_._2)
        implicit val pos = ctors.head.pos

        val js.MethodDef(_, args, body) = genExportMethod(ctors, jsName)

        val jsCtor = envField("c") DOT encodeClassFullNameIdent(classSym)
        val (createNamespace, expCtorVar) = genCreateNamespaceInExports(jsName)

        js.Block(
          createNamespace,
          js.DocComment("@constructor"),
          expCtorVar := js.Function(args, js.Block(
            // Call the js constructor while passing the current this
            js.ApplyMethod(jsCtor, js.Ident("call"), List(js.This())),
            body
          )),
          expCtorVar DOT "prototype" := jsCtor DOT "prototype"
        )
      }

      exports.toList
    }

    def genModuleAccessorExports(classSym: Symbol): List[js.Tree] = {
      import js.TreeDSL._

      for {
        (jsName, p) <- jsInterop.exportsOf(classSym)
      } yield {
        implicit val pos = p

        val accessorVar =
          envField("modules") DOT encodeModuleFullNameIdent(classSym)
        val (createNamespace, expAccessorVar) = genCreateNamespaceInExports(jsName)

        js.Block(
          createNamespace,
          expAccessorVar := accessorVar
        )
      }
    }

    private def genMemberExport(classSym: Symbol, name: TermName): js.Tree = {
      val alts = classSym.info.member(name).alternatives

      assert(!alts.isEmpty,
          s"Ended up with no alternatives for ${classSym.fullName}::$name. " +
          s"Original set was ${alts} with types ${alts.map(_.tpe)}")

      val (jsName, isProp) = jsInterop.jsExportInfo(name)

      if (isProp)
        genExportProperty(alts, jsName)
      else
        genExportMethod(alts, jsName)

    }

    private def genExportProperty(alts: List[Symbol], jsName: String) = {
      assert(!alts.isEmpty)
      implicit val pos = alts.head.pos

      // Separate getters and setters. Somehow isJSGetter doesn't work here. Hence
      // we just check the parameter list length.
      val (getter, setters) = alts.partition(_.tpe.params.isEmpty)

      // if we have more than one getter, something went horribly wrong
      assert(getter.size <= 1,
          s"Found more than one getter to export for name ${jsName}.")

      val getTree =
        if (getter.isEmpty) js.EmptyTree
        else genApplyForSym(getter.head)

      val setTree =
        if (setters.isEmpty) js.EmptyTree
        else genExportSameArgc(setters, 0) // we only have 1 argument

      js.PropertyDef(js.StringLiteral(jsName), getTree, genFormalArg(1), setTree)
    }

    /** generates the exporter function (i.e. exporter for non-properties) for
     *  a given name */
    private def genExportMethod(alts: List[Symbol], jsName: String) = {
      implicit val pos = alts.head.pos

      assert(alts.nonEmpty,
          "need at least one alternative to generate exporter method")

      // Factor out methods with variable argument lists. Note that they can
      // only be at the end of the lists as enforced by PrepJSExports
      val (varArgMeths, normalMeths) = alts.partition(hasRepeatedParam _)

      // Highest non-repeated argument count
      val maxArgc = (
          // We have argc - 1, since a repeated parameter list may also be empty
          // (unlike a normal parameter)
          varArgMeths.map(_.tpe.params.size - 1) ++
          normalMeths.map(_.tpe.params.size)
      ).max

      val formalArgs = genFormalArgs(maxArgc)

      // Calculates possible arg counts for normal method
      def argCounts(sym: Symbol) = {
        val params = sym.tpe.params
        // Find default param
        val dParam = params.indexWhere { _.hasFlag(Flags.DEFAULTPARAM) }
        if (dParam == -1) Seq(params.size)
        else dParam to params.size
      }

      // Generate tuples (argc, method)
      val methodArgCounts = {
        // Normal methods
        for {
          method <- normalMeths
          argc   <- argCounts(method)
        } yield (argc, method)
      } ++ {
        // Repeated parameter methods
        for {
          method <- varArgMeths
          argc   <- method.tpe.params.size - 1 to maxArgc
        } yield (argc, method)
      }

      // Create a map: argCount -> methods (methods may appear multiple times)
      val methodByArgCount =
        methodArgCounts.groupBy(_._1).mapValues(_.map(_._2).toSet)

      // Create tuples: (methods, argCounts). This will be the cases we generate
      val caseDefinitions =
        methodByArgCount.groupBy(_._2).mapValues(_.keySet)

      // Verify stuff about caseDefinitions
      assert({
        val argcs = caseDefinitions.values.flatten.toList
        argcs == argcs.distinct
        argcs.forall(_ <= maxArgc)
      }, "every argc should appear only once and be lower than max")

      // Generate a case block for each (methods, argCounts) tuple
      val cases = for {
        (methods, argcs) <- caseDefinitions
        if methods.nonEmpty

        // exclude default case we're generating anyways for varargs
        if methods != varArgMeths.toSet

        // body of case to disambiguates methods with current count
        caseBody =
          genExportSameArgc(methods.toList, 0, Some(argcs.min))

        // argc in reverse order
        argcList = argcs.toList.sortBy(- _)

        // A case statement for each argc. Last one contains body
        caseStmt <- genMultiValCase(argcList, caseBody)
      } yield caseStmt

      val hasVarArg = varArgMeths.nonEmpty

      def defaultCase = {
        if (!hasVarArg)
          genThrowTypeError()
        else
          genExportSameArgc(varArgMeths, 0)
      }

      val body = {
        if (cases.isEmpty)
          defaultCase
        else if (cases.size == 1 && !hasVarArg)
          cases.head._2
        else {
          js.Switch(js.DotSelect(js.Ident("arguments"), js.Ident("length")),
              cases.toList, defaultCase)
        }
      }

      js.MethodDef(js.StringLiteral(jsName), formalArgs, body)
    }

    /**
     * Resolve method calls to [[alts]] while assuming they have the same
     * parameter count.
     * @param alts Alternative methods
     * @param paramIndex Index where to start disambiguation
     * @param maxArgc only use that many arguments
     */
    private def genExportSameArgc(alts: List[Symbol],
        paramIndex: Int, maxArgc: Option[Int] = None): js.Tree = {

      implicit val pos = alts.head.pos

      if (alts.size == 1)
        genApplyForSym(alts.head)
      else if (maxArgc.exists(_ <= paramIndex) ||
        !alts.exists(_.tpe.params.size > paramIndex)) {
        // We reach here in three cases:
        // 1. The parameter list has been exhausted
        // 2. The optional argument count restriction has triggered
        // 3. We only have (more than once) repeated parameters left
        // Therefore, we should fail
        currentUnit.error(pos,
            s"""Cannot disambiguate overloads for exported method ${alts.head.name} with types
               |  ${alts.map(_.tpe).mkString("\n  ")}""".stripMargin)
        js.Return(js.Undefined())
      } else {

        val altsByTypeTest = groupByWithoutHashCode(alts) { alt =>
          // get parameter type while resolving repeated params
          val tpe = enteringPhase(currentRun.uncurryPhase) {
            val ps = alt.paramss.flatten
            if (ps.size <= paramIndex || isRepeated(ps(paramIndex))) {
              assert(isRepeated(ps.last))
              repeatedToSingle(ps.last.tpe)
            } else ps(paramIndex).tpe
          }

          typeTestForTpe(tpe)
        }

        if (altsByTypeTest.size == 1) {
          // Testing this parameter is not doing any us good
          genExportSameArgc(alts, paramIndex+1, maxArgc)
        } else {
          // Sort them so that, e.g., isInstanceOf[String]
          // comes before isInstanceOf[Object]
          val sortedAltsByTypeTest = topoSortDistinctsBy(
              altsByTypeTest)(_._1)(RTTypeTest.Ordering)

          val defaultCase = genThrowTypeError()

          sortedAltsByTypeTest.foldRight[js.Tree](defaultCase) { (elem, elsep) =>
            val (typeTest, subAlts) = elem
            implicit val pos = subAlts.head.pos

            val param = genFormalArg(paramIndex+1)
            val genSubAlts = genExportSameArgc(subAlts, paramIndex+1, maxArgc)

            def hasDefaultParam = subAlts.exists { p =>
              val params = p.tpe.params
              params.size > paramIndex &&
              params(paramIndex).hasFlag(Flags.DEFAULTPARAM)
            }

            def orUndef(cond: js.Tree) = if (!hasDefaultParam) cond else {
              js.BinaryOp("||", cond, js.BinaryOp("===", param, js.Undefined()))
            }

            typeTest match {
              case TypeOfTypeTest(typeString) =>
                js.If(orUndef {
                  js.BinaryOp("===", js.UnaryOp("typeof", param),
                      js.StringLiteral(typeString))
                }, genSubAlts, elsep)

              case InstanceOfTypeTest(tpe) =>
                js.If(orUndef {
                  encodeIsInstanceOf(param, tpe)._1
                }, genSubAlts, elsep)

              case NoTypeTest =>
                genSubAlts // note: elsep is discarded, obviously
            }
          }
        }
      }
    }

    /**
     * Generate a call to the method [[sym]] while using the formalArguments
     * and potentially the argument array. Also inserts default parameters if
     * required.
     */
    private def genApplyForSym(sym: Symbol): js.Tree = {
      implicit val pos = sym.pos

      // the (single) type of the repeated parameter if any
      val repeatedTpe = enteringPhase(currentRun.uncurryPhase) {
        for {
          param <- sym.paramss.flatten.lastOption
          if isRepeated(param)
        } yield repeatedToSingle(param.tpe)
      }

      val normalArgc = sym.tpe.params.size -
        (if (repeatedTpe.isDefined) 1 else 0)

      // optional repeated parameter list
      val jsVarArg = repeatedTpe map { tpe =>
        // Construct a new JSArraySeq with optional boxing
        genNew(JSArraySeqClass, JSArraySeq_ctor,
          List(js.Ident("arguments"), js.IntLiteral(normalArgc),
              genBoxFunction(tpe)))
      }

      // normal arguments
      val jsArgs = genFormalArgs(normalArgc)

      // Generate JS code to arguments using default getters
      val jsDefaultArgPrep = for {
        (jsArg, (param, i)) <- jsArgs zip sym.tpe.params.zipWithIndex
        if param.hasFlag(Flags.DEFAULTPARAM)
      } yield {
        import js.TreeDSL._

        // If argument is undefined, call default getter
        IF (jsArg === js.Undefined()) {
          val defaultGetter = sym.owner.tpe.member(
              nme.defaultGetterName(sym.name, i+1))

          assert(defaultGetter.exists)
          assert(!defaultGetter.isOverloaded)

          // Pass previous arguments to defaultGetter
          jsArg := js.ApplyMethod(js.This(), encodeMethodSym(defaultGetter),
              jsArgs.take(defaultGetter.tpe.params.size))

        } ELSE js.Skip() // inference for withoutElse doesn't work
      }

      val jsReturn = js.Return {
        js.ApplyMethod(js.This(), encodeMethodSym(sym),
          jsArgs ++ jsVarArg)
      }

      js.Block(jsDefaultArgPrep :+ jsReturn)
    }

    private def genBoxFunction(tpe: Type)(implicit pos: Position) = {
      toTypeKind(tpe) match {
        case kind: ValueTypeKind =>
          js.Select(environment, js.Ident("b" + kind.primitiveCharCode))
        case _ =>
          // No boxing. Identity function
          val arg = js.Ident("x", None)
          js.Function(arg :: Nil, js.Return(arg))
      }
    }

  }

  /** Gen JS code for assigning an rhs to a qualified name in the exports scope.
   *  For example, given the qualified name "foo.bar.Something", generates:
   *
   *  ScalaJS.e["foo"] = ScalaJS.e["foo"] || {};
   *  ScalaJS.e["foo"]["bar"] = ScalaJS.e["foo"]["bar"] || {};
   *
   *  Returns (statements, ScalaJS.e["foo"]["bar"]["Something"])
   */
  private def genCreateNamespaceInExports(qualName: String)(
      implicit pos: Position): (js.Tree, js.Tree) = {
    val parts = qualName.split("\\.")
    val statements = mutable.ListBuffer.empty[js.Tree]
    var namespace = envField("e")
    for (i <- 0 until parts.length-1) {
      namespace = js.BracketSelect(namespace, js.StringLiteral(parts(i)))
      statements +=
        js.Assign(namespace, js.BinaryOp("||", namespace, js.ObjectConstr(Nil)))
    }
    val lhs = js.BracketSelect(namespace, js.StringLiteral(parts.last))
    (js.Block(statements.result()), lhs)
  }

  private def isOverridingExport(sym: Symbol): Boolean = {
    lazy val osym = sym.nextOverriddenSymbol
    sym.isOverridingSymbol && !osym.owner.isInterface
  }

  private sealed abstract class RTTypeTest

  private final case class TypeOfTypeTest(typeString: String) extends RTTypeTest

  private final case class InstanceOfTypeTest(tpe: Type) extends RTTypeTest {
    override def equals(that: Any): Boolean = {
      that match {
        case InstanceOfTypeTest(thatTpe) => tpe =:= thatTpe
        case _ => false
      }
    }
  }

  private case object NoTypeTest extends RTTypeTest

  private object RTTypeTest {
    implicit object Ordering extends PartialOrdering[RTTypeTest] {
      override def tryCompare(lhs: RTTypeTest, rhs: RTTypeTest): Option[Int] = {
        if (lteq(lhs, rhs)) if (lteq(rhs, lhs)) Some(0) else Some(-1)
        else                if (lteq(rhs, lhs)) Some(1) else None
      }

      override def lteq(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        (lhs, rhs) match {
          case (_, NoTypeTest) => true
          case (NoTypeTest, _) => false

          case (TypeOfTypeTest(s1), TypeOfTypeTest(s2)) =>
            s1 == "undefined" || (s1 <= s2) && s2 != "undefined"

          case (InstanceOfTypeTest(t1), InstanceOfTypeTest(t2)) =>
            t1 <:< t2

          case (_:TypeOfTypeTest, _:InstanceOfTypeTest) => true
          case (_:InstanceOfTypeTest, _:TypeOfTypeTest) => false
        }
      }

      override def equiv(lhs: RTTypeTest, rhs: RTTypeTest): Boolean = {
        lhs == rhs
      }
    }
  }

  // Very simple O(n²) topological sort for elements assumed to be distinct
  private def topoSortDistinctsBy[A <: AnyRef, B](coll: List[A])(f: A => B)(
      implicit ord: PartialOrdering[B]): List[A] = {

    @scala.annotation.tailrec
    def loop(coll: List[A], acc: List[A]): List[A] = {
      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (lhs, rhs) = coll.span(x => !coll.forall(
            y => (x eq y) || !ord.lteq(f(x), f(y))))
        assert(!rhs.isEmpty, s"cycle while ordering $coll")
        loop(lhs ::: rhs.tail, rhs.head :: acc)
      }
    }

    loop(coll, Nil)
  }

  private def typeTestForTpe(tpe: Type): RTTypeTest = {
    toTypeKind(tpe) match {
      case UNDEFINED => TypeOfTypeTest("undefined")
      case LongKind => InstanceOfTypeTest(RuntimeLongClass.tpe)
      case _:INT | _:FLOAT => TypeOfTypeTest("number")
      case BOOL => TypeOfTypeTest("boolean")

      case REFERENCE(cls) =>
        if (cls == StringClass) TypeOfTypeTest("string")
        else if (isRawJSType(tpe)) {
          cls match {
            case JSNumberClass => TypeOfTypeTest("number")
            case JSBooleanClass => TypeOfTypeTest("boolean")
            case JSStringClass => TypeOfTypeTest("string")
            case JSUndefinedClass => TypeOfTypeTest("undefined")
            case _ => NoTypeTest
          }
        } else InstanceOfTypeTest(tpe)

      case ARRAY(_) => InstanceOfTypeTest(tpe)
    }
  }

  // Group-by that does not rely on hashCode(), only equals() - O(n²)
  private def groupByWithoutHashCode[A, B](
      coll: List[A])(f: A => B): List[(B, List[A])] = {

    import scala.collection.mutable.ArrayBuffer
    val m = new ArrayBuffer[(B, List[A])]
    m.sizeHint(coll.length)

    for (elem <- coll) {
      val key = f(elem)
      val index = m.indexWhere(_._1 == key)
      if (index < 0) m += ((key, List(elem)))
      else m(index) = (key, elem :: m(index)._2)
    }

    m.toList
  }

  private def genThrowTypeError()(
      implicit pos: Position): js.Tree = {
    js.Throw(js.StringLiteral("No matching overload"))
  }

  private def genFormalArgs(count: Int)(implicit pos: Position): List[js.Ident] =
    (1 to count map genFormalArg).toList

  private def genFormalArg(index: Int)(implicit pos: Position): js.Ident =
    js.Ident("arg$" + index)


  /**
   * Generate a JS tree like:
   *
   *    case x1:
   *    case x2:
   *    case x3:
   *      <body>
   *
   * @param ris literals on cases in reverse order
   * @param body the body to put in the last statement
   */
  private def genMultiValCase(ris: Seq[Int], body: => js.Tree)(
      implicit pos: Position): List[(js.Tree,js.Tree)] = {

    if (ris.isEmpty) Nil
    else {
      val bodyCase = (js.IntLiteral(ris.head), body)
      val emptyCases = ris.tail.map(i => (js.IntLiteral(i), js.Skip()))

      (emptyCases.reverse :+ bodyCase).toList
    }
  }

  private def hasRepeatedParam(sym: Symbol) =
    enteringPhase(currentRun.uncurryPhase) {
      sym.paramss.flatten.lastOption.exists(isRepeated _)
  }

}
