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

package org.scalajs.linker.backend.emitter

import scala.language.implicitConversions

import scala.collection.mutable

import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.Position
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp}
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.{CheckedBehavior, ModuleKind}
import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl
import org.scalajs.linker.backend.javascript.Trees._

import EmitterNames._

private[emitter] object CoreJSLib {

  def build(sjsGen: SJSGen, moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[Lib] = {
    new CoreJSLibBuilder(sjsGen)(moduleContext, globalKnowledge).build()
  }

  /** A fully built CoreJSLib
   *
   *  @param preObjectDefinitions The bulk of the CoreJSLib.
   *      Definitions that do not depend on any other Scala.js emitted code
   *      (notably Object and RuntimeLong). These must be available to all
   *      Scala.js emitted code.
   *
   *  @param postObjectDefinitions Definitions coming after `j.l.Object`.
   *      Definitions that need the `$c_O` class to be defined, but nothing
   *      else. This notably includes the Array classes and everything that
   *      depends on them, such as the `$TypeData` class.
   *
   *  @param initialization Things that depend on Scala.js generated classes.
   *      These must have class definitions (but not static fields) available.
   */
  final class Lib private[CoreJSLib] (
      val preObjectDefinitions: Tree,
      val postObjectDefinitions: Tree,
      val initialization: Tree)

  private class CoreJSLibBuilder(sjsGen: SJSGen)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge) {

    import sjsGen._
    import jsGen._
    import config._
    import nameGen._
    import varGen._
    import esFeatures._
    import semantics._
    import TreeDSL._

    implicit private val noPosition: Position = Position.NoPosition

    private var trackedGlobalRefs = Set.empty[String]

    private def globalRef(name: String): VarRef = {
      trackGlobalRef(name)
      varRef(name)
    }

    private def trackGlobalRef(name: String): Unit = {
      // We never access dangerous global refs from the core JS lib
      assert(!GlobalRefUtils.isDangerousGlobalRef(name))
      if (trackAllGlobalRefs)
        trackedGlobalRefs += name
    }

    private def extractWithGlobals[A](withGlobals: WithGlobals[A]): A = {
      withGlobals.globalVarNames.foreach(trackGlobalRef(_))
      withGlobals.value
    }

    // Unconditional global references
    private val ObjectRef = globalRef("Object")
    private val ArrayRef = globalRef("Array")
    private val StringRef = globalRef("String")
    private val MathRef = globalRef("Math")
    private val TypeErrorRef = globalRef("TypeError")
    private val SymbolRef = globalRef("Symbol")

    // Conditional global references that we often use
    private def BigIntRef = globalRef("BigInt")

    private val classData = Ident("$classData")

    private val orderedPrimRefsWithoutVoid = {
      List(BooleanRef, CharRef, ByteRef, ShortRef, IntRef, LongRef,
          FloatRef, DoubleRef)
    }

    private val orderedPrimRefs = VoidRef :: orderedPrimRefsWithoutVoid

    private val specializedArrayTypeRefs: List[NonArrayTypeRef] =
      ClassRef(ObjectClass) :: orderedPrimRefsWithoutVoid

    def build(): WithGlobals[Lib] = {
      val lib = new Lib(buildPreObjectDefinitions(),
          buildPostObjectDefinitions(), buildInitializations())
      WithGlobals(lib, trackedGlobalRefs)
    }

    private def buildPreObjectDefinitions(): Tree = Block(
      defineLinkingInfo(),
      defineJSBuiltinsSnapshotsAndPolyfills(),
      declareCachedL0(),
      definePropertyName(),
      defineCharClass(),
      defineRuntimeFunctions(),
      defineObjectGetClassFunctions(),
      defineDispatchFunctions(),
      defineArithmeticOps(),
      defineES2015LikeHelpers(),
      defineModuleHelpers(),
      defineIntrinsics(),
      defineIsPrimitiveFunctions(),
      defineBoxFunctions()
    )

    private def buildPostObjectDefinitions(): Tree = Block(
      defineSpecializedArrayClasses(),
      defineTypeDataClass(),
      defineSpecializedIsArrayOfFunctions(),
      defineSpecializedAsArrayOfFunctions(),
      defineSpecializedTypeDatas()
    )

    private def buildInitializations(): Tree = Block(
      assignCachedL0()
    )

    private def defineLinkingInfo(): Tree = {
      // must be in sync with scala.scalajs.runtime.LinkingInfo

      def objectFreeze(tree: Tree): Tree =
        Apply(genIdentBracketSelect(ObjectRef, "freeze"), tree :: Nil)

      val linkingInfo = objectFreeze(ObjectConstr(List(
          str("assumingES6") -> bool(useECMAScript2015),
          str("productionMode") -> bool(productionMode),
          str("linkerVersion") -> str(ScalaJSVersions.current),
          str("fileLevelThis") -> This()
      )))

      extractWithGlobals(globalVarDef("linkingInfo", CoreVar, linkingInfo))
    }

    private def defineJSBuiltinsSnapshotsAndPolyfills(): Tree = {
      def genPolyfillFor(builtinName: String): Tree = builtinName match {
        case "is" =>
          val x = varRef("x")
          val y = varRef("y")
          genArrowFunction(paramList(x, y), Return {
            If(x === y, {
              // +0.0 must be different from -0.0
              (x !== 0) || ((int(1) / x) === (int(1) / y))
            }, {
              // NaN must be equal to NaN
              (x !== x) && (y !== y)
            })
          })

        case "imul" =>
          val a = varRef("a")
          val b = varRef("b")
          val ah = varRef("ah")
          val al = varRef("al")
          val bh = varRef("bh")
          val bl = varRef("bl")
          genArrowFunction(paramList(a, b), Block(
              const(ah, a >>> 16),
              const(al, a & 0xffff),
              const(bh, b >>> 16),
              const(bl, b & 0xffff),
              Return((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0)
          ))

        case "fround" =>
          val v = varRef("v")
          if (!strictFloats) {
            genArrowFunction(paramList(v), Return(+v))
          } else {
            val Float32ArrayRef = globalRef("Float32Array")

            val array = varRef("array")
            val typedArrayPolyfill = genArrowFunction(paramList(v), {
              Block(
                  const(array, New(Float32ArrayRef, 1 :: Nil)),
                  BracketSelect(array, 0) := v,
                  Return(BracketSelect(array, 0))
              )
            })

            // scalastyle:off line.size.limit
            /* Originally inspired by the Typed Array polyfills written by
             * Joshua Bell:
             * https://github.com/inexorabletash/polyfill/blob/a682f42c1092280bb01907c245979fb07219513d/typedarray.js#L150-L255
             * Then simplified quite a lot because
             * 1) we do not need to produce the actual bit string that serves
             *    as storage of the floats, and
             * 2) we are only interested in the float32 case.
             */
            // scalastyle:on line.size.limit
            val isNegative = varRef("isNegative")
            val av = varRef("av")
            val absResult = varRef("absResult")
            val e0 = varRef("e0")
            val twoPowE0 = varRef("twoPowE0")
            val n1 = varRef("n1")
            val w1 = varRef("w1")
            val d1 = varRef("d1")
            val f0 = varRef("f0")
            val n2 = varRef("n2")
            val w2 = varRef("w2")
            val d2 = varRef("d2")

            def callMathFun(fun: String, args: Tree*): Tree =
              Apply(genIdentBracketSelect(MathRef, fun), args.toList)

            def roundAndThen(n: VarRef, w: VarRef, d: VarRef)(
                rest: Tree => Tree): Tree = {
              Block(
                  const(w, callMathFun("floor", n)),
                  const(d, n - w),
                  rest {
                    If(d < double(0.5), w,
                        If(d > double(0.5), w + 1,
                            If((w % 2) !== 0, w + 1, w)))
                  }
              )
            }

            val Inf = double(Double.PositiveInfinity)
            val NegInf = double(Double.NegativeInfinity)
            val subnormalThreshold = double(1.1754943508222875e-38)
            val ln2 = double(0.6931471805599453)
            val twoPowMantissaBits = int(8388608)
            val FloatMinPosValue = double(Float.MinPositiveValue)

            val noTypedArrayPolyfill = genArrowFunction(paramList(v), {
              Block(
                  If((v !== v) || (v === 0) || (v === Inf) || (v === NegInf), {
                    Return(v)
                  }, Skip()),
                  const(isNegative, v < 0),
                  const(av, If(isNegative, -v, v)),
                  genEmptyMutableLet(absResult.ident),
                  If(av >= subnormalThreshold, {
                    Block(
                        const(e0, callMathFun("floor", callMathFun("log", av) / ln2)),
                        If(e0 > 127, { // bias
                          absResult := Inf
                        }, {
                          Block(
                              const(twoPowE0, callMathFun("pow", 2, e0)),
                              const(n1, twoPowMantissaBits * (av / twoPowE0)),
                              roundAndThen(n1, w1, d1) { rounded =>
                                const(f0, rounded)
                              },
                              If((f0 / twoPowMantissaBits) < 2, {
                                absResult := twoPowE0 * (int(1) + ((f0-twoPowMantissaBits) / twoPowMantissaBits))
                              }, {
                                If(e0 > 126, {
                                  absResult := Inf
                                }, {
                                  // 1.1920928955078125e-7 is (1 + ((1-twoPowMantissaBits) / twoPowMantissaBits))
                                  absResult := (int(2) * twoPowE0) * double(1.1920928955078125e-7)
                                })
                              })
                          )
                        })
                    )
                  }, {
                    Block(
                        const(n2, av / FloatMinPosValue),
                        roundAndThen(n2, w2, d2) { rounded =>
                          absResult := FloatMinPosValue * rounded
                        }
                    )
                  }),
                  Return(If(isNegative, -absResult, absResult))
              )
            })

            If(typeof(Float32ArrayRef) !== str("undefined"),
                typedArrayPolyfill, noTypedArrayPolyfill)
          }

        case "clz32" =>
          val i = varRef("i")
          val r = varRef("r")
          genArrowFunction(paramList(i), Block(
              // See Hacker's Delight, Section 5-3
              If(i === 0, Return(32), Skip()),
              let(r, 1),
              If((i & 0xffff0000) === 0, Block(i := i << 16, r := r + 16), Skip()),
              If((i & 0xff000000) === 0, Block(i := i << 8, r := r + 8), Skip()),
              If((i & 0xf0000000) === 0, Block(i := i << 4, r := r + 4), Skip()),
              If((i & 0xc0000000) === 0, Block(i := i << 2, r := r + 2), Skip()),
              Return(r + (i >> 31))
          ))

        case "privateJSFieldSymbol" =>
          /* function privateJSFieldSymbol(description) {
           *   function rand32() {
           *     const s = ((Math.random() * 4294967296.0) | 0).toString(16);
           *     return "00000000".substring(s.length) + s;
           *   }
           *   return description + rand32() + rand32() + rand32() + rand32();
           * }
           *
           * In production mode, we remove the `description` parameter.
           */
          val description = varRef("description")
          val rand32 = varRef("rand32")
          val s = varRef("s")

          val theParamList =
            if (semantics.productionMode) Nil
            else paramList(description)

          genArrowFunction(theParamList, Block(
              FunctionDef(rand32.ident, Nil, Block(
                  genLet(s.ident, mutable = false, {
                      val randomDouble =
                        Apply(genIdentBracketSelect(MathRef, "random"), Nil)
                      val randomInt =
                        (randomDouble * double(4294967296.0)) | 0
                      Apply(genIdentBracketSelect(randomInt, "toString"), 16 :: Nil)
                  }),
                  {
                    val padding = Apply(
                        genIdentBracketSelect(str("00000000"), "substring"),
                        genIdentBracketSelect(s, "length") :: Nil)
                    Return(padding + s)
                  }
              )),
              {
                val callRand32 = Apply(rand32, Nil)
                val rand128 = callRand32 + callRand32 + callRand32 + callRand32
                val result =
                  if (semantics.productionMode) rand128
                  else description + rand128
                Return(result)
              }
          ))
      }

      val mathBuiltins = Block(
        List("imul", "fround", "clz32").map { builtinName =>
          val rhs0 = genIdentBracketSelect(MathRef, builtinName)
          val rhs =
            if (useECMAScript2015) rhs0
            else rhs0 || genPolyfillFor(builtinName)
          extractWithGlobals(globalVarDef(builtinName, CoreVar, rhs))
        }
      )

      val es5Compat = condTree(!useECMAScript2015)(Block(
        extractWithGlobals(globalVarDef("is", CoreVar,
            genIdentBracketSelect(ObjectRef, "is") || genPolyfillFor("is"))),
        extractWithGlobals(globalVarDef("privateJSFieldSymbol", CoreVar,
            If(UnaryOp(JSUnaryOp.typeof, SymbolRef) !== str("undefined"),
                SymbolRef, genPolyfillFor("privateJSFieldSymbol"))))
      ))

      Block(mathBuiltins, es5Compat)
    }

    private def declareCachedL0(): Tree = {
      condTree(!allowBigIntsForLongs)(
        extractWithGlobals(globalVarDecl("L0", CoreVar))
      )
    }

    private def assignCachedL0(): Tree = {
      condTree(!allowBigIntsForLongs)(Block(
        globalVar("L0", CoreVar) := genScalaClassNew(
            LongImpl.RuntimeLongClass, LongImpl.initFromParts, 0, 0),
        genClassDataOf(LongRef) DOT "zero" := globalVar("L0", CoreVar)
      ))
    }

    private def definePropertyName(): Tree = {
      /* Encodes a property name for runtime manipulation.
       *
       * Usage:
       *   env.propertyName({someProp:0})
       * Returns:
       *   "someProp"
       * Useful when the property is renamed by a global optimizer (like
       * Closure) but we must still get hold of a string of that name for
       * runtime reflection.
       */
      defineFunction1("propertyName") { obj =>
        val prop = varRef("prop")
        ForIn(genEmptyImmutableLet(prop.ident), obj, Return(prop))
      }
    }

    private def defineCharClass(): Tree = {
      val ctor = {
        val c = varRef("c")
        MethodDef(static = false, Ident("constructor"), paramList(c), {
          This() DOT "c" := c
        })
      }

      val toStr = {
        MethodDef(static = false, Ident("toString"), Nil, {
          Return(Apply(genIdentBracketSelect(StringRef, "fromCharCode"),
              (This() DOT "c") :: Nil))
        })
      }

      if (useClassesForRegularClasses) {
        extractWithGlobals(globalClassDef("Char", CoreVar, None, ctor :: toStr :: Nil))
      } else {
        Block(
          defineFunction("Char", ctor.args, ctor.body),
          assignES5ClassMembers(globalVar("Char", CoreVar), List(toStr))
        )
      }
    }

    private def defineRuntimeFunctions(): Tree = Block(
      condTree(asInstanceOfs != CheckedBehavior.Unchecked)(Block(
        defineFunction2("throwClassCastException") { (instance, classFullName) =>
          Throw(maybeWrapInUBE(asInstanceOfs, {
            genScalaClassNew(ClassCastExceptionClass, StringArgConstructorName,
                instance + str(" is not an instance of ") + classFullName)
          }))
        },

        defineFunction3("throwArrayCastException") { (instance, classArrayEncodedName, depth) =>
          Block(
              While(depth.prefix_--, {
                classArrayEncodedName := (str("[") + classArrayEncodedName)
              }),
              genCallHelper("throwClassCastException", instance, classArrayEncodedName)
          )
        }
      )),

      condTree(arrayIndexOutOfBounds != CheckedBehavior.Unchecked)(
        defineFunction1("throwArrayIndexOutOfBoundsException") { i =>
          Throw(maybeWrapInUBE(arrayIndexOutOfBounds, {
            genScalaClassNew(ArrayIndexOutOfBoundsExceptionClass,
                StringArgConstructorName,
                If(i === Null(), Null(), str("") + i))
          }))
        }
      ),

      condTree(moduleInit == CheckedBehavior.Fatal)(
        defineFunction1("throwModuleInitError") { name =>
          Throw(genScalaClassNew(UndefinedBehaviorErrorClass,
              StringArgConstructorName, str("Initializer of ") + name +
              str(" called before completion of its super constructor")))
        }
      ),

      defineFunction1("noIsInstance") { instance =>
        Throw(New(TypeErrorRef,
            str("Cannot call isInstance() on a Class representing a JS trait/object") :: Nil))
      },

      defineFunction2("newArrayObject") { (arrayClassData, lengths) =>
        Return(genCallHelper("newArrayObjectInternal", arrayClassData, lengths, int(0)))
      },

      defineFunction3("newArrayObjectInternal") { (arrayClassData, lengths, lengthIndex) =>
        val result = varRef("result")
        val subArrayClassData = varRef("subArrayClassData")
        val subLengthIndex = varRef("subLengthIndex")
        val underlying = varRef("underlying")
        val i = varRef("i")

        Block(
          const(result, New(arrayClassData DOT "constr",
              BracketSelect(lengths, lengthIndex) :: Nil)),
          If(lengthIndex < (lengths.length - 1), Block(
            const(subArrayClassData, arrayClassData DOT "componentData"),
            const(subLengthIndex, lengthIndex + 1),
            const(underlying, result.u),
            For(let(i, 0), i < underlying.length, i.++, {
              BracketSelect(underlying, i) :=
                genCallHelper("newArrayObjectInternal", subArrayClassData, lengths, subLengthIndex)
            })
          )),
          Return(result)
        )
      }
    )

    private def defineObjectGetClassFunctions(): Tree = {
      // objectGetClass and objectClassName

      def defineObjectGetClassBasedFun(name: String,
          constantClassResult: ClassName => Tree,
          scalaObjectResult: VarRef => Tree, jsObjectResult: Tree): Tree = {
        defineFunction1(name) { instance =>
          Switch(typeof(instance), List(
              str("string") -> {
                Return(constantClassResult(BoxedStringClass))
              },
              str("number") -> {
                Block(
                    If(genCallHelper("isInt", instance), {
                      If((instance << 24 >> 24) === instance, {
                        Return(constantClassResult(BoxedByteClass))
                      }, {
                        If((instance << 16 >> 16) === instance, {
                          Return(constantClassResult(BoxedShortClass))
                        }, {
                          Return(constantClassResult(BoxedIntegerClass))
                        })
                      })
                    }, {
                      if (strictFloats) {
                        If(genCallHelper("isFloat", instance), {
                          Return(constantClassResult(BoxedFloatClass))
                        }, {
                          Return(constantClassResult(BoxedDoubleClass))
                        })
                      } else {
                        Return(constantClassResult(BoxedFloatClass))
                      }
                    })
                )
              },
              str("boolean") -> {
                Return(constantClassResult(BoxedBooleanClass))
              },
              str("undefined") -> {
                Return(constantClassResult(BoxedUnitClass))
              }
          ), {
            If(instance === Null(), {
              Return(Apply(instance DOT genName(getClassMethodName), Nil))
            }, {
              If(genIsInstanceOfHijackedClass(instance, BoxedLongClass), {
                Return(constantClassResult(BoxedLongClass))
              }, {
                If(genIsInstanceOfHijackedClass(instance, BoxedCharacterClass), {
                  Return(constantClassResult(BoxedCharacterClass))
                }, {
                  If(genIsScalaJSObject(instance), {
                    Return(scalaObjectResult(instance))
                  }, {
                    Return(jsObjectResult)
                  })
                })
              })
            })
          })
        }
      }


      Block(
        /* We use isClassClassInstantiated as an over-approximation of whether
         * the program contains any `GetClass` node. If `j.l.Class` is not
         * instantiated, then we know that there is no `GetClass` node, and it is
         * safe to omit the definition of `objectGetClass`. However, it is
         * possible that we generate `objectGetClass` even if it is not
         * necessary, in the case that `j.l.Class` is otherwise instantiated
         * (i.e., through a `ClassOf` node).
         */
        condTree(globalKnowledge.isClassClassInstantiated)(
          defineObjectGetClassBasedFun("objectGetClass",
              className => genClassOf(className),
              instance => Apply(instance DOT classData DOT "getClassOf", Nil),
              Null()
          )
        ),

        defineObjectGetClassBasedFun("objectClassName",
            { className =>
              StringLiteral(RuntimeClassNameMapperImpl.map(
                  semantics.runtimeClassNameMapper, className.nameString))
            },
            instance => genIdentBracketSelect(instance DOT classData, "name"),
            Apply(Null() DOT genName(getNameMethodName), Nil)
        )
      )
    }

    private def defineDispatchFunctions(): Tree = {
      val instance = varRef("instance")

      def defineDispatcher(methodName: MethodName, args: List[VarRef],
          body: Tree): Tree = {
        defineFunction("dp_" + genName(methodName),
            paramList((instance :: args): _*), body)
      }

      /* A standard dispatcher performs a type test on the instance and then
       * calls the relevant implementation which is either of:
       *
       * - A normal method call if the instance is a normal scala class.
       * - A method in the relevant hijacked class.
       * - The implementation in java.lang.Object (if this is a JS object).
       */
      def defineStandardDispatcher(methodName: MethodName,
          targetHijackedClasses: List[ClassName]): Tree = {

        val args =
          methodName.paramTypeRefs.indices.map(i => varRef("x" + i)).toList

        val implementingHijackedClasses = targetHijackedClasses
          .filter(globalKnowledge.representativeClassHasPublicMethod(_, methodName))

        def hijackedClassNameToTypeof(className: ClassName): Option[String] = className match {
          case BoxedStringClass  => Some("string")
          case BoxedDoubleClass  => Some("number")
          case BoxedBooleanClass => Some("boolean")
          case BoxedUnitClass    => Some("undefined")
          case _                 => None
        }

        def genHijackedMethodApply(className: ClassName): Tree =
          Apply(globalVar("f", (className, methodName)), instance :: args)

        def genBodyNoSwitch(hijackedClasses: List[ClassName]): Tree = {
          val normalCall = Return(Apply(instance DOT genName(methodName), args))
          val implementedInObject =
            globalKnowledge.representativeClassHasPublicMethod(ObjectClass, methodName)

          def hijackedDispatch(default: Tree) = {
            hijackedClasses.foldRight(default) { (className, next) =>
              If(genIsInstanceOfHijackedClass(instance, className),
                  Return(genHijackedMethodApply(className)),
                  next)
            }
          }

          if (implementedInObject) {
            val staticObjectCall: Tree = {
              val fun = globalVar("c", ObjectClass).prototype DOT genName(methodName)
              Return(Apply(fun DOT "call", instance :: args))
            }

            If(genIsScalaJSObjectOrNull(instance),
                normalCall,
                hijackedDispatch(staticObjectCall))
          } else {
            hijackedDispatch(normalCall)
          }
        }

        defineDispatcher(methodName, args, {
          val maybeWithoutLong =
            if (allowBigIntsForLongs) implementingHijackedClasses
            else implementingHijackedClasses.filter(_ != BoxedLongClass)

          val (classesWithTypeof, otherClasses) =
            maybeWithoutLong.partition(hijackedClassNameToTypeof(_).isDefined)

          if (classesWithTypeof.lengthCompare(1) > 0) {
            // First switch on the typeof
            Switch(typeof(instance), for (className <- classesWithTypeof) yield {
              str(hijackedClassNameToTypeof(className).get) -> {
                Return(genHijackedMethodApply(className))
              }
            }, {
              genBodyNoSwitch(otherClasses)
            })
          } else {
            genBodyNoSwitch(maybeWithoutLong)
          }
        })
      }

      val dispatchers = (
        for {
          methodName <- List(getClassMethodName, cloneMethodName,
              notifyMethodName, notifyAllMethodName, finalizeMethodName)
        } yield {
          defineStandardDispatcher(methodName, Nil)
        }
      ) ++ List(
        // toString()java.lang.String is special as per IR spec.
        defineDispatcher(toStringMethodName, Nil, {
          Return(If(instance === Undefined(),
              str("undefined"),
              Apply(instance DOT "toString", Nil)))
        }),

        defineStandardDispatcher(equalsMethodName,
            List(BoxedDoubleClass, BoxedLongClass, BoxedCharacterClass)),

        defineStandardDispatcher(hashCodeMethodName,
            List(BoxedStringClass, BoxedDoubleClass, BoxedBooleanClass,
                BoxedUnitClass, BoxedLongClass, BoxedCharacterClass)),

        defineStandardDispatcher(compareToMethodName,
            List(BoxedStringClass, BoxedDoubleClass, BoxedBooleanClass,
                BoxedLongClass, BoxedCharacterClass)),

        defineStandardDispatcher(lengthMethodName,
            List(BoxedStringClass)),

        defineStandardDispatcher(charAtMethodName,
            List(BoxedStringClass)),

        defineStandardDispatcher(subSequenceMethodName,
            List(BoxedStringClass))
      ) ++ (
        for {
          methodName <- List(byteValueMethodName, shortValueMethodName,
              intValueMethodName, longValueMethodName, floatValueMethodName,
              doubleValueMethodName)
        } yield {
          defineStandardDispatcher(methodName,
              List(BoxedDoubleClass, BoxedLongClass))
        }
      )

      Block(dispatchers)
    }

    private def defineArithmeticOps(): Tree = {
      val throwDivByZero = {
        Throw(genScalaClassNew(ArithmeticExceptionClass,
            StringArgConstructorName, str("/ by zero")))
      }

      def wrapBigInt64(tree: Tree): Tree =
        Apply(genIdentBracketSelect(BigIntRef, "asIntN"), 64 :: tree :: Nil)

      Block(
        defineFunction2("intDiv") { (x, y) =>
          If(y === 0, throwDivByZero, {
            Return((x / y) | 0)
          })
        },

        defineFunction2("intMod") { (x, y) =>
          If(y === 0, throwDivByZero, {
            Return((x % y) | 0)
          })
        },

        defineFunction1("doubleToInt") { x =>
          Return(If(x > 2147483647, 2147483647, If(x < -2147483648, -2147483648, x | 0)))
        },

        condTree(allowBigIntsForLongs)(Block(
          defineFunction2("longDiv") { (x, y) =>
            If(y === BigIntLiteral(0), throwDivByZero, {
              Return(wrapBigInt64(x / y))
            })
          },
          defineFunction2("longMod") { (x, y) =>
            If(y === BigIntLiteral(0), throwDivByZero, {
              Return(wrapBigInt64(x % y))
            })
          },

          defineFunction1("doubleToLong")(x => Return {
            If(x < double(-9223372036854775808.0), { // -2^63
              BigIntLiteral(-9223372036854775808L)
            }, {
              If (x >= double(9223372036854775808.0), { // 2^63
                BigIntLiteral(9223372036854775807L)
              }, {
                If (x !== x, { // NaN
                  BigIntLiteral(0L)
                }, {
                  Apply(BigIntRef,
                      Apply(genIdentBracketSelect(MathRef, "trunc"), x :: Nil) :: Nil)
                })
              })
            })
          })
        ))
      )
    }

    private def defineES2015LikeHelpers(): Tree = Block(
      condTree(!useECMAScript2015)(
        defineFunction2("newJSObjectWithVarargs") { (ctor, args) =>
          val instance = varRef("instance")
          val result = varRef("result")

          // This basically emulates the ECMAScript specification for 'new'.
          Block(
            const(instance, Apply(genIdentBracketSelect(ObjectRef, "create"), ctor.prototype :: Nil)),
            const(result, Apply(genIdentBracketSelect(ctor, "apply"), instance :: args :: Nil)),
            Switch(typeof(result),
                List("string", "number", "boolean", "undefined").map(str(_) -> Skip()) :+
                str("symbol") -> Return(instance),
                Return(If(result === Null(), instance, result)))
          )
        }
      ),

      defineFunction2("resolveSuperRef") { (superClass, propName) =>
        val getPrototypeOf = varRef("getPrototypeOf")
        val getOwnPropertyDescriptor = varRef("getOwnPropertyDescriptor")
        val superProto = varRef("superProto")
        val desc = varRef("desc")

        Block(
          const(getPrototypeOf, genIdentBracketSelect(ObjectRef, "getPrototyeOf")),
          const(getOwnPropertyDescriptor, genIdentBracketSelect(ObjectRef, "getOwnPropertyDescriptor")),
          let(superProto, superClass.prototype),
          While(superProto !== Null(), Block(
            const(desc, Apply(getOwnPropertyDescriptor, superProto :: propName :: Nil)),
            If(desc !== Undefined(), Return(desc)),
            superProto := Apply(getPrototypeOf, superProto :: Nil)
          ))
        )
      },

      defineFunction3("superGet") { (superClass, self, propName) =>
        val desc = varRef("desc")
        val getter = varRef("getter")

        Block(
          const(desc, genCallHelper("resolveSuperRef", superClass, propName)),
          If(desc !== Undefined(), Block(
            const(getter, genIdentBracketSelect(desc, "get")),
            Return(If(getter !== Undefined(),
                Apply(genIdentBracketSelect(getter, "call"), self :: Nil),
                genIdentBracketSelect(getter, "value")))
          ))
        )
      },

      defineFunction4("superSet") { (superClass, self, propName, value) =>
        val desc = varRef("desc")
        val setter = varRef("setter")

        Block(
          const(desc, genCallHelper("resolveSuperRef", superClass, propName)),
          If(desc !== Undefined(), Block(
            const(setter, genIdentBracketSelect(desc, "set")),
            If(setter !== Undefined(), Block(
              Apply(genIdentBracketSelect(setter, "call"), self :: value :: Nil),
              Return(Undefined())
            ))
          )),
          Throw(New(TypeErrorRef,
              List(str("super has no setter '") + propName + str("'."))))
        )
      }
    )

    private def defineModuleHelpers(): Tree = {
      condTree(moduleKind == ModuleKind.CommonJSModule)(
        defineFunction1("moduleDefault") { m =>
          Return(If(
              m && (typeof(m) === str("object")) && (str("default") in m),
              BracketSelect(m, str("default")),
              m))
        }
      )
    }

    private def defineIntrinsics(): Tree = Block(
      condTree(arrayIndexOutOfBounds != CheckedBehavior.Unchecked)(
        defineFunction5("arraycopyCheckBounds") { (srcLen, srcPos, destLen, destPos, length) =>
          If((srcPos < 0) || (destPos < 0) || (length < 0) ||
              (srcPos > ((srcLen - length) | 0)) ||
              (destPos > ((destLen - length) | 0)), {
            genCallHelper("throwArrayIndexOutOfBoundsException", Null())
          })
        }
      ),

      defineFunction5("arraycopyGeneric") { (srcArray, srcPos, destArray, destPos, length) =>
        val i = varRef("i")
        Block(
          if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
            genCallHelper("arraycopyCheckBounds", srcArray.length,
                srcPos, destArray.length, destPos, length)
          } else {
            Skip()
          },
          If((srcArray !== destArray) || (destPos < srcPos) || (((srcPos + length) | 0) < destPos), {
            For(let(i, 0), i < length, i := ((i + 1) | 0), {
              BracketSelect(destArray, (destPos + i) | 0) := BracketSelect(srcArray, (srcPos + i) | 0)
            })
          }, {
            For(let(i, (length - 1) | 0), i >= 0, i := ((i - 1) | 0), {
              BracketSelect(destArray, (destPos + i) | 0) := BracketSelect(srcArray, (srcPos + i) | 0)
            })
          })
        )
      },

      condTree(!useECMAScript2015)(
        defineFunction5("systemArraycopy") { (src, srcPos, dest, destPos, length) =>
          genCallHelper("arraycopyGeneric", src.u, srcPos, dest.u, destPos, length)
        }
      ),

      // systemIdentityHashCode
      locally {
        val WeakMapRef = globalRef("WeakMap")

        val lastIDHash = fileLevelVar("lastIDHash")
        val idHashCodeMap = fileLevelVar("idHashCodeMap")

        val obj = varRef("obj")
        val hash = varRef("hash")

        def functionSkeleton(defaultImpl: Tree): Function = {
          genArrowFunction(paramList(obj), {
            Switch(typeof(obj),
                List("string", "number", "bigint", "boolean").map(str(_) -> Skip()) :+
                str("undefined") -> Return(genCallHelper("dp_" + genName(hashCodeMethodName), obj)),
                defaultImpl)
          })
        }

        def weakMapBasedFunction: Function = {
          functionSkeleton {
            If(obj === Null(), {
              Return(0)
            }, {
              Block(
                  let(hash, Apply(genIdentBracketSelect(idHashCodeMap, "get"), obj :: Nil)),
                  If(hash === Undefined(), {
                    Block(
                        hash := ((lastIDHash + 1) | 0),
                        lastIDHash := hash,
                        Apply(genIdentBracketSelect(idHashCodeMap, "set"), obj :: hash :: Nil)
                    )
                  }, {
                    Skip()
                  }),
                  Return(hash)
              )
            })
          }
        }

        def fieldBasedFunction: Function = {
          functionSkeleton {
            If(genIsScalaJSObject(obj), {
              Block(
                  let(hash, genIdentBracketSelect(obj, "$idHashCode$0")),
                  If(hash !== Undefined(), {
                    Return(hash)
                  }, {
                    If(!Apply(genIdentBracketSelect(ObjectRef, "isSealed"), obj :: Nil), {
                      Block(
                          hash := ((lastIDHash + 1) | 0),
                          lastIDHash := hash,
                          genIdentBracketSelect(obj, "$idHashCode$0") := hash,
                          Return(hash)
                      )
                    }, {
                      Return(42)
                    })
                  })
              )
            }, {
              If(obj === Null(), 0, 42)
            })
          }
        }

        Block(
          let(lastIDHash, 0),
          const(idHashCodeMap,
              if (useECMAScript2015) New(WeakMapRef, Nil)
              else If(typeof(WeakMapRef) !== str("undefined"), New(WeakMapRef, Nil), Null())),
          if (useECMAScript2015) {
            val f = weakMapBasedFunction
            defineFunction("systemIdentityHashCode", f.args, f.body)
          } else {
            extractWithGlobals(globalVarDef("systemIdentityHashCode", CoreVar,
                If(idHashCodeMap !== Null(), weakMapBasedFunction, fieldBasedFunction)))
          }
        )
      }
    )

    private def defineIsPrimitiveFunctions(): Tree = {
      def defineIsIntLike(name: String, specificTest: VarRef => Tree): Tree = {
        defineFunction1(name) { v =>
          Return((typeof(v) === str("number")) && specificTest(v) &&
              ((int(1) / v) !== (int(1) / double(-0.0))))
        }
      }

      Block(
        defineIsIntLike("isByte", v => (v << 24 >> 24) === v),
        defineIsIntLike("isShort", v => (v << 16 >> 16) === v),
        defineIsIntLike("isInt", v => (v | 0) === v),
        condTree(allowBigIntsForLongs)(
          defineFunction1("isLong") { v =>
            Return((typeof(v) === str("bigint")) &&
                (Apply(genIdentBracketSelect(BigIntRef, "asIntN"), int(64) :: v :: Nil) === v))
          }
        ),
        condTree(strictFloats)(
          defineFunction1("isFloat") { v =>
            Return((typeof(v) === str("number")) &&
                ((v !== v) || (genCallHelper("fround", v) === v)))
          }
        )
      )
    }

    private def defineBoxFunctions(): Tree = Block(
      // Boxes for Chars
      defineFunction1("bC") { c =>
        Return(New(globalVar("Char", CoreVar), c :: Nil))
      },
      extractWithGlobals(globalVarDef("bC0", CoreVar, genCallHelper("bC", 0))),

      if (asInstanceOfs != CheckedBehavior.Unchecked) {
        // Unboxes for everything
        def defineUnbox(name: String, boxedClassName: ClassName, resultExpr: VarRef => Tree): Tree = {
          val fullName = boxedClassName.nameString
          defineFunction1(name)(v => Return {
            If(genIsInstanceOfHijackedClass(v, boxedClassName) || (v === Null()),
                resultExpr(v),
                genCallHelper("throwClassCastException", v, str(fullName)))
          })
        }

        Block(
          defineUnbox("uV", BoxedUnitClass, _ => Undefined()),
          defineUnbox("uZ", BoxedBooleanClass, v => !(!v)),
          defineUnbox("uC", BoxedCharacterClass, v => If(v === Null(), 0, v DOT "c")),
          defineUnbox("uB", BoxedByteClass, _ | 0),
          defineUnbox("uS", BoxedShortClass, _ | 0),
          defineUnbox("uI", BoxedIntegerClass, _ | 0),
          defineUnbox("uJ", BoxedLongClass, v => If(v === Null(), genLongZero(), v)),

          /* Since the type test ensures that v is either null or a float, we can
           * use + instead of fround.
           */
          defineUnbox("uF", BoxedFloatClass, v => +v),

          defineUnbox("uD", BoxedDoubleClass, v => +v),
          defineUnbox("uT", BoxedStringClass, v => If(v === Null(), StringLiteral(""), v))
        )
      } else {
        // Unboxes for Chars and Longs
        Block(
          defineFunction1("uC") { v =>
            Return(If(v === Null(), 0, v DOT "c"))
          },
          defineFunction1("uJ") { v =>
            Return(If(v === Null(), genLongZero(), v))
          }
        )
      }
    )

    /** Define the array classes for primitive types and for `Object`.
     *
     *  Other array classes are created dynamically from their TypeData's
     *  `initArray` initializer, and extend the array class for `Object`.
     */
    private def defineSpecializedArrayClasses(): Tree = Block(
      for (componentTypeRef <- specializedArrayTypeRefs) yield {
        val ArrayClass = globalVar("ac", componentTypeRef)

        val isTypedArray = usesUnderlyingTypedArray(componentTypeRef)

        val ctor = {
          val arg = varRef("arg")
          MethodDef(static = false, Ident("constructor"), paramList(arg), {
            Block(
                if (useClassesForRegularClasses) Apply(Super(), Nil) else Skip(),
                genArrayClassConstructorBody(arg, componentTypeRef)
            )
          })
        }

        val getAndSet = if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
          val i = varRef("i")
          val v = varRef("v")

          val boundsCheck = {
            If((i < 0) || (i >= This().u.length),
                genCallHelper("throwArrayIndexOutOfBoundsException", i))
          }

          List(
              MethodDef(static = false, Ident("get"), paramList(i), {
                Block(
                    boundsCheck,
                    Return(BracketSelect(This().u, i))
                )
              }),
              MethodDef(static = false, Ident("set"), paramList(i, v), {
                Block(
                    boundsCheck,
                    BracketSelect(This().u, i) := v
                )
              })
          )
        } else {
          Nil
        }

        val copyTo = if (useECMAScript2015) {
          val srcPos = varRef("srcPos")
          val dest = varRef("dest")
          val destPos = varRef("destPos")
          val length = varRef("length")
          val methodDef = MethodDef(static = false, Ident("copyTo"),
              paramList(srcPos, dest, destPos, length), {
            if (isTypedArray) {
              Block(
                  if (semantics.arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
                    genCallHelper("arraycopyCheckBounds", This().u.length,
                        srcPos, dest.u.length, destPos, length)
                  } else {
                    Skip()
                  },
                  Apply(genIdentBracketSelect(dest.u, "set"),
                      Apply(genIdentBracketSelect(This().u, "subarray"), srcPos :: ((srcPos + length) | 0) :: Nil) ::
                      destPos ::
                      Nil)
              )
            } else {
              genCallHelper("arraycopyGeneric", This().u, srcPos,
                  dest.u, destPos, length)
            }
          })
          methodDef :: Nil
        } else {
          Nil
        }

        val clone = MethodDef(static = false, Ident(genName(cloneMethodName)), Nil, {
          Return(New(ArrayClass,
              Apply(genIdentBracketSelect(This().u, "slice"), Nil) :: Nil))
        })

        val members = getAndSet ::: copyTo ::: clone :: Nil

        if (useClassesForRegularClasses) {
          extractWithGlobals(globalClassDef("ac", componentTypeRef,
              Some(globalVar("c", ObjectClass)), ctor :: members))
        } else {
          val clsDef = Block(
            extractWithGlobals(globalFunctionDef("ac", componentTypeRef,
                ctor.args, ctor.body)),
            (ArrayClass.prototype := New(globalVar("h", ObjectClass), Nil)),
            (ArrayClass.prototype DOT "constructor" := ArrayClass),
            assignES5ClassMembers(ArrayClass, members)
          )

          componentTypeRef match {
            case _: ClassRef =>
              Block(
                clsDef,
                extractWithGlobals(globalFunctionDef("ah", ObjectClass, Nil, Skip())),
                (globalVar("ah", ObjectClass).prototype := ArrayClass.prototype)
              )
            case _: PrimRef =>
              clsDef
          }
        }
      }
    )

    private def genArrayClassConstructorBody(arg: VarRef,
        componentTypeRef: NonArrayTypeRef): Tree = {
      val i = varRef("i")

      If(typeof(arg) === str("number"), {
        getArrayUnderlyingTypedArrayClassRef(componentTypeRef) match {
          case Some(typeArrayClassWithGlobalRefs) =>
            This().u := New(extractWithGlobals(typeArrayClassWithGlobalRefs), arg :: Nil)
          case None =>
            Block(
                This().u := New(ArrayRef, arg :: Nil),
                For(let(i, 0), i < arg, i.++, {
                  BracketSelect(This().u, i) := genZeroOf(componentTypeRef)
                })
            )
        }
      }, {
        // arg is a native array that we wrap
        This().u := arg
      })
    }

    private def defineTypeDataClass(): Tree = {
      def privateFieldSet(fieldName: String, value: Tree): Tree =
        This() DOT fieldName := value

      def publicFieldSet(fieldName: String, value: Tree): Tree =
        genIdentBracketSelect(This(), fieldName) := value

      val ctor = {
        MethodDef(static = false, Ident("constructor"), Nil, {
          Block(
              privateFieldSet("constr", Undefined()),
              if (globalKnowledge.isParentDataAccessed)
                privateFieldSet("parentData", Undefined())
              else
                Skip(),
              privateFieldSet("ancestors", Null()),
              privateFieldSet("componentData", Null()),
              privateFieldSet("arrayBase", Null()),
              privateFieldSet("arrayDepth", int(0)),
              privateFieldSet("zero", Null()),
              privateFieldSet("arrayEncodedName", str("")),
              privateFieldSet("_classOf", Undefined()),
              privateFieldSet("_arrayOf", Undefined()),

              /* A lambda for the logic of the public `isAssignableFrom`,
               * without its fast-path. See the comment on the definition of
               * `isAssignableFrom` for the rationale of this decomposition.
               */
              privateFieldSet("isAssignableFromFun", Undefined()),

              privateFieldSet("wrapArray", Undefined()),

              publicFieldSet("name", str("")),
              publicFieldSet("isPrimitive", bool(false)),
              publicFieldSet("isInterface", bool(false)),
              publicFieldSet("isArrayClass", bool(false)),
              publicFieldSet("isJSClass", bool(false)),
              publicFieldSet("isInstance", Undefined())
          )
        })
      }

      val initPrim = {
        val zero = varRef("zero")
        val arrayEncodedName = varRef("arrayEncodedName")
        val displayName = varRef("displayName")
        val arrayClass = varRef("arrayClass")
        val typedArrayClass = varRef("typedArrayClass")
        val self = varRef("self")
        val that = varRef("that")
        val depth = varRef("depth")
        val obj = varRef("obj")
        MethodDef(static = false, Ident("initPrim"),
            paramList(zero, arrayEncodedName, displayName, arrayClass, typedArrayClass), {
          Block(
              privateFieldSet("ancestors", ObjectConstr(Nil)),
              privateFieldSet("zero", zero),
              privateFieldSet("arrayEncodedName", arrayEncodedName),
              const(self, This()), // capture `this` for use in arrow fun
              privateFieldSet("isAssignableFromFun",
                  genArrowFunction(paramList(that), Return(that === self))),
              publicFieldSet("name", displayName),
              publicFieldSet("isPrimitive", bool(true)),
              publicFieldSet("isInstance",
                  genArrowFunction(paramList(obj), Return(bool(false)))),
              If(arrayClass !== Undefined(), { // it is undefined for void
                privateFieldSet("_arrayOf",
                    Apply(New(globalVar("TypeData", CoreVar), Nil) DOT "initSpecializedArray",
                        List(This(), arrayClass, typedArrayClass)))
              }),
              Return(This())
          )
        })
      }

      val initClass = {
        val internalNameObj = varRef("internalNameObj")
        val isInterface = varRef("isInterface")
        val fullName = varRef("fullName")
        val ancestors = varRef("ancestors")
        val isJSType = varRef("isJSType")
        val parentData = varRef("parentData")
        val isInstance = varRef("isInstance")
        val internalName = varRef("internalName")
        val that = varRef("that")
        val depth = varRef("depth")
        val obj = varRef("obj")
        MethodDef(static = false, Ident("initClass"),
            paramList(internalNameObj, isInterface, fullName, ancestors,
                isJSType, parentData, isInstance), {
          Block(
              const(internalName, genCallHelper("propertyName", internalNameObj)),
              if (globalKnowledge.isParentDataAccessed)
                privateFieldSet("parentData", parentData)
              else
                Skip(),
              privateFieldSet("ancestors", ancestors),
              privateFieldSet("arrayEncodedName", str("L") + fullName + str(";")),
              privateFieldSet("isAssignableFromFun", {
                genArrowFunction(paramList(that), {
                  Return(!(!(BracketSelect(that DOT "ancestors", internalName))))
                })
              }),
              privateFieldSet("isJSType", !(!isJSType)),
              publicFieldSet("name", fullName),
              publicFieldSet("isInterface", isInterface),
              publicFieldSet("isInstance", isInstance || {
                genArrowFunction(paramList(obj), {
                  Return(!(!(obj && (obj DOT classData) &&
                      BracketSelect(obj DOT classData DOT "ancestors", internalName))))
                })
              }),
              Return(This())
          )
        })
      }

      def initArrayCommonBody(arrayClass: VarRef, componentData: VarRef,
          arrayBase: VarRef, arrayDepth: Tree): Tree = {
        val name = varRef("name")

        Block(
            arrayClass.prototype DOT classData := This(),
            const(name, str("[") + (componentData DOT "arrayEncodedName")),
            privateFieldSet("constr", arrayClass),
            if (globalKnowledge.isParentDataAccessed)
              privateFieldSet("parentData", genClassDataOf(ObjectClass))
            else
              Skip(),
            privateFieldSet("ancestors", ObjectConstr(List(
                Ident(genName(ObjectClass)) -> 1,
                Ident(genName(CloneableClass)) -> 1,
                Ident(genName(SerializableClass)) -> 1
            ))),
            privateFieldSet("componentData", componentData),
            privateFieldSet("arrayBase", arrayBase),
            privateFieldSet("arrayDepth", arrayDepth),
            privateFieldSet("arrayEncodedName", name),
            publicFieldSet("name", name),
            publicFieldSet("isArrayClass", bool(true))
        )
      }

      val initSpecializedArray = {
        val componentData = varRef("componentData")
        val arrayClass = varRef("arrayClass")
        val typedArrayClass = varRef("typedArrayClass")
        val isAssignableFromFun = varRef("isAssignableFromFun")
        val self = varRef("self")
        val that = varRef("that")
        val obj = varRef("obj")
        val array = varRef("array")
        MethodDef(static = false, Ident("initSpecializedArray"),
            paramList(componentData, arrayClass, typedArrayClass, isAssignableFromFun), {
          Block(
              initArrayCommonBody(arrayClass, componentData, componentData, 1),
              const(self, This()), // capture `this` for use in arrow fun
              privateFieldSet("isAssignableFromFun", isAssignableFromFun || {
                genArrowFunction(paramList(that), Return(self === that))
              }),
              privateFieldSet("wrapArray", {
                If(typedArrayClass, {
                  genArrowFunction(paramList(array), {
                    Return(New(arrayClass, New(typedArrayClass, array :: Nil) :: Nil))
                  })
                }, {
                  genArrowFunction(paramList(array), {
                    Return(New(arrayClass, array :: Nil))
                  })
                })
              }),
              publicFieldSet("isInstance",
                  genArrowFunction(paramList(obj), Return(obj instanceof arrayClass))),
              Return(This())
          )
        })
      }

      val initArray = {
        val componentData = varRef("componentData")
        val ArrayClass = varRef("ArrayClass")
        val arrayBase = varRef("arrayBase")
        val arrayDepth = varRef("arrayDepth")
        val isAssignableFromFun = varRef("isAssignableFromFun")
        val that = varRef("that")
        val self = varRef("self")
        val obj = varRef("obj")
        val array = varRef("array")
        MethodDef(static = false, Ident("initArray"),
            paramList(componentData), {
          val ArrayClassDef = {
            val ctor = {
              val arg = varRef("arg")
              val i = varRef("i")
              MethodDef(static = false, Ident("constructor"), paramList(arg), {
                if (useClassesForRegularClasses)
                  Apply(Super(), arg :: Nil)
                else
                  genArrayClassConstructorBody(arg, ClassRef(ObjectClass))
              })
            }

            val copyTo = if (useECMAScript2015) {
              val srcPos = varRef("srcPos")
              val dest = varRef("dest")
              val destPos = varRef("destPos")
              val length = varRef("length")
              val methodDef = MethodDef(static = false, Ident("copyTo"),
                  paramList(srcPos, dest, destPos, length), {
                genCallHelper("arraycopyGeneric", This().u, srcPos,
                    dest.u, destPos, length)
              })
              methodDef :: Nil
            } else {
              Nil
            }

            val clone = MethodDef(static = false, Ident(genName(cloneMethodName)), Nil, {
              Return(New(ArrayClass,
                  Apply(genIdentBracketSelect(This().u, "slice"), Nil) :: Nil))
            })

            val members = copyTo ::: clone :: Nil

            if (useClassesForRegularClasses) {
              ClassDef(Some(ArrayClass.ident), Some(globalVar("ac", ObjectClass)),
                  ctor :: members)
            } else {
              Block(
                  FunctionDef(ArrayClass.ident, ctor.args, ctor.body),
                  ArrayClass.prototype := New(globalVar("ah", ObjectClass), Nil),
                  ArrayClass.prototype DOT "constructor" := ArrayClass,
                  assignES5ClassMembers(ArrayClass, members)
              )
            }
          }

          Block(
              ArrayClassDef,
              const(arrayBase, (componentData DOT "arrayBase") || componentData),
              const(arrayDepth, (componentData DOT "arrayDepth") + 1),
              initArrayCommonBody(ArrayClass, componentData, arrayBase, arrayDepth),
              const(isAssignableFromFun, {
                genArrowFunction(paramList(that), {
                  val thatDepth = varRef("thatDepth")
                  Block(
                      const(thatDepth, that DOT "arrayDepth"),
                      Return(If(thatDepth === arrayDepth, {
                        Apply(arrayBase DOT "isAssignableFromFun", (that DOT "arrayBase") :: Nil)
                      }, {
                        (thatDepth > arrayDepth) && (arrayBase === genClassDataOf(ObjectClass))
                      }))
                  )
                })
              }),
              privateFieldSet("isAssignableFromFun", isAssignableFromFun),
              privateFieldSet("wrapArray", genArrowFunction(paramList(array), {
                Return(New(ArrayClass, array :: Nil))
              })),
              const(self, This()), // don't rely on the lambda being called with `this` as receiver
              publicFieldSet("isInstance", genArrowFunction(paramList(obj), {
                val data = varRef("data")
                Block(
                    const(data, obj && (obj DOT classData)),
                    Return(!(!data) && {
                      (data === self) || // fast path
                      Apply(isAssignableFromFun, data :: Nil)
                    })
                )
              })),
              Return(This())
          )
        })
      }

      val getArrayOf = {
        MethodDef(static = false, Ident("getArrayOf"), Nil, {
          Block(
              If(!(This() DOT "_arrayOf"),
                  This() DOT "_arrayOf" :=
                    Apply(New(globalVar("TypeData", CoreVar), Nil) DOT "initArray", This() :: Nil),
                  Skip()),
              Return(This() DOT "_arrayOf")
          )
        })
      }

      def getClassOf = {
        MethodDef(static = false, Ident("getClassOf"), Nil, {
          Block(
              If(!(This() DOT "_classOf"),
                  This() DOT "_classOf" := genScalaClassNew(ClassClass, ObjectArgConstructorName, This()),
                  Skip()),
              Return(This() DOT "_classOf")
          )
        })
      }

      def isAssignableFrom = {
        /* This is the public method called by j.l.Class.isAssignableFrom. It
         * first performs a fast-path with `this === that`, and otherwise calls
         * the internal `isAssignableFromFun` function.
         * The reason for this decomposition (as opposed to performing the
         * fast-path in each `isAssignableFromFun`) is to keep the fast-path
         * monomorphic: on the happy path, the VM performs a monomorphic
         * dispatch to this method, which performs the fast-path and returns.
         * We only need a polymorphic dispatch in the slow path.
         */
        val that = varRef("that")
        MethodDef(static = false, StringLiteral("isAssignableFrom"),
            paramList(that), {
          Return(
              (This() === that) || // fast path
              Apply(This() DOT "isAssignableFromFun", that :: Nil))
        })
      }

      def checkCast = {
        val obj = varRef("obj")
        MethodDef(static = false, StringLiteral("checkCast"), paramList(obj),
          if (asInstanceOfs != CheckedBehavior.Unchecked) {
            If((obj !== Null()) && !(This() DOT "isJSType") &&
                !Apply(genIdentBracketSelect(This(), "isInstance"), obj :: Nil),
              genCallHelper("throwClassCastException", obj, genIdentBracketSelect(This(), "name")),
              Skip())
          } else {
            Skip()
          }
        )
      }

      def getSuperclass = {
        MethodDef(static = false, StringLiteral("getSuperclass"), Nil, {
          Return(If(This() DOT "parentData",
              Apply(This() DOT "parentData" DOT "getClassOf", Nil),
              Null()))
        })
      }

      def getComponentType = {
        MethodDef(static = false, StringLiteral("getComponentType"), Nil, {
          Return(If(This() DOT "componentData",
              Apply(This() DOT "componentData" DOT "getClassOf", Nil),
              Null()))
        })
      }

      def newArrayOfThisClass = {
        val lengths = varRef("lengths")
        val arrayClassData = varRef("arrayClassData")
        val i = varRef("i")
        MethodDef(static = false, StringLiteral("newArrayOfThisClass"),
            paramList(lengths), {
          Block(
              let(arrayClassData, This()),
              For(let(i, 0), i < lengths.length, i.++, {
                arrayClassData := Apply(arrayClassData DOT "getArrayOf", Nil)
              }),
              Return(genCallHelper("newArrayObject", arrayClassData, lengths))
          )
        })
      }

      val members = List(
          initPrim,
          initClass,
          initSpecializedArray,
          initArray,
          getArrayOf
      ) ::: (
          if (globalKnowledge.isClassClassInstantiated) {
            List(
                getClassOf,
                isAssignableFrom,
                checkCast,
                getSuperclass,
                getComponentType,
                newArrayOfThisClass
            )
          } else {
            Nil
          }
      )

      if (useClassesForRegularClasses) {
        extractWithGlobals(globalClassDef("TypeData", CoreVar, None, ctor :: members))
      } else {
        Block(
          defineFunction("TypeData", ctor.args, ctor.body),
          assignES5ClassMembers(globalVar("TypeData", CoreVar), members)
        )
      }
    }

    private def defineSpecializedIsArrayOfFunctions(): Tree = {
      // isArrayOf_O
      val obj = varRef("obj")
      val depth = varRef("depth")
      val data = varRef("data")
      val arrayDepth = varRef("arrayDepth")

      val forObj = extractWithGlobals(globalFunctionDef("isArrayOf", ObjectClass, paramList(obj, depth), {
        Block(
            const(data, obj && (obj DOT "$classData")),
            If(!data, {
              Return(BooleanLiteral(false))
            }, {
              Block(
                  const(arrayDepth, data DOT "arrayDepth"),
                  Return(If(arrayDepth === depth, {
                    !genIdentBracketSelect(data DOT "arrayBase", "isPrimitive")
                  }, {
                    arrayDepth > depth
                  }))
              )
            })
        )
      }))

      val forPrims = for (primRef <- orderedPrimRefsWithoutVoid) yield {
        val obj = varRef("obj")
        val depth = varRef("depth")
        extractWithGlobals(globalFunctionDef("isArrayOf", primRef, paramList(obj, depth), {
          Return(!(!(obj && (obj DOT classData) &&
              ((obj DOT classData DOT "arrayDepth") === depth) &&
              ((obj DOT classData DOT "arrayBase") === genClassDataOf(primRef)))))
        }))
      }

      Block(forObj :: forPrims)
    }

    private def defineSpecializedAsArrayOfFunctions(): Tree = {
      condTree(asInstanceOfs != CheckedBehavior.Unchecked)(Block(
        for (typeRef <- specializedArrayTypeRefs) yield {
          val encodedName = typeRef match {
            case typeRef: PrimRef => typeRef.charCode.toString()
            case _                => "L" + ObjectClass.nameString + ";"
          }

          val obj = varRef("obj")
          val depth = varRef("depth")
          extractWithGlobals(globalFunctionDef("asArrayOf", typeRef, paramList(obj, depth), {
            If(Apply(globalVar("isArrayOf", typeRef), obj :: depth :: Nil) || (obj === Null()), {
              Return(obj)
            }, {
              genCallHelper("throwArrayCastException", obj, str(encodedName), depth)
            })
          }))
        }
      ))
    }

    private def defineSpecializedTypeDatas(): Tree = {
      /* d_O must be first to correctly populate the parentData of array
       * classes. Unlike all other type datas, we assign the first of d_O
       * directly in the generated code, rather than through an `initXyz`
       * method. That's because its initialization code does not follow the
       * pattern of other type datas, and therefore the appropriate `initXyz`
       * would be called only from here anyway.
       */
      val obj = locally {
        val fullName = RuntimeClassNameMapperImpl.map(
            semantics.runtimeClassNameMapper, ObjectClass.nameString)

        val that = varRef("that")
        val obj = varRef("obj")

        val typeDataVar = globalVar("d", ObjectClass)

        def privateFieldSet(fieldName: String, value: Tree): Tree =
          typeDataVar DOT fieldName := value

        def publicFieldSet(fieldName: String, value: Tree): Tree =
          genIdentBracketSelect(typeDataVar, fieldName) := value

        Block(
          extractWithGlobals(
              globalVarDef("d", ObjectClass, New(globalVar("TypeData", CoreVar), Nil))),
          privateFieldSet("ancestors", ObjectConstr(List((Ident(genName(ObjectClass)) -> 1)))),
          privateFieldSet("arrayEncodedName", str("L" + fullName + ";")),
          privateFieldSet("isAssignableFromFun", {
            genArrowFunction(paramList(that), {
              Return(!genIdentBracketSelect(that, "isPrimitive"))
            })
          }),
          publicFieldSet("name", str(fullName)),
          publicFieldSet("isInstance",
              genArrowFunction(paramList(obj), Return(obj !== Null()))),
          privateFieldSet("_arrayOf", {
            Apply(New(globalVar("TypeData", CoreVar), Nil) DOT "initSpecializedArray", List(
              typeDataVar,
              globalVar("ac", ObjectClass),
              Undefined(), // typedArray
              genArrowFunction(paramList(that), {
                val thatDepth = varRef("thatDepth")
                Block(
                    const(thatDepth, that DOT "arrayDepth"),
                    Return(If(thatDepth === 1, {
                      !genIdentBracketSelect(that DOT "arrayBase", "isPrimitive")
                    }, {
                      (thatDepth > 1)
                    }))
                )
              })
            ))
          }),
          globalVar("c", ObjectClass).prototype DOT "$classData" := typeDataVar
        )
      }

      val prims = for (primRef <- orderedPrimRefs) yield {
        /* Zero value, for use by the intrinsified code of
         * `scala.collection.mutable.ArrayBuilder.genericArrayBuilderResult`.
         * This code is Scala-specific, and "unboxes" `null` as the zero of
         * primitive types. For `void`, it is even more special, as it produces
         * a boxed Unit value, which is `undefined` (although `VoidRef`/`NoType`
         * doesn't have a zero value per se).
         */
        val zero = primRef match {
          case VoidRef                          => Undefined()
          case LongRef if !allowBigIntsForLongs => Null() // set later when $L0 is initialized
          case _                                => genZeroOf(primRef)
        }

        val typedArrayClass = getArrayUnderlyingTypedArrayClassRef(primRef) match {
          case Some(typedArrayClassWithGlobals) =>
            extractWithGlobals(typedArrayClassWithGlobals)
          case None =>
            Undefined()
        }

        extractWithGlobals(globalVarDef("d", primRef, {
          Apply(New(globalVar("TypeData", CoreVar), Nil) DOT "initPrim",
              List(zero, str(primRef.charCode.toString()),
                  str(primRef.displayName),
                  if (primRef == VoidRef) Undefined()
                  else genArrayConstrOf(ArrayTypeRef(primRef, 1)),
                  typedArrayClass))
        }))
      }

      Block(obj :: prims)
    }

    private def defineFunction(name: String, args: List[ParamDef], body: Tree): Tree =
      extractWithGlobals(globalFunctionDef(name, CoreVar, args, body))

    private val argRefs = List.tabulate(5)(i => varRef("arg" + i))

    private def defineFunction1(name: String)(body: VarRef => Tree): Tree = {
      val a :: _ = argRefs
      defineFunction(name, paramList(a), body(a))
    }

    private def defineFunction2(name: String)(body: (VarRef, VarRef) => Tree): Tree = {
      val a :: b :: _ = argRefs
      defineFunction(name, paramList(a, b), body(a, b))
    }

    private def defineFunction3(name: String)(body: (VarRef, VarRef, VarRef) => Tree): Tree = {
      val a :: b :: c :: _ = argRefs
      defineFunction(name, paramList(a, b, c), body(a, b, c))
    }

    private def defineFunction4(name: String)(body: (VarRef, VarRef, VarRef, VarRef) => Tree): Tree = {
      val a :: b :: c :: d :: _ = argRefs
      defineFunction(name, paramList(a, b, c, d), body(a, b, c, d))
    }

    private def defineFunction5(name: String)(body: (VarRef, VarRef, VarRef, VarRef, VarRef) => Tree): Tree = {
      val a :: b :: c :: d :: e :: _ = argRefs
      defineFunction(name, paramList(a, b, c, d, e), body(a, b, c, d, e))
    }

    private def maybeWrapInUBE(behavior: CheckedBehavior, exception: Tree): Tree = {
      if (behavior == CheckedBehavior.Fatal) {
        genScalaClassNew(UndefinedBehaviorErrorClass,
            ThrowableArgConsructorName, exception)
      } else {
        exception
      }
    }

    private def genIsScalaJSObject(obj: VarRef): Tree =
      !(!(obj && (obj DOT classData)))

    private def genIsScalaJSObjectOrNull(obj: VarRef): Tree =
      genIsScalaJSObject(obj) || (obj === Null())

    private def condTree(cond: Boolean)(tree: => Tree): Tree =
      if (cond) tree
      else Skip()

    private def varRef(name: String): VarRef = VarRef(Ident(name))

    private def const(ref: VarRef, rhs: Tree): LocalDef =
      genConst(ref.ident, rhs)

    private def let(ref: VarRef, rhs: Tree): LocalDef =
      genLet(ref.ident, mutable = true, rhs)

    private def paramList(refs: VarRef*): List[ParamDef] =
      refs.toList.map(ref => ParamDef(ref.ident, rest = false))

    private def str(s: String): StringLiteral = StringLiteral(s)

    private def bool(b: Boolean): BooleanLiteral = BooleanLiteral(b)

    /* This one is implicit because there are *many* ints in the trees we
     * created, so this helps readability.
     */
    private implicit def int(i: Int): IntLiteral = IntLiteral(i)

    private def double(d: Double): DoubleLiteral = DoubleLiteral(d)
  }
}
