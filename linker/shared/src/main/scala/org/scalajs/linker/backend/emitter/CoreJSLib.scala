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

import org.scalajs.linker.interface.{CheckedBehavior, ESVersion, ModuleKind}
import org.scalajs.linker.interface.unstable.RuntimeClassNameMapperImpl
import org.scalajs.linker.backend.javascript.Trees._

import EmitterNames._
import PolyfillableBuiltin._

private[emitter] object CoreJSLib {

  def build[E](sjsGen: SJSGen, postTransform: List[Tree] => E, moduleContext: ModuleContext,
      globalKnowledge: GlobalKnowledge): WithGlobals[Lib[E]] = {
    new CoreJSLibBuilder(sjsGen)(moduleContext, globalKnowledge).build(postTransform)
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
  final class Lib[E] private[CoreJSLib] (
      val preObjectDefinitions: E,
      val postObjectDefinitions: E,
      val initialization: E)

  private class CoreJSLibBuilder(sjsGen: SJSGen)(
      implicit moduleContext: ModuleContext, globalKnowledge: GlobalKnowledge) {

    import sjsGen._
    import jsGen._
    import config._
    import coreSpec._
    import nameGen._
    import varGen._
    import esFeatures._
    import semantics._
    import TreeDSL._

    implicit private val noPosition: Position = Position.NoPosition

    private implicit val globalRefTracking: GlobalRefTracking =
      topLevelGlobalRefTracking

    private var trackedGlobalRefs = Set.empty[String]

    private def globalRef(name: String): VarRef = {
      trackGlobalRef(name)
      varRef(name)
    }

    private def trackGlobalRef(name: String): Unit = {
      // We never access dangerous global refs from the core JS lib
      assert(!GlobalRefUtils.isDangerousGlobalRef(name))
      if (globalRefTracking.shouldTrack(name))
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
    private val NumberRef = globalRef("Number")
    private val TypeErrorRef = globalRef("TypeError")
    private def BigIntRef = globalRef("BigInt")
    private val SymbolRef = globalRef("Symbol")

    // Conditional global references that we often use
    private def ReflectRef = globalRef("Reflect")

    private val classData = Ident(cpn.classData)

    private val orderedPrimRefsWithoutVoid = {
      List(BooleanRef, CharRef, ByteRef, ShortRef, IntRef, LongRef,
          FloatRef, DoubleRef)
    }

    private val orderedPrimRefs = VoidRef :: orderedPrimRefsWithoutVoid

    private val specializedArrayTypeRefs: List[NonArrayTypeRef] =
      ClassRef(ObjectClass) :: orderedPrimRefsWithoutVoid

    def build[E](postTransform: List[Tree] => E): WithGlobals[Lib[E]] = {
      val lib = new Lib(
        postTransform(buildPreObjectDefinitions()),
        postTransform(buildPostObjectDefinitions()),
        postTransform(buildInitializations()))
      WithGlobals(lib, trackedGlobalRefs)
    }

    private def buildPreObjectDefinitions(): List[Tree] = {
      defineLinkingInfo() :::
      defineJSBuiltinsSnapshotsAndPolyfills() :::
      declareCachedL0() :::
      defineCharClass() :::
      defineRuntimeFunctions() :::
      defineObjectGetClassFunctions() :::
      defineDispatchFunctions() :::
      defineArithmeticOps() :::
      defineES2015LikeHelpers() :::
      defineModuleHelpers() :::
      defineIntrinsics() :::
      defineIsPrimitiveFunctions() :::
      defineBoxFunctions()
    }

    private def buildPostObjectDefinitions(): List[Tree] = {
      defineSpecializedArrayClasses() :::
      defineTypeDataClass() :::
      defineSpecializedIsArrayOfFunctions() :::
      defineSpecializedAsArrayOfFunctions() :::
      defineSpecializedTypeDatas()
    }

    private def buildInitializations(): List[Tree] = {
      assignCachedL0()
    }

    private def defineLinkingInfo(): List[Tree] = {
      // must be in sync with scala.scalajs.runtime.LinkingInfo

      def objectFreeze(tree: Tree): Tree =
        Apply(genIdentBracketSelect(ObjectRef, "freeze"), tree :: Nil)

      val linkingInfo = objectFreeze(ObjectConstr(List(
          str("esVersion") -> int(esVersion.edition),
          str("assumingES6") -> bool(useECMAScript2015Semantics), // different name for historical reasons
          str("productionMode") -> bool(productionMode),
          str("linkerVersion") -> str(ScalaJSVersions.current),
          str("fileLevelThis") -> This()
      )))

      extractWithGlobals(globalVarDef(VarField.linkingInfo, CoreVar, linkingInfo))
    }

    private def defineJSBuiltinsSnapshotsAndPolyfills(): List[Tree] = {
      def genPolyfillFor(builtin: PolyfillableBuiltin): Tree = builtin match {
        case ObjectIsBuiltin =>
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

        case ImulBuiltin =>
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

        case FroundBuiltin =>
          val v = varRef("v")
          if (!strictFloats) {
            genArrowFunction(paramList(v), Return(+v))
          } else {
            val Float32ArrayRef = globalRef("Float32Array")

            /* (function(array) {
             *   return function(v) {
             *     array[0] = v;
             *     return array[0];
             *   }
             * })(new Float32Array(1))
             *
             * Allocating the Float32Array once and for all, and capturing it
             * in an IIFE, is *much* faster than recreating it in every call of
             * the polyfill (about an order of magnitude).
             */
            val array = varRef("array")
            val typedArrayPolyfillInner = genArrowFunction(paramList(v), {
              Block(
                  BracketSelect(array, 0) := v,
                  Return(BracketSelect(array, 0))
              )
            })
            val typedArrayPolyfill = Apply(
                genArrowFunction(paramList(array), Return(typedArrayPolyfillInner)),
                New(Float32ArrayRef, 1 :: Nil) :: Nil)

            // scalastyle:off line.size.limit
            /* Originally inspired by the Typed Array polyfills written by
             * Joshua Bell:
             * https://github.com/inexorabletash/polyfill/blob/a682f42c1092280bb01907c245979fb07219513d/typedarray.js#L150-L255
             * Then simplified quite a lot because
             * 1) we do not need to produce the actual bit string that serves
             *    as storage of the floats, and
             * 2) we are only interested in the float32 case.
             *
             * Eventually, the last bits of the above were replaced by an
             * application of Veltkamp's splitting (see below). The inspiration
             * for that use case came from core-js' implementation at
             * https://github.com/zloirock/core-js/blob/a3f591658e063a6e2c2594ec3c80eff16340a98d/packages/core-js/internals/math-fround.js
             * The code does not mention Veltkamp's splitting, but the PR
             * discussion that led to it does, although with a question mark,
             * and without any explanation of how/why it works:
             * https://github.com/paulmillr/es6-shim/pull/140#issuecomment-91787165
             * We tracked down the descriptions and proofs relative to
             * Veltkamp's splitting and re-derived an implementation from there.
             *
             * The direct tests for this polyfill are the tests for `toFloat`
             * in org.scalajs.testsuite.compiler.DoubleTest.
             */
            // scalastyle:on line.size.limit
            val sign = varRef("sign")
            val av = varRef("av")
            val p = varRef("p")

            val Inf = double(Double.PositiveInfinity)
            val overflowThreshold = double(3.4028235677973366e38)
            val normalThreshold = double(1.1754943508222875e-38)

            val noTypedArrayPolyfill = genArrowFunction(paramList(v), Block(
              v := +v, // turns `null` into +0, making sure not to deoptimize what follows
              const(sign, If(v < 0, -1, 1)), // 1 for NaN, +0 and -0
              const(av, sign * v), // abs(v), or -0 if v is -0
              If(av >= overflowThreshold, { // also handles the case av === Infinity
                Return(sign * Inf)
              }, If(av >= normalThreshold, Block(
                /* Here, we know that both the input and output are expressed
                 * in a Double normal form, so standard floating point
                 * algorithms from papers can be used.
                 *
                 * We use Veltkamp's splitting, as described and studied in
                 *   Sylvie Boldo.
                 *   Pitfalls of a Full Floating-Point Proof: Example on the
                 *   Formal Proof of the Veltkamp/Dekker Algorithms
                 *   https://dx.doi.org/10.1007/11814771_6
                 * Section 3, with β = 2, t = 53, s = 53 - 24 = 29, x = av.
                 * 53 is the number of effective mantissa bits in a Double;
                 * 24 in a Float.
                 *
                 * ◦ is the round-to-nearest operation with a tie-breaking
                 * rule (in our case, break-to-even).
                 *
                 *   Let C = βˢ + 1 = 536870913
                 *   p = ◦(x × C)
                 *   q = ◦(x − p)
                 *   x₁ = ◦(p + q)
                 *
                 * Boldo proves that x₁ is the (t-s)-bit float closest to x,
                 * using the same tie-breaking rule as ◦. Since (t-s) = 24,
                 * this is the closest float32 (with 24 mantissa bits), and
                 * therefore the correct result of `fround`.
                 *
                 * Boldo also proves that if the computation of x × C does not
                 * cause overflow, then none of the following operations will
                 * cause overflow. We know that x (av) is less than the
                 * overflowThreshold, and overflowThreshold × C does not
                 * overflow, so that computation can never cause an overflow.
                 *
                 * If the reader does not have access to Boldo's paper, they
                 * may refer instead to
                 *   Claude-Pierre Jeannerod, Jean-Michel Muller, Paul Zimmermann.
                 *   On various ways to split a floating-point number.
                 *   ARITH 2018 - 25th IEEE Symposium on Computer Arithmetic,
                 *   Jun 2018, Amherst (MA), United States.
                 *   pp.53-60, 10.1109/ARITH.2018.8464793. hal-01774587v2
                 * available at
                 *   https://hal.inria.fr/hal-01774587v2/document
                 * Section III, although that paper defers some theorems and
                 * proofs to Boldo's.
                 */
                const(p, av * 536870913),
                Return(sign * (p + (av - p)))
              ), {
                /* Here, the result is represented as a subnormal form in a
                 * float32 representation.
                 *
                 * We round `av` to the nearest multiple of the smallest
                 * positive Float value (i.e., `Float.MinPositiveValue`),
                 * breaking ties to an even multiple.
                 *
                 * We do this by leveraging the inherent loss of precision near
                 * the minimum positive *double* value: conceptually, we divide
                 * the value by
                 *   Float.MinPositiveValue / Double.MinPositiveValue
                 * which will drop the excess precision, applying exactly the
                 * rounding strategy that we want. Then we multiply the value
                 * back by the same constant.
                 *
                 * However, `Float.MinPositiveValue / Double.MinPositiveValue`
                 * is not representable as a finite Double. Therefore, we
                 * instead use the *inverse* constant
                 *   Double.MinPositiveValue / Float.MinPositiveValue
                 * and we first multiply by that constant, then divide by it.
                 *
                 * ---
                 *
                 * As an additional "hack", the input values NaN, +0 and -0
                 * also fall in this code path. For them, this computation
                 * happens to be an identity, and is therefore correct as well.
                 */
                val roundingFactor = double(Double.MinPositiveValue / Float.MinPositiveValue.toDouble)
                Return(sign * ((av * roundingFactor) / roundingFactor))
              }))
            ))

            If(typeof(Float32ArrayRef) !== str("undefined"),
                typedArrayPolyfill, noTypedArrayPolyfill)
          }

        case PrivateSymbolBuiltin =>
          /* function privateJSFieldSymbol(description) {
           *   function rand32() {
           *     const s = ((Math.random() * 4294967296.0) >>> 0).toString(16);
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
              FunctionDef(rand32.ident, Nil, None, Block(
                  genLet(s.ident, mutable = false, {
                      val randomDouble =
                        Apply(genIdentBracketSelect(MathRef, "random"), Nil)
                      val randomUint =
                        (randomDouble * double(4294967296.0)) >>> 0
                      Apply(genIdentBracketSelect(randomUint, "toString"), 16 :: Nil)
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

        case GetOwnPropertyDescriptorsBuiltin =>
          /* getOwnPropertyDescriptors = (() => {
           *   // Fetch or polyfill Reflect.ownKeys
           *   var ownKeysFun;
           *   if (typeof Reflect != "undefined" && Reflect.ownKeys) {
           *     ownKeysFun = Reflect.ownKeys;
           *   } else {
           *     /* Fetch or polyfill Object.getOwnPropertySymbols.
           *      * We assume that if that function does not exist, then
           *      * symbols do not exist at all. Therefore, the result is
           *      * always an empty array.
           *      */
           *     var getOwnPropertySymbols = Object.getOwnPropertySymbols || (o => []);
           *
           *     // Polyfill for Reflect.ownKeys
           *     ownKeysFun = o => Object.getOwnPropertyNames(o).concat(getOwnPropertySymbols(o));
           *   }
           *
           *   // Polyfill for Object.getOwnPropertyDescriptors
           *   return (o => {
           *     var ownKeys = ownKeysFun(o);
           *     var descriptors = {};
           *     var len = ownKeys.length | 0;
           *     var i = 0;
           *     while (i !== len) {
           *       var key = ownKeys[i];
           *       /* Almost equivalent to
           *        *   descriptors[key] = Object.getOwnPropertyDescriptor(descriptors, key);
           *        * except that `defineProperty` will bypass any existing setter for
           *        * the property `key` on `descriptors` or in its prototype chain.
           *        */
           *       Object.defineProperty(descriptors, key, {
           *         configurable: true,
           *         enumerable: true,
           *         writable: true,
           *         value: Object.getOwnPropertyDescriptor(o, key)
           *       });
           *       i = (i + 1) | 0;
           *     }
           *     return descriptors;
           *   });
           * })();
           */
          val o = varRef("o")
          val ownKeysFun = varRef("ownKeysFun")
          val getOwnPropertySymbols = varRef("getOwnPropertySymbols")
          val ownKeys = varRef("ownKeys")
          val descriptors = varRef("descriptors")
          val len = varRef("len")
          val i = varRef("i")
          val key = varRef("key")

          val funGenerator = genArrowFunction(Nil, Block(
            VarDef(ownKeysFun.ident, None),
            If((typeof(ReflectRef) !== str("undefined")) && genIdentBracketSelect(ReflectRef, "ownKeys"), {
              ownKeysFun := genIdentBracketSelect(ReflectRef, "ownKeys")
            }, Block(
              const(getOwnPropertySymbols,
                  genIdentBracketSelect(ObjectRef, "getOwnPropertySymbols") ||
                  genArrowFunction(paramList(o), Return(ArrayConstr(Nil)))),
              ownKeysFun := genArrowFunction(paramList(o), Return {
                Apply(
                    genIdentBracketSelect(
                        Apply(genIdentBracketSelect(ObjectRef, "getOwnPropertyNames"), o :: Nil),
                        "concat"),
                    Apply(getOwnPropertySymbols, o :: Nil) :: Nil)
              })
            )),
            Return(genArrowFunction(paramList(o), Block(
              const(ownKeys, Apply(ownKeysFun, o :: Nil)),
              const(descriptors, ObjectConstr(Nil)),
              const(len, ownKeys.length | 0),
              let(i, 0),
              While(i !== len, Block(
                const(key, BracketSelect(ownKeys, i)),
                Apply(genIdentBracketSelect(ObjectRef, "defineProperty"), List(
                  descriptors,
                  key,
                  ObjectConstr(List(
                    str("configurable") -> bool(true),
                    str("enumerable") -> bool(true),
                    str("writable") -> bool(true),
                    str("value") -> {
                      Apply(
                          genIdentBracketSelect(ObjectRef, "getOwnPropertyDescriptor"),
                          o :: key :: Nil)
                    }
                  ))
                )),
                i := (i + 1) | 0
              )),
              Return(descriptors)
            )))
          ))

          Apply(funGenerator, Nil)
      }

      PolyfillableBuiltin.All.withFilter(esVersion < _.availableInESVersion).flatMap { builtin =>
        val polyfill = genPolyfillFor(builtin)
        val rhs = builtin match {
          case builtin: GlobalVarBuiltin =>
            // (typeof GlobalVar !== "undefined") ? GlobalVar : polyfill
            val globalVarRef = globalRef(builtin.globalVar)
            If(UnaryOp(JSUnaryOp.typeof, globalVarRef) !== str("undefined"),
                globalVarRef, polyfill)
          case builtin: NamespacedBuiltin =>
            // NamespaceGlobalVar.builtinName || polyfill
            genIdentBracketSelect(globalRef(builtin.namespaceGlobalVar), builtin.builtinName) || polyfill
        }
        extractWithGlobals(globalVarDef(builtin.polyfillField, CoreVar, rhs))
      }
    }

    private def declareCachedL0(): List[Tree] = {
      condDefs(!allowBigIntsForLongs)(
        extractWithGlobals(globalVarDecl(VarField.L0, CoreVar))
      )
    }

    private def assignCachedL0(): List[Tree] = {
      condDefs(!allowBigIntsForLongs)(List(
        globalVar(VarField.L0, CoreVar) := genScalaClassNew(
            LongImpl.RuntimeLongClass, LongImpl.initFromParts, 0, 0),
        genClassDataOf(LongRef) DOT cpn.zero := globalVar(VarField.L0, CoreVar)
      ))
    }

    private def defineCharClass(): List[Tree] = {
      val ctor = {
        val c = varRef("c")
        MethodDef(static = false, Ident("constructor"), paramList(c), None, {
          This() DOT cpn.c := c
        })
      }

      val toStr = {
        MethodDef(static = false, Ident("toString"), Nil, None, {
          Return(Apply(genIdentBracketSelect(StringRef, "fromCharCode"),
              (This() DOT cpn.c) :: Nil))
        })
      }

      if (useClassesForRegularClasses) {
        extractWithGlobals(globalClassDef(VarField.Char, CoreVar, None, ctor :: toStr :: Nil))
      } else {
        defineFunction(VarField.Char, ctor.args, ctor.body) :::
        setPrototypeVar(globalVar(VarField.Char, CoreVar)) :::
        assignES5ClassMembers(globalVar(VarField.Char, CoreVar), List(toStr))
      }
    }

    private def defineRuntimeFunctions(): List[Tree] = (
      condDefs(asInstanceOfs != CheckedBehavior.Unchecked || arrayStores != CheckedBehavior.Unchecked)(
        /* Returns a safe string description of a value.
         * This helper is never called for `value === null`. As implemented,
         * it would return `"object"` if it were.
         */
        defineFunction1(VarField.valueDescription) { value =>
          Return {
            If(typeof(value) === str("number"), {
              If((value === 0) && (int(1) / value < 0), {
                str("number(-0)")
              }, {
                str("number(") + value + str(")")
              })
            }, {
              val longOrBigIntTest =
                if (useBigIntForLongs) typeof(value) === str("bigint")
                else genIsInstanceOfHijackedClass(value, BoxedLongClass)
              If(longOrBigIntTest, {
                if (useBigIntForLongs)
                  str("bigint(") + value + str(")")
                else
                  str("long")
              }, {
                If(genIsInstanceOfHijackedClass(value, BoxedCharacterClass), {
                  str("char")
                }, {
                  If(genIsScalaJSObject(value), {
                    genIdentBracketSelect(value DOT classData, cpn.name)
                  }, {
                    typeof(value)
                  })
                })
              })
            })
          }
        }
      ) :::

      condDefs(asInstanceOfs != CheckedBehavior.Unchecked)(
        defineFunction2(VarField.throwClassCastException) { (instance, classFullName) =>
          Throw(maybeWrapInUBE(asInstanceOfs, {
            genScalaClassNew(ClassCastExceptionClass, StringArgConstructorName,
                genCallHelper(VarField.valueDescription, instance) + str(" cannot be cast to ") + classFullName)
          }))
        } :::

        defineFunction3(VarField.throwArrayCastException) { (instance, classArrayEncodedName, depth) =>
          Block(
              While(depth.prefix_--, {
                classArrayEncodedName := (str("[") + classArrayEncodedName)
              }),
              genCallHelper(VarField.throwClassCastException, instance, classArrayEncodedName)
          )
        }
      ) :::

      condDefs(arrayIndexOutOfBounds != CheckedBehavior.Unchecked)(
        defineFunction1(VarField.throwArrayIndexOutOfBoundsException) { i =>
          Throw(maybeWrapInUBE(arrayIndexOutOfBounds, {
            genScalaClassNew(ArrayIndexOutOfBoundsExceptionClass,
                StringArgConstructorName,
                If(i === Null(), Null(), str("") + i))
          }))
        }
      ) :::

      condDefs(arrayStores != CheckedBehavior.Unchecked)(
        defineFunction1(VarField.throwArrayStoreException) { v =>
          Throw(maybeWrapInUBE(arrayStores, {
            genScalaClassNew(ArrayStoreExceptionClass,
                StringArgConstructorName,
                If(v === Null(), Null(), genCallHelper(VarField.valueDescription, v)))
          }))
        }
      ) :::

      condDefs(negativeArraySizes != CheckedBehavior.Unchecked)(
        defineFunction0(VarField.throwNegativeArraySizeException) {
          Throw(maybeWrapInUBE(negativeArraySizes, {
            genScalaClassNew(NegativeArraySizeExceptionClass,
                NoArgConstructorName)
          }))
        }
      ) :::

      condDefs(moduleInit == CheckedBehavior.Fatal)(
        defineFunction1(VarField.throwModuleInitError) { name =>
          Throw(genScalaClassNew(UndefinedBehaviorErrorClass,
              StringArgConstructorName, str("Initializer of ") + name +
              str(" called before completion of its super constructor")))
        }
      ) :::

      condDefs(nullPointers != CheckedBehavior.Unchecked)(
        defineFunction0(VarField.throwNullPointerException) {
          Throw(maybeWrapInUBE(nullPointers, {
            genScalaClassNew(NullPointerExceptionClass, NoArgConstructorName)
          }))
        } :::

        // "checkNotNull", but with a very short name
        defineFunction1(VarField.n) { x =>
          Block(
            If(x === Null(), genCallHelper(VarField.throwNullPointerException)),
            Return(x)
          )
        }
      ) :::

      defineFunction1(VarField.noIsInstance) { instance =>
        Throw(New(TypeErrorRef,
            str("Cannot call isInstance() on a Class representing a JS trait/object") :: Nil))
      } :::

      defineFunction2(VarField.newArrayObject) { (arrayClassData, lengths) =>
        Return(genCallHelper(VarField.newArrayObjectInternal, arrayClassData, lengths, int(0)))
      } :::

      defineFunction3(VarField.newArrayObjectInternal) { (arrayClassData, lengths, lengthIndex) =>
        val result = varRef("result")
        val subArrayClassData = varRef("subArrayClassData")
        val subLengthIndex = varRef("subLengthIndex")
        val underlying = varRef("underlying")
        val i = varRef("i")

        Block(
          const(result, New(arrayClassData DOT cpn.constr,
              BracketSelect(lengths, lengthIndex) :: Nil)),
          If(lengthIndex < (lengths.length - 1), Block(
            const(subArrayClassData, arrayClassData DOT cpn.componentData),
            const(subLengthIndex, lengthIndex + 1),
            const(underlying, result.u),
            For(let(i, 0), i < underlying.length, i.++, {
              BracketSelect(underlying, i) :=
                genCallHelper(VarField.newArrayObjectInternal, subArrayClassData, lengths, subLengthIndex)
            })
          )),
          Return(result)
        )
      } :::

      defineFunction1(VarField.objectClone) { instance =>
        // return Object.create(Object.getPrototypeOf(instance), $getOwnPropertyDescriptors(instance));
        val callGetOwnPropertyDescriptors = genCallPolyfillableBuiltin(
            GetOwnPropertyDescriptorsBuiltin, instance)
        Return(Apply(genIdentBracketSelect(ObjectRef, "create"), List(
            Apply(genIdentBracketSelect(ObjectRef, "getPrototypeOf"), instance :: Nil),
            callGetOwnPropertyDescriptors)))
      } :::

      defineFunction1(VarField.objectOrArrayClone) { instance =>
        // return instance.$classData.isArrayClass ? instance.clone__O() : $objectClone(instance);
        Return(If(genIdentBracketSelect(instance DOT classData, cpn.isArrayClass),
            genApply(instance, cloneMethodName, Nil),
            genCallHelper(VarField.objectClone, instance)))
      }
    )

    private def defineObjectGetClassFunctions(): List[Tree] = {
      // objectGetClass and objectClassName

      def defineObjectGetClassBasedFun(name: VarField,
          constantClassResult: ClassName => Tree,
          scalaObjectResult: VarRef => Tree, jsObjectResult: Tree): List[Tree] = {
        defineFunction1(name) { instance =>
          Switch(typeof(instance), List(
              str("string") -> {
                Return(constantClassResult(BoxedStringClass))
              },
              str("number") -> {
                Block(
                    If(genCallHelper(VarField.isInt, instance), {
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
                        If(genCallHelper(VarField.isFloat, instance), {
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
              if (nullPointers == CheckedBehavior.Unchecked)
                Return(genApply(instance, getClassMethodName, Nil))
              else
                genCallHelper(VarField.throwNullPointerException)
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


      /* We use isClassClassInstantiated as an over-approximation of whether
       * the program contains any `GetClass` node. If `j.l.Class` is not
       * instantiated, then we know that there is no `GetClass` node, and it is
       * safe to omit the definition of `objectGetClass`. However, it is
       * possible that we generate `objectGetClass` even if it is not
       * necessary, in the case that `j.l.Class` is otherwise instantiated
       * (i.e., through a `ClassOf` node).
       */
      condDefs(globalKnowledge.isClassClassInstantiated)(
        defineObjectGetClassBasedFun(VarField.objectGetClass,
            className => genClassOf(className),
            instance => Apply(instance DOT classData DOT cpn.getClassOf, Nil),
            Null()
        )
      ) :::
      defineObjectGetClassBasedFun(VarField.objectClassName,
          { className =>
            StringLiteral(RuntimeClassNameMapperImpl.map(
                semantics.runtimeClassNameMapper, className.nameString))
          },
          instance => genIdentBracketSelect(instance DOT classData, cpn.name),
          {
            if (nullPointers == CheckedBehavior.Unchecked)
              genApply(Null(), getNameMethodName, Nil)
            else
              genCallHelper(VarField.throwNullPointerException)
          }
      )
    }

    private def defineDispatchFunctions(): List[Tree] = {
      val instance = varRef("instance")

      def defineDispatcher(methodName: MethodName, args: List[VarRef],
          body: Tree): List[Tree] = {
        val params = paramList((instance :: args): _*)
        extractWithGlobals(globalFunctionDef(VarField.dp, methodName, params, None, body))
      }

      /* A standard dispatcher performs a type test on the instance and then
       * calls the relevant implementation which is either of:
       *
       * - A normal method call if the instance is a normal scala class.
       * - A method in the relevant hijacked class.
       * - The implementation in java.lang.Object (if this is a JS object).
       */
      def defineStandardDispatcher(methodName: MethodName,
          implementingClasses: Set[ClassName]): List[Tree] = {

        val args =
          methodName.paramTypeRefs.indices.map(i => varRef("x" + i)).toList

        val targetHijackedClasses =
          subsetOfHijackedClassesOrderedForTypeTests(implementingClasses)
        val implementedInObject = implementingClasses.contains(ObjectClass)

        def hijackedClassNameToTypeof(className: ClassName): Option[String] = className match {
          case BoxedStringClass  => Some("string")
          case BoxedDoubleClass  => Some("number")
          case BoxedBooleanClass => Some("boolean")
          case BoxedUnitClass    => Some("undefined")
          case _                 => None
        }

        def genHijackedMethodApply(className: ClassName): Tree = {
          val instanceAsPrimitive =
            if (className == BoxedCharacterClass) genCallHelper(VarField.uC, instance)
            else instance
          Apply(globalVar(VarField.f, (className, methodName)), instanceAsPrimitive :: args)
        }

        def genBodyNoSwitch(hijackedClasses: List[ClassName]): Tree = {
          val normalCall = Return(genApply(instance, methodName, args))

          def hijackedDispatch(default: Tree) = {
            hijackedClasses.foldRight(default) { (className, next) =>
              If(genIsInstanceOfHijackedClass(instance, className),
                  Return(genHijackedMethodApply(className)),
                  next)
            }
          }

          if (implementedInObject) {
            val staticObjectCall: Tree = {
              val fun = globalVar(VarField.c, ObjectClass).prototype DOT genMethodIdent(methodName)
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
          val (classesWithTypeof, otherClasses) =
            targetHijackedClasses.span(hijackedClassNameToTypeof(_).isDefined)

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
            genBodyNoSwitch(targetHijackedClasses)
          }
        })
      }

      val methodsInRepresentativeClasses =
        globalKnowledge.methodsInRepresentativeClasses()

      methodsInRepresentativeClasses.flatMap { case (methodName, implementingClasses) =>
        if (methodName == toStringMethodName) {
          // toString()java.lang.String is special as per IR spec.
          defineDispatcher(toStringMethodName, Nil, {
            Return(If(instance === Undefined(),
                str("undefined"),
                Apply(instance DOT "toString", Nil)))
          })
        } else {
          defineStandardDispatcher(methodName, implementingClasses)
        }
      }
    }

    private def defineArithmeticOps(): List[Tree] = {
      val isArithmeticExceptionClassInstantiated =
        globalKnowledge.isArithmeticExceptionClassInstantiated

      def throwDivByZero: Tree = {
        Throw(genScalaClassNew(ArithmeticExceptionClass,
            StringArgConstructorName, str("/ by zero")))
      }

      def wrapBigInt64(tree: Tree): Tree =
        Apply(genIdentBracketSelect(BigIntRef, "asIntN"), 64 :: tree :: Nil)

      condDefs(isArithmeticExceptionClassInstantiated)(
        defineFunction2(VarField.intDiv) { (x, y) =>
          If(y === 0, throwDivByZero, {
            Return((x / y) | 0)
          })
        } :::
        defineFunction2(VarField.intMod) { (x, y) =>
          If(y === 0, throwDivByZero, {
            Return((x % y) | 0)
          })
        } :::
        Nil
      ) :::
      defineFunction1(VarField.doubleToInt) { x =>
        Return(If(x > 2147483647, 2147483647, If(x < -2147483648, -2147483648, x | 0)))
      } :::
      defineFunction1(VarField.charToString) { x =>
        Return(Apply(genIdentBracketSelect(StringRef, "fromCharCode"), x :: Nil))
      } :::
      condDefs(semantics.stringIndexOutOfBounds != CheckedBehavior.Unchecked)(
        defineFunction2(VarField.charAt) { (s, i) =>
          val r = varRef("r")

          val throwStringIndexOutOfBoundsException = {
            Throw(maybeWrapInUBE(semantics.stringIndexOutOfBounds,
                genScalaClassNew(StringIndexOutOfBoundsExceptionClass, IntArgConstructorName, i)))
          }

          Block(
            const(r, Apply(genIdentBracketSelect(s, "charCodeAt"), List(i))),
            If(r !== r, throwStringIndexOutOfBoundsException, Return(r))
          )
        }
      ) :::
      condDefs(allowBigIntsForLongs && isArithmeticExceptionClassInstantiated)(
        defineFunction2(VarField.longDiv) { (x, y) =>
          If(y === bigInt(0), throwDivByZero, {
            Return(wrapBigInt64(x / y))
          })
        } :::
        defineFunction2(VarField.longMod) { (x, y) =>
          If(y === bigInt(0), throwDivByZero, {
            Return(wrapBigInt64(x % y))
          })
        } :::
        Nil
      ) :::
      condDefs(allowBigIntsForLongs)(
        defineFunction1(VarField.doubleToLong)(x => Return {
          If(x < double(-9223372036854775808.0), { // -2^63
            bigInt(-9223372036854775808L)
          }, {
            If (x >= double(9223372036854775808.0), { // 2^63
              bigInt(9223372036854775807L)
            }, {
              If (x !== x, { // NaN
                bigInt(0L)
              }, {
                Apply(BigIntRef,
                    Apply(genIdentBracketSelect(MathRef, "trunc"), x :: Nil) :: Nil)
              })
            })
          })
        }) :::

        defineFunction1(VarField.longToFloat) { x =>
          val abs = varRef("abs")
          val y = varRef("y")
          val absR = varRef("absR")

          // See RuntimeLong.toFloat for the strategy
          Block(
            const(abs, If(x < bigInt(0L), -x, x)),
            const(y, If(abs <= bigInt(1L << 53) || (abs & bigInt(0xffffL)) === bigInt(0L), {
              abs
            }, {
              (abs & bigInt(~0xffffL)) | bigInt(0x8000L)
            })),
            const(absR, Apply(NumberRef, y :: Nil)),
            Return(genCallPolyfillableBuiltin(FroundBuiltin, If(x < bigInt(0L), -absR, absR)))
          )
        }
      )
    }

    private def defineES2015LikeHelpers(): List[Tree] = (
      condDefs(esVersion < ESVersion.ES2015)(
        defineFunction2(VarField.newJSObjectWithVarargs) { (ctor, args) =>
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
      ) :::

      defineFunction2(VarField.resolveSuperRef) { (superClass, propName) =>
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
      } :::

      defineFunction3(VarField.superGet) { (superClass, self, propName) =>
        val desc = varRef("desc")
        val getter = varRef("getter")

        Block(
          const(desc, genCallHelper(VarField.resolveSuperRef, superClass, propName)),
          If(desc !== Undefined(), Block(
            const(getter, genIdentBracketSelect(desc, "get")),
            Return(If(getter !== Undefined(),
                Apply(genIdentBracketSelect(getter, "call"), self :: Nil),
                genIdentBracketSelect(getter, "value")))
          ))
        )
      } :::

      defineFunction4(VarField.superSet) { (superClass, self, propName, value) =>
        val desc = varRef("desc")
        val setter = varRef("setter")

        Block(
          const(desc, genCallHelper(VarField.resolveSuperRef, superClass, propName)),
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

    private def defineModuleHelpers(): List[Tree] = {
      condDefs(moduleKind == ModuleKind.CommonJSModule)(
        defineFunction1(VarField.moduleDefault) { m =>
          Return(If(
              m && (typeof(m) === str("object")) && (str("default") in m),
              BracketSelect(m, str("default")),
              m))
        }
      )
    }

    private def defineIntrinsics(): List[Tree] = (
      condDefs(arrayIndexOutOfBounds != CheckedBehavior.Unchecked)(
        defineFunction5(VarField.arraycopyCheckBounds) { (srcLen, srcPos, destLen, destPos, length) =>
          If((srcPos < 0) || (destPos < 0) || (length < 0) ||
              (srcPos > ((srcLen - length) | 0)) ||
              (destPos > ((destLen - length) | 0)), {
            genCallHelper(VarField.throwArrayIndexOutOfBoundsException, Null())
          })
        }
      ) :::

      defineFunction5(VarField.arraycopyGeneric) { (srcArray, srcPos, destArray, destPos, length) =>
        val i = varRef("i")
        Block(
          if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
            genCallHelper(VarField.arraycopyCheckBounds, srcArray.length,
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
      } :::

      condDefs(esVersion < ESVersion.ES2015)(
        defineFunction5(VarField.systemArraycopy) { (src, srcPos, dest, destPos, length) =>
          genCallHelper(VarField.arraycopyGeneric, src.u, srcPos, dest.u, destPos, length)
        }
      ) :::
      condDefs(esVersion >= ESVersion.ES2015 && nullPointers != CheckedBehavior.Unchecked)(
        defineFunction5(VarField.systemArraycopy) { (src, srcPos, dest, destPos, length) =>
          genArrayClassPropApply(src, ArrayClassProperty.copyTo, srcPos, dest, destPos, length)
        }
      ) :::

      condDefs(arrayStores != CheckedBehavior.Unchecked)(
        defineFunction5(VarField.systemArraycopyRefs) { (src, srcPos, dest, destPos, length) =>
          If(Apply(genIdentBracketSelect(dest DOT classData, cpn.isAssignableFrom), List(src DOT classData)), {
            /* Fast-path, no need for array store checks. This always applies
             * for arrays of the same type, and a fortiori, when `src eq dest`.
             */
            genCallHelper(VarField.arraycopyGeneric, src.u, srcPos, dest.u, destPos, length)
          }, {
            /* Slow copy with "set" calls for every element. By construction,
             * we have `src ne dest` in this case.
             */
            val srcArray = varRef("srcArray")
            val i = varRef("i")
            Block(
              const(srcArray, src.u),
              condTree(arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
                genCallHelper(VarField.arraycopyCheckBounds, srcArray.length,
                    srcPos, dest.u.length, destPos, length)
              },
              For(let(i, 0), i < length, i := ((i + 1) | 0), {
                genArrayClassPropApply(dest, ArrayClassProperty.set,
                    (destPos + i) | 0, BracketSelect(srcArray, (srcPos + i) | 0))
              })
            )
          })
        } :::

        defineFunction5(VarField.systemArraycopyFull) { (src, srcPos, dest, destPos, length) =>
          val ObjectArray = globalVar(VarField.ac, ObjectClass)
          val srcData = varRef("srcData")

          Block(
            const(srcData, src && (src DOT classData)),
            If(srcData === (dest && (dest DOT classData)), {
              // Both values have the same "data" (could also be falsy values)
              If(srcData && genIdentBracketSelect(srcData, cpn.isArrayClass), {
                // Fast path: the values are array of the same type
                if (esVersion >= ESVersion.ES2015 && nullPointers == CheckedBehavior.Unchecked)
                  genArrayClassPropApply(src, ArrayClassProperty.copyTo, srcPos, dest, destPos, length)
                else
                  genCallHelper(VarField.systemArraycopy, src, srcPos, dest, destPos, length)
              }, {
                genCallHelper(VarField.throwArrayStoreException, Null())
              })
            }, {
              /* src and dest are of different types; the only situation that
               * can still be valid is if they are two reference array types.
               */
              If((src instanceof ObjectArray) && (dest instanceof ObjectArray), {
                genCallHelper(VarField.systemArraycopyRefs, src, srcPos, dest, destPos, length)
              }, {
                genCallHelper(VarField.throwArrayStoreException, Null())
              })
            })
          )
        }
      ) :::

      // systemIdentityHashCode
      locally {
        val WeakMapRef = globalRef("WeakMap")

        val lastIDHash = fileLevelVar(VarField.lastIDHash)
        val idHashCodeMap = fileLevelVar(VarField.idHashCodeMap)

        val obj = varRef("obj")
        val biHash = varRef("biHash")
        val description = varRef("description")
        val hash = varRef("hash")

        def functionSkeleton(defaultImpl: Tree): Function = {
          def genHijackedMethodApply(className: ClassName, arg: Tree): Tree =
            Apply(globalVar(VarField.f, (className, hashCodeMethodName)), arg :: Nil)

          def genReturnHijackedMethodApply(className: ClassName): Tree =
            Return(genHijackedMethodApply(className, obj))

          def genReturnBigIntHashCode(): Tree = {
            /* Xor together all the chunks of 32 bits. For negative numbers,
             * take their bitwise not first (otherwise we would go into an
             * infinite loop).
             *
             * This is compatible with the specified hash code of j.l.Long,
             * which is desirable: it means that the hashCode() of bigints does
             * not depend on whether we implement Longs as BigInts or not.
             * (By spec, x.hashCode() delegates to systemIdentityHashCode(x)
             * for bigints unless they fit in a Long and we implement Longs as
             * bigints.)
             *
             * let biHash = 0;
             * if (obj < 0n)
             *   obj = ~obj;
             * while (obj !== 0n) {
             *   biHash ^= Number(BigInt.asIntN(32, obj));
             *   obj >>= 32n;
             * }
             * return biHash;
             */

            def biLit(x: Int): Tree =
              if (esFeatures.allowBigIntsForLongs) bigInt(x)
              else Apply(BigIntRef, x :: Nil)

            def asInt32(arg: Tree): Tree =
              Apply(genIdentBracketSelect(BigIntRef, "asIntN"), 32 :: arg :: Nil)

            Block(
              let(biHash, 0),
              If(obj < biLit(0), obj := ~obj),
              While(obj !== biLit(0), Block(
                biHash := biHash ^ Apply(NumberRef, asInt32(obj) :: Nil),
                obj := (obj >> biLit(32))
              )),
              Return(biHash)
            )
          }

          def genReturnSymbolHashCode(): Tree = {
            /* Hash the `description` field of the symbol, which is either
             * `undefined` or a string.
             */

            Block(
              const(description, genIdentBracketSelect(obj, "description")),
              Return(If(description === Undefined(), 0,
                  genHijackedMethodApply(BoxedStringClass, description)))
            )
          }

          genArrowFunction(paramList(obj), {
            Switch(typeof(obj), List(
              str("string") -> genReturnHijackedMethodApply(BoxedStringClass),
              str("number") -> genReturnHijackedMethodApply(BoxedDoubleClass),
              str("bigint") -> genReturnBigIntHashCode(),
              str("boolean") -> Return(If(obj, 1231, 1237)),
              str("undefined") -> Return(0),
              str("symbol") -> genReturnSymbolHashCode()
            ), defaultImpl)
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

        List(
          let(lastIDHash, 0),
          const(idHashCodeMap,
              if (esVersion >= ESVersion.ES2015) New(WeakMapRef, Nil)
              else If(typeof(WeakMapRef) !== str("undefined"), New(WeakMapRef, Nil), Null()))
        ) ::: (
          if (esVersion >= ESVersion.ES2015) {
            val f = weakMapBasedFunction
            defineFunction(VarField.systemIdentityHashCode, f.args, f.body)
          } else {
            extractWithGlobals(globalVarDef(VarField.systemIdentityHashCode, CoreVar,
                If(idHashCodeMap !== Null(), weakMapBasedFunction, fieldBasedFunction)))
          }
        )
      }
    )

    private def defineIsPrimitiveFunctions(): List[Tree] = {
      def defineIsIntLike(name: VarField, specificTest: VarRef => Tree): List[Tree] = {
        defineFunction1(name) { v =>
          Return((typeof(v) === str("number")) && specificTest(v) &&
              ((int(1) / v) !== (int(1) / double(-0.0))))
        }
      }

      defineIsIntLike(VarField.isByte, v => (v << 24 >> 24) === v) :::
      defineIsIntLike(VarField.isShort, v => (v << 16 >> 16) === v) :::
      defineIsIntLike(VarField.isInt, v => (v | 0) === v) :::
      condDefs(allowBigIntsForLongs)(
        defineFunction1(VarField.isLong) { v =>
          Return((typeof(v) === str("bigint")) &&
              (Apply(genIdentBracketSelect(BigIntRef, "asIntN"), int(64) :: v :: Nil) === v))
        }
      ) :::
      condDefs(strictFloats)(
        defineFunction1(VarField.isFloat) { v =>
          Return((typeof(v) === str("number")) &&
              ((v !== v) || (genCallPolyfillableBuiltin(FroundBuiltin, v) === v)))
        }
      )
    }

    private def defineBoxFunctions(): List[Tree] = (
      // Boxes for Chars
      defineFunction1(VarField.bC) { c =>
        Return(New(globalVar(VarField.Char, CoreVar), c :: Nil))
      } :::
      extractWithGlobals(globalVarDef(VarField.bC0, CoreVar, genCallHelper(VarField.bC, 0)))
    ) ::: (
      if (asInstanceOfs != CheckedBehavior.Unchecked) {
        // Unboxes for everything
        def defineUnbox(name: VarField, boxedClassName: ClassName, resultExpr: VarRef => Tree): List[Tree] = {
          val fullName = boxedClassName.nameString
          defineFunction1(name)(v => Return {
            If(genIsInstanceOfHijackedClass(v, boxedClassName) || (v === Null()),
                resultExpr(v),
                genCallHelper(VarField.throwClassCastException, v, str(fullName)))
          })
        }

        (
          defineUnbox(VarField.uV, BoxedUnitClass, _ => Undefined()) :::
          defineUnbox(VarField.uZ, BoxedBooleanClass, v => !(!v)) :::
          defineUnbox(VarField.uC, BoxedCharacterClass, v => If(v === Null(), 0, v DOT cpn.c)) :::
          defineUnbox(VarField.uB, BoxedByteClass, _ | 0) :::
          defineUnbox(VarField.uS, BoxedShortClass, _ | 0) :::
          defineUnbox(VarField.uI, BoxedIntegerClass, _ | 0) :::
          defineUnbox(VarField.uJ, BoxedLongClass, v => If(v === Null(), genLongZero(), v)) :::

          /* Since the type test ensures that v is either null or a float, we can
           * use + instead of fround.
           */
          defineUnbox(VarField.uF, BoxedFloatClass, v => +v) :::

          defineUnbox(VarField.uD, BoxedDoubleClass, v => +v) :::
          defineUnbox(VarField.uT, BoxedStringClass, v => If(v === Null(), StringLiteral(""), v))
        )
      } else {
        // Unboxes for Chars and Longs
        (
          defineFunction1(VarField.uC) { v =>
            Return(If(v === Null(), 0, v DOT cpn.c))
          } :::
          defineFunction1(VarField.uJ) { v =>
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
    private def defineSpecializedArrayClasses(): List[Tree] = {
      specializedArrayTypeRefs.flatMap { componentTypeRef =>
        val ArrayClass = globalVar(VarField.ac, componentTypeRef)

        val isArrayOfObject = componentTypeRef == ClassRef(ObjectClass)
        val isTypedArray = usesUnderlyingTypedArray(componentTypeRef)

        val ctor = {
          val arg = varRef("arg")
          MethodDef(static = false, Ident("constructor"), paramList(arg), None, {
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
                genCallHelper(VarField.throwArrayIndexOutOfBoundsException, i))
          }

          val getName = genArrayClassPropertyForDef(ArrayClassProperty.get)
          val setName = genArrayClassPropertyForDef(ArrayClassProperty.set)

          List(
              MethodDef(static = false, getName, paramList(i), None, {
                Block(
                    boundsCheck,
                    Return(BracketSelect(This().u, i))
                )
              }),
              MethodDef(static = false, setName, paramList(i, v), None, {
                Block(
                    boundsCheck,
                    BracketSelect(This().u, i) := v
                )
              })
          )
        } else if (isArrayOfObject && arrayStores != CheckedBehavior.Unchecked) {
          /* We need to define a straightforward "set" method, without any
           * check necessary, which will be overridden in subclasses.
           */
          val i = varRef("i")
          val v = varRef("v")

          val setName = genArrayClassPropertyForDef(ArrayClassProperty.set)

          List(
            MethodDef(static = false, setName, paramList(i, v), None, {
              BracketSelect(This().u, i) := v
            })
          )
        } else {
          Nil
        }

        val copyTo = if (esVersion >= ESVersion.ES2015) {
          val srcPos = varRef("srcPos")
          val dest = varRef("dest")
          val destPos = varRef("destPos")
          val length = varRef("length")

          val copyToName = genArrayClassPropertyForDef(ArrayClassProperty.copyTo)

          val methodDef = MethodDef(static = false, copyToName,
              paramList(srcPos, dest, destPos, length), None, {
            if (isTypedArray) {
              Block(
                  if (semantics.arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
                    genCallHelper(VarField.arraycopyCheckBounds, This().u.length,
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
              genCallHelper(VarField.arraycopyGeneric, This().u, srcPos,
                  dest.u, destPos, length)
            }
          })
          methodDef :: Nil
        } else {
          Nil
        }

        val cloneMethodIdent = genMethodIdentForDef(cloneMethodName, NoOriginalName)
        val clone = MethodDef(static = false, cloneMethodIdent, Nil, None, {
          Return(New(ArrayClass,
              Apply(genIdentBracketSelect(This().u, "slice"), Nil) :: Nil))
        })

        val members = getAndSet ::: copyTo ::: clone :: Nil

        if (useClassesForRegularClasses) {
          extractWithGlobals(globalClassDef(VarField.ac, componentTypeRef,
              Some(globalVar(VarField.c, ObjectClass)), ctor :: members))
        } else {
          val clsDef = {
            extractWithGlobals(globalFunctionDef(VarField.ac, componentTypeRef,
                ctor.args, ctor.restParam, ctor.body)) :::
            genAssignPrototype(ArrayClass, New(globalVar(VarField.h, ObjectClass), Nil)) ::
            (prototypeFor(ArrayClass) DOT "constructor" := ArrayClass) ::
            assignES5ClassMembers(ArrayClass, members)
          }

          componentTypeRef match {
            case _: ClassRef =>
              clsDef :::
              extractWithGlobals(globalFunctionDef(VarField.ah, ObjectClass, Nil, None, Skip())) :::
              (globalVar(VarField.ah, ObjectClass).prototype := prototypeFor(ArrayClass)) :: Nil
            case _: PrimRef =>
              clsDef
          }
        }
      }
    }

    private def genArrayClassConstructorBody(arg: VarRef,
        componentTypeRef: NonArrayTypeRef): Tree = {
      val i = varRef("i")

      If(typeof(arg) === str("number"), {
        val arraySizeCheck = condTree(negativeArraySizes != CheckedBehavior.Unchecked) {
          If(arg < 0, genCallHelper(VarField.throwNegativeArraySizeException))
        }

        getArrayUnderlyingTypedArrayClassRef(componentTypeRef) match {
          case Some(typeArrayClassWithGlobalRefs) =>
            Block(
                arraySizeCheck,
                This().u := New(extractWithGlobals(typeArrayClassWithGlobalRefs), arg :: Nil)
            )
          case None =>
            Block(
                arraySizeCheck,
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

    private def defineTypeDataClass(): List[Tree] = {
      def privateFieldSet(fieldName: String, value: Tree): Tree =
        This() DOT fieldName := value

      def publicFieldSet(fieldName: String, value: Tree): Tree =
        genIdentBracketSelect(This(), fieldName) := value

      val ctor = {
        MethodDef(static = false, Ident("constructor"), Nil, None, {
          Block(
              privateFieldSet(cpn.constr, Undefined()),
              if (globalKnowledge.isParentDataAccessed)
                privateFieldSet(cpn.parentData, Undefined())
              else
                Skip(),
              privateFieldSet(cpn.ancestors, Null()),
              privateFieldSet(cpn.componentData, Null()),
              privateFieldSet(cpn.arrayBase, Null()),
              privateFieldSet(cpn.arrayDepth, int(0)),
              privateFieldSet(cpn.zero, Null()),
              privateFieldSet(cpn.arrayEncodedName, str("")),
              privateFieldSet(cpn._classOf, Undefined()),
              privateFieldSet(cpn._arrayOf, Undefined()),

              /* A lambda for the logic of the public `isAssignableFrom`,
               * without its fast-path. See the comment on the definition of
               * `isAssignableFrom` for the rationale of this decomposition.
               */
              privateFieldSet(cpn.isAssignableFromFun, Undefined()),

              privateFieldSet(cpn.wrapArray, Undefined()),
              privateFieldSet(cpn.isJSType, bool(false)),

              publicFieldSet(cpn.name, str("")),
              publicFieldSet(cpn.isPrimitive, bool(false)),
              publicFieldSet(cpn.isInterface, bool(false)),
              publicFieldSet(cpn.isArrayClass, bool(false)),
              publicFieldSet(cpn.isInstance, Undefined())
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
        MethodDef(static = false, Ident(cpn.initPrim),
            paramList(zero, arrayEncodedName, displayName, arrayClass, typedArrayClass), None, {
          Block(
              privateFieldSet(cpn.ancestors, ObjectConstr(Nil)),
              privateFieldSet(cpn.zero, zero),
              privateFieldSet(cpn.arrayEncodedName, arrayEncodedName),
              const(self, This()), // capture `this` for use in arrow fun
              privateFieldSet(cpn.isAssignableFromFun,
                  genArrowFunction(paramList(that), Return(that === self))),
              publicFieldSet(cpn.name, displayName),
              publicFieldSet(cpn.isPrimitive, bool(true)),
              publicFieldSet(cpn.isInstance,
                  genArrowFunction(paramList(obj), Return(bool(false)))),
              If(arrayClass !== Undefined(), { // it is undefined for void
                privateFieldSet(cpn._arrayOf,
                    Apply(New(globalVar(VarField.TypeData, CoreVar), Nil) DOT cpn.initSpecializedArray,
                        List(This(), arrayClass, typedArrayClass)))
              }),
              Return(This())
          )
        })
      }

      val initClass = {
        /* This is either:
         * - an int: 1 means isInterface; 2 means isJSType; 0 otherwise
         * - a Scala class constructor: means 0 + assign `kindOrCtor.prototype.$classData = this;`
         */
        val kindOrCtor = varRef("kindOrCtor")

        val hasParentData = globalKnowledge.isParentDataAccessed

        val fullName = varRef("fullName")
        val ancestors = varRef("ancestors")
        val parentData = varRef("parentData")
        val isInstance = varRef("isInstance")
        val internalName = varRef("internalName")
        val that = varRef("that")
        val depth = varRef("depth")
        val obj = varRef("obj")
        val params =
          if (hasParentData) paramList(kindOrCtor, fullName, ancestors, parentData, isInstance)
          else paramList(kindOrCtor, fullName, ancestors, isInstance)
        MethodDef(static = false, Ident(cpn.initClass), params, None, {
          Block(
              /* Extract the internalName, which is the first property of ancestors.
               * We use `getOwnPropertyNames()`, which since ES 2015 guarantees
               * to return non-integer string keys in creation order.
               */
              const(internalName,
                  BracketSelect(Apply(genIdentBracketSelect(ObjectRef, "getOwnPropertyNames"), List(ancestors)), 0)),
              if (hasParentData)
                privateFieldSet(cpn.parentData, parentData)
              else
                Skip(),
              privateFieldSet(cpn.ancestors, ancestors),
              privateFieldSet(cpn.arrayEncodedName, str("L") + fullName + str(";")),
              privateFieldSet(cpn.isAssignableFromFun, {
                genArrowFunction(paramList(that), {
                  Return(!(!(BracketSelect(that DOT cpn.ancestors, internalName))))
                })
              }),
              privateFieldSet(cpn.isJSType, kindOrCtor === 2),
              publicFieldSet(cpn.name, fullName),
              publicFieldSet(cpn.isInterface, kindOrCtor === 1),
              publicFieldSet(cpn.isInstance, isInstance || {
                genArrowFunction(paramList(obj), {
                  Return(!(!(obj && (obj DOT classData) &&
                      BracketSelect(obj DOT classData DOT cpn.ancestors, internalName))))
                })
              }),
              If(typeof(kindOrCtor) !== str("number"), {
                kindOrCtor.prototype DOT cpn.classData := This()
              }),
              Return(This())
          )
        })
      }

      def initArrayCommonBody(arrayClass: VarRef, componentData: VarRef,
          arrayBase: VarRef, arrayDepth: Tree): Tree = {
        val name = varRef("name")

        Block(
            const(name, str("[") + (componentData DOT cpn.arrayEncodedName)),
            privateFieldSet(cpn.constr, arrayClass),
            if (globalKnowledge.isParentDataAccessed)
              privateFieldSet(cpn.parentData, genClassDataOf(ObjectClass))
            else
              Skip(),
            privateFieldSet(cpn.ancestors, ObjectConstr(List(
                genAncestorIdent(CloneableClass) -> 1,
                genAncestorIdent(SerializableClass) -> 1
            ))),
            privateFieldSet(cpn.componentData, componentData),
            privateFieldSet(cpn.arrayBase, arrayBase),
            privateFieldSet(cpn.arrayDepth, arrayDepth),
            privateFieldSet(cpn.arrayEncodedName, name),
            publicFieldSet(cpn.name, name),
            publicFieldSet(cpn.isArrayClass, bool(true))
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
        MethodDef(static = false, Ident(cpn.initSpecializedArray),
            paramList(componentData, arrayClass, typedArrayClass, isAssignableFromFun), None, {
          Block(
              arrayClass.prototype DOT classData := This(),
              initArrayCommonBody(arrayClass, componentData, componentData, 1),
              const(self, This()), // capture `this` for use in arrow fun
              privateFieldSet(cpn.isAssignableFromFun, isAssignableFromFun || {
                genArrowFunction(paramList(that), Return(self === that))
              }),
              privateFieldSet(cpn.wrapArray, {
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
              publicFieldSet(cpn.isInstance,
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
        MethodDef(static = false, Ident(cpn.initArray),
            paramList(componentData), None, {
          val ArrayClassDef = {
            val ctor = {
              val arg = varRef("arg")
              val i = varRef("i")
              MethodDef(static = false, Ident("constructor"), paramList(arg), None, {
                if (useClassesForRegularClasses)
                  Apply(Super(), arg :: Nil)
                else
                  genArrayClassConstructorBody(arg, ClassRef(ObjectClass))
              })
            }

            val set = if (arrayStores != CheckedBehavior.Unchecked) {
              val i = varRef("i")
              val v = varRef("v")

              val setName = genArrayClassPropertyForDef(ArrayClassProperty.set)

              val boundsCheck = condTree(arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
                If((i < 0) || (i >= This().u.length),
                    genCallHelper(VarField.throwArrayIndexOutOfBoundsException, i))
              }

              val storeCheck = {
                If((v !== Null()) && !(componentData DOT cpn.isJSType) &&
                    !Apply(genIdentBracketSelect(componentData, cpn.isInstance), v :: Nil),
                    genCallHelper(VarField.throwArrayStoreException, v))
              }

              List(
                MethodDef(static = false, setName, paramList(i, v), None, {
                  Block(
                      boundsCheck,
                      storeCheck,
                      BracketSelect(This().u, i) := v
                  )
                })
              )
            } else {
              Nil
            }

            val copyTo = if (esVersion >= ESVersion.ES2015) {
              val srcPos = varRef("srcPos")
              val dest = varRef("dest")
              val destPos = varRef("destPos")
              val length = varRef("length")

              val copyToName = genArrayClassPropertyForDef(ArrayClassProperty.copyTo)

              val methodDef = MethodDef(static = false, copyToName,
                  paramList(srcPos, dest, destPos, length), None, {
                genCallHelper(VarField.arraycopyGeneric, This().u, srcPos,
                    dest.u, destPos, length)
              })
              methodDef :: Nil
            } else {
              Nil
            }

            val cloneMethodIdent = genMethodIdentForDef(cloneMethodName, NoOriginalName)
            val clone = MethodDef(static = false, cloneMethodIdent, Nil, None, {
              Return(New(ArrayClass,
                  Apply(genIdentBracketSelect(This().u, "slice"), Nil) :: Nil))
            })

            val members = set ::: copyTo ::: clone :: Nil

            if (useClassesForRegularClasses) {
              Block(
                  ClassDef(Some(ArrayClass.ident), Some(globalVar(VarField.ac, ObjectClass)),
                      ctor :: members),
                  ArrayClass.prototype DOT cpn.classData := This()
              )
            } else {
              Block(
                FunctionDef(ArrayClass.ident, ctor.args, ctor.restParam, ctor.body) ::
                genAssignPrototype(ArrayClass, New(globalVar(VarField.ah, ObjectClass), Nil), localDecl = true) ::
                (prototypeFor(ArrayClass) DOT "constructor" := ArrayClass) ::
                assignES5ClassMembers(ArrayClass, members) :::
                (prototypeFor(ArrayClass) DOT cpn.classData := This()) ::
                Nil
              )
            }
          }

          Block(
              ArrayClassDef,
              const(arrayBase, (componentData DOT cpn.arrayBase) || componentData),
              const(arrayDepth, (componentData DOT cpn.arrayDepth) + 1),
              initArrayCommonBody(ArrayClass, componentData, arrayBase, arrayDepth),
              const(isAssignableFromFun, {
                genArrowFunction(paramList(that), {
                  val thatDepth = varRef("thatDepth")
                  Block(
                      const(thatDepth, that DOT cpn.arrayDepth),
                      Return(If(thatDepth === arrayDepth, {
                        Apply(arrayBase DOT cpn.isAssignableFromFun, (that DOT cpn.arrayBase) :: Nil)
                      }, {
                        (thatDepth > arrayDepth) && (arrayBase === genClassDataOf(ObjectClass))
                      }))
                  )
                })
              }),
              privateFieldSet(cpn.isAssignableFromFun, isAssignableFromFun),
              privateFieldSet(cpn.wrapArray, genArrowFunction(paramList(array), {
                Return(New(ArrayClass, array :: Nil))
              })),
              const(self, This()), // don't rely on the lambda being called with `this` as receiver
              publicFieldSet(cpn.isInstance, genArrowFunction(paramList(obj), {
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
        MethodDef(static = false, Ident(cpn.getArrayOf), Nil, None, {
          Block(
              If(!(This() DOT cpn._arrayOf),
                  This() DOT cpn._arrayOf :=
                    Apply(New(globalVar(VarField.TypeData, CoreVar), Nil) DOT cpn.initArray, This() :: Nil),
                  Skip()),
              Return(This() DOT cpn._arrayOf)
          )
        })
      }

      def getClassOf = {
        MethodDef(static = false, Ident(cpn.getClassOf), Nil, None, {
          Block(
              If(!(This() DOT cpn._classOf),
                  This() DOT cpn._classOf := genScalaClassNew(ClassClass, ObjectArgConstructorName, This()),
                  Skip()),
              Return(This() DOT cpn._classOf)
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
        MethodDef(static = false, StringLiteral(cpn.isAssignableFrom),
            paramList(that), None, {
          Return(
              (This() === that) || // fast path
              Apply(This() DOT cpn.isAssignableFromFun, that :: Nil))
        })
      }

      def checkCast = {
        val obj = varRef("obj")
        MethodDef(static = false, StringLiteral(cpn.checkCast), paramList(obj), None,
          if (asInstanceOfs != CheckedBehavior.Unchecked) {
            If((obj !== Null()) && !(This() DOT cpn.isJSType) &&
                !Apply(genIdentBracketSelect(This(), cpn.isInstance), obj :: Nil),
              genCallHelper(VarField.throwClassCastException, obj, genIdentBracketSelect(This(), cpn.name)),
              Skip())
          } else {
            Skip()
          }
        )
      }

      def getSuperclass = {
        MethodDef(static = false, StringLiteral(cpn.getSuperclass), Nil, None, {
          Return(If(This() DOT cpn.parentData,
              Apply(This() DOT cpn.parentData DOT cpn.getClassOf, Nil),
              Null()))
        })
      }

      def getComponentType = {
        MethodDef(static = false, StringLiteral(cpn.getComponentType), Nil, None, {
          Return(If(This() DOT cpn.componentData,
              Apply(This() DOT cpn.componentData DOT cpn.getClassOf, Nil),
              Null()))
        })
      }

      def newArrayOfThisClass = {
        val lengths = varRef("lengths")
        val arrayClassData = varRef("arrayClassData")
        val i = varRef("i")
        MethodDef(static = false, StringLiteral(cpn.newArrayOfThisClass),
            paramList(lengths), None, {
          Block(
              let(arrayClassData, This()),
              For(let(i, 0), i < lengths.length, i.++, {
                arrayClassData := Apply(arrayClassData DOT cpn.getArrayOf, Nil)
              }),
              Return(genCallHelper(VarField.newArrayObject, arrayClassData, lengths))
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
          } else if (arrayStores != CheckedBehavior.Unchecked) {
            List(
              isAssignableFrom
            )
          } else {
            Nil
          }
      )

      if (useClassesForRegularClasses) {
        extractWithGlobals(globalClassDef(VarField.TypeData, CoreVar, None, ctor :: members))
      } else {
        defineFunction(VarField.TypeData, ctor.args, ctor.body) :::
        setPrototypeVar(globalVar(VarField.TypeData, CoreVar)) :::
        assignES5ClassMembers(globalVar(VarField.TypeData, CoreVar), members)
      }
    }

    private def defineSpecializedIsArrayOfFunctions(): List[Tree] = {
      // isArrayOf_O
      val obj = varRef("obj")
      val depth = varRef("depth")
      val data = varRef("data")
      val arrayDepth = varRef("arrayDepth")

      val forObj = extractWithGlobals(globalFunctionDef(VarField.isArrayOf, ObjectClass, paramList(obj, depth), None, {
        Block(
            const(data, obj && (obj DOT cpn.classData)),
            If(!data, {
              Return(BooleanLiteral(false))
            }, {
              Block(
                  const(arrayDepth, data DOT cpn.arrayDepth),
                  Return(If(arrayDepth === depth, {
                    !genIdentBracketSelect(data DOT cpn.arrayBase, cpn.isPrimitive)
                  }, {
                    arrayDepth > depth
                  }))
              )
            })
        )
      }))

      val forPrims = orderedPrimRefsWithoutVoid.flatMap { primRef =>
        val obj = varRef("obj")
        val depth = varRef("depth")
        extractWithGlobals(globalFunctionDef(VarField.isArrayOf, primRef, paramList(obj, depth), None, {
          Return(!(!(obj && (obj DOT classData) &&
              ((obj DOT classData DOT cpn.arrayDepth) === depth) &&
              ((obj DOT classData DOT cpn.arrayBase) === genClassDataOf(primRef)))))
        }))
      }

      forObj ::: forPrims
    }

    private def defineSpecializedAsArrayOfFunctions(): List[Tree] = {
      condDefs(asInstanceOfs != CheckedBehavior.Unchecked)(
        specializedArrayTypeRefs.flatMap { typeRef =>
          val encodedName = typeRef match {
            case typeRef: PrimRef => typeRef.charCode.toString()
            case _                => "L" + ObjectClass.nameString + ";"
          }

          val obj = varRef("obj")
          val depth = varRef("depth")
          extractWithGlobals(globalFunctionDef(VarField.asArrayOf, typeRef, paramList(obj, depth), None, {
            If(Apply(globalVar(VarField.isArrayOf, typeRef), obj :: depth :: Nil) || (obj === Null()), {
              Return(obj)
            }, {
              genCallHelper(VarField.throwArrayCastException, obj, str(encodedName), depth)
            })
          }))
        }
      )
    }

    private def defineSpecializedTypeDatas(): List[Tree] = {
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

        val typeDataVar = globalVar(VarField.d, ObjectClass)

        def privateFieldSet(fieldName: String, value: Tree): Tree =
          typeDataVar DOT fieldName := value

        def publicFieldSet(fieldName: String, value: Tree): Tree =
          genIdentBracketSelect(typeDataVar, fieldName) := value

        extractWithGlobals(
            globalVarDef(VarField.d, ObjectClass, New(globalVar(VarField.TypeData, CoreVar), Nil))) :::
        List(
          privateFieldSet(cpn.ancestors, ObjectConstr(Nil)),
          privateFieldSet(cpn.arrayEncodedName, str("L" + fullName + ";")),
          privateFieldSet(cpn.isAssignableFromFun, {
            genArrowFunction(paramList(that), {
              Return(!genIdentBracketSelect(that, cpn.isPrimitive))
            })
          }),
          publicFieldSet(cpn.name, str(fullName)),
          publicFieldSet(cpn.isInstance,
              genArrowFunction(paramList(obj), Return(obj !== Null()))),
          privateFieldSet(cpn._arrayOf, {
            Apply(New(globalVar(VarField.TypeData, CoreVar), Nil) DOT cpn.initSpecializedArray, List(
              typeDataVar,
              globalVar(VarField.ac, ObjectClass),
              Undefined(), // typedArray
              genArrowFunction(paramList(that), {
                val thatDepth = varRef("thatDepth")
                Block(
                    const(thatDepth, that DOT cpn.arrayDepth),
                    Return(If(thatDepth === 1, {
                      !genIdentBracketSelect(that DOT cpn.arrayBase, cpn.isPrimitive)
                    }, {
                      (thatDepth > 1)
                    }))
                )
              })
            ))
          }),
          globalVar(VarField.c, ObjectClass).prototype DOT cpn.classData := typeDataVar
        )
      }

      val prims = orderedPrimRefs.flatMap { primRef =>
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

        extractWithGlobals(globalVarDef(VarField.d, primRef, {
          Apply(New(globalVar(VarField.TypeData, CoreVar), Nil) DOT cpn.initPrim,
              List(zero, str(primRef.charCode.toString()),
                  str(primRef.displayName),
                  if (primRef == VoidRef) Undefined()
                  else genArrayConstrOf(ArrayTypeRef(primRef, 1)),
                  typedArrayClass))
        }))
      }

      obj ::: prims
    }

    private def assignES5ClassMembers(classRef: Tree, members: List[MethodDef]): List[Tree] = {
      for {
        MethodDef(static, name, args, restParam, body) <- members
      } yield {
        val target = if (static) classRef else prototypeFor(classRef)
        genPropSelect(target, name) := Function(arrow = false, args, restParam, body)
      }
    }

    private def defineFunction(name: VarField, args: List[ParamDef], body: Tree): List[Tree] =
      extractWithGlobals(globalFunctionDef(name, CoreVar, args, None, body))

    private val argRefs = List.tabulate(5)(i => varRef("arg" + i))

    private def defineFunction0(name: VarField)(body: Tree): List[Tree] =
      defineFunction(name, Nil, body)

    private def defineFunction1(name: VarField)(body: VarRef => Tree): List[Tree] = {
      val a :: _ = argRefs
      defineFunction(name, paramList(a), body(a))
    }

    private def defineFunction2(name: VarField)(body: (VarRef, VarRef) => Tree): List[Tree] = {
      val a :: b :: _ = argRefs
      defineFunction(name, paramList(a, b), body(a, b))
    }

    private def defineFunction3(name: VarField)(body: (VarRef, VarRef, VarRef) => Tree): List[Tree] = {
      val a :: b :: c :: _ = argRefs
      defineFunction(name, paramList(a, b, c), body(a, b, c))
    }

    private def defineFunction4(name: VarField)(body: (VarRef, VarRef, VarRef, VarRef) => Tree): List[Tree] = {
      val a :: b :: c :: d :: _ = argRefs
      defineFunction(name, paramList(a, b, c, d), body(a, b, c, d))
    }

    private def defineFunction5(name: VarField)(body: (VarRef, VarRef, VarRef, VarRef, VarRef) => Tree): List[Tree] = {
      val a :: b :: c :: d :: e :: _ = argRefs
      defineFunction(name, paramList(a, b, c, d, e), body(a, b, c, d, e))
    }

    private def genArrowFunction(args: List[ParamDef], body: Tree): Function =
      jsGen.genArrowFunction(args, None, body)

    private def genCallPolyfillableBuiltin(builtin: PolyfillableBuiltin,
        args: Tree*): Tree = {
      extractWithGlobals(sjsGen.genCallPolyfillableBuiltin(builtin, args: _*))
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

    private def condDefs(cond: Boolean)(trees: => List[Tree]): List[Tree] =
      if (cond) trees
      else Nil

    private def varRef(name: String): VarRef = VarRef(Ident(name))

    private def const(ref: VarRef, rhs: Tree): LocalDef =
      genConst(ref.ident, rhs)

    private def let(ref: VarRef, rhs: Tree): LocalDef =
      genLet(ref.ident, mutable = true, rhs)

    private def paramList(refs: VarRef*): List[ParamDef] =
      refs.toList.map(ref => ParamDef(ref.ident))

    private def str(s: String): StringLiteral = StringLiteral(s)

    private def bool(b: Boolean): BooleanLiteral = BooleanLiteral(b)

    /* This one is implicit because there are *many* ints in the trees we
     * created, so this helps readability.
     */
    private implicit def int(i: Int): IntLiteral = IntLiteral(i)

    private def double(d: Double): DoubleLiteral = DoubleLiteral(d)

    private def bigInt(i: Long): BigIntLiteral = BigIntLiteral(i)

    // cannot extend AnyVal because this is not a static class
    private implicit class CustomTreeOps(private val self: Tree) {
      def u: Tree = genArrayClassPropSelect(self, ArrayClassProperty.u)
    }
  }
}
