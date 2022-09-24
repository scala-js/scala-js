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
    private val NumberRef = globalRef("Number")
    private val TypeErrorRef = globalRef("TypeError")
    private def BigIntRef = globalRef("BigInt")
    private val SymbolRef = globalRef("Symbol")

    // Conditional global references that we often use
    private def ReflectRef = globalRef("Reflect")

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
          str("esVersion") -> int(esVersion.edition),
          str("assumingES6") -> bool(useECMAScript2015Semantics), // different name for historical reasons
          str("productionMode") -> bool(productionMode),
          str("linkerVersion") -> str(ScalaJSVersions.current),
          str("fileLevelThis") -> This()
      )))

      extractWithGlobals(globalVarDef("linkingInfo", CoreVar, linkingInfo))
    }

    private def defineJSBuiltinsSnapshotsAndPolyfills(): Tree = {
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

        case Clz32Builtin =>
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

      val polyfillDefs = for {
        builtin <- PolyfillableBuiltin.All
        if esVersion < builtin.availableInESVersion
      } yield {
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
        extractWithGlobals(globalVarDef(builtin.builtinName, CoreVar, rhs))
      }
      Block(polyfillDefs)
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
        MethodDef(static = false, Ident("constructor"), paramList(c), None, {
          This() DOT "c" := c
        })
      }

      val toStr = {
        MethodDef(static = false, Ident("toString"), Nil, None, {
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
      condTree(asInstanceOfs != CheckedBehavior.Unchecked || arrayErrors != CheckedBehavior.Unchecked)(
        /* Returns a safe string description of a value.
         * This helper is never called for `value === null`. As implemented,
         * it would return `"object"` if it were.
         */
        defineFunction1("valueDescription") { value =>
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
                    genIdentBracketSelect(value DOT classData, "name")
                  }, {
                    typeof(value)
                  })
                })
              })
            })
          }
        }
      ),

      condTree(asInstanceOfs != CheckedBehavior.Unchecked)(Block(
        defineFunction2("throwClassCastException") { (instance, classFullName) =>
          Throw(maybeWrapInUBE(asInstanceOfs, {
            genScalaClassNew(ClassCastExceptionClass, StringArgConstructorName,
                genCallHelper("valueDescription", instance) + str(" cannot be cast to ") + classFullName)
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

      condTree(arrayErrors != CheckedBehavior.Unchecked)(Block(
        defineFunction1("throwArrayIndexOutOfBoundsException") { i =>
          Throw(maybeWrapInUBE(arrayErrors, {
            genScalaClassNew(ArrayIndexOutOfBoundsExceptionClass,
                StringArgConstructorName,
                If(i === Null(), Null(), str("") + i))
          }))
        },

        defineFunction1("throwArrayStoreException") { v =>
          Throw(maybeWrapInUBE(arrayErrors, {
            genScalaClassNew(ArrayStoreExceptionClass,
                StringArgConstructorName,
                If(v === Null(), Null(), genCallHelper("valueDescription", v)))
          }))
        }
      )),

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
      },

      defineFunction1("objectClone") { instance =>
        // return Object.create(Object.getPrototypeOf(instance), $getOwnPropertyDescriptors(instance));
        val callGetOwnPropertyDescriptors = genCallPolyfillableBuiltin(
            GetOwnPropertyDescriptorsBuiltin, instance)
        Return(Apply(genIdentBracketSelect(ObjectRef, "create"), List(
            Apply(genIdentBracketSelect(ObjectRef, "getPrototypeOf"), instance :: Nil),
            callGetOwnPropertyDescriptors)))
      },

      defineFunction1("objectOrArrayClone") { instance =>
        // return instance.$classData.isArrayClass ? instance.clone__O() : $objectClone(instance);
        Return(If(genIdentBracketSelect(instance DOT classData, "isArrayClass"),
            Apply(instance DOT genName(cloneMethodName), Nil),
            genCallHelper("objectClone", instance)))
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
          implementingClasses: Set[ClassName]): Tree = {

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
            if (className == BoxedCharacterClass) genCallHelper("uC", instance)
            else instance
          Apply(globalVar("f", (className, methodName)), instanceAsPrimitive :: args)
        }

        def genBodyNoSwitch(hijackedClasses: List[ClassName]): Tree = {
          val normalCall = Return(Apply(instance DOT genName(methodName), args))

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

      val dispatchers = for {
        (methodName, implementingClasses) <- methodsInRepresentativeClasses
      } yield {
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

        condTree(semantics.stringIndexOutOfBounds != CheckedBehavior.Unchecked)(
          defineFunction2("charAt") { (s, i) =>
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
        ),

        condTree(allowBigIntsForLongs)(Block(
          defineFunction2("longDiv") { (x, y) =>
            If(y === bigInt(0), throwDivByZero, {
              Return(wrapBigInt64(x / y))
            })
          },
          defineFunction2("longMod") { (x, y) =>
            If(y === bigInt(0), throwDivByZero, {
              Return(wrapBigInt64(x % y))
            })
          },

          defineFunction1("doubleToLong")(x => Return {
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
          }),

          defineFunction1("longToFloat") { x =>
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
        ))
      )
    }

    private def defineES2015LikeHelpers(): Tree = Block(
      condTree(esVersion < ESVersion.ES2015)(
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
      condTree(arrayErrors != CheckedBehavior.Unchecked)(
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
          if (arrayErrors != CheckedBehavior.Unchecked) {
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

      condTree(esVersion < ESVersion.ES2015)(
        defineFunction5("systemArraycopy") { (src, srcPos, dest, destPos, length) =>
          genCallHelper("arraycopyGeneric", src.u, srcPos, dest.u, destPos, length)
        }
      ),

      condTree(arrayErrors != CheckedBehavior.Unchecked)(Block(
        defineFunction5("systemArraycopyRefs") { (src, srcPos, dest, destPos, length) =>
          If(Apply(genIdentBracketSelect(dest DOT classData, "isAssignableFrom"), List(src DOT classData)), {
            /* Fast-path, no need for array store checks. This always applies
             * for arrays of the same type, and a fortiori, when `src eq dest`.
             */
            genCallHelper("arraycopyGeneric", src.u, srcPos, dest.u, destPos, length)
          }, {
            /* Slow copy with "set" calls for every element. By construction,
             * we have `src ne dest` in this case.
             */
            val srcArray = varRef("srcArray")
            val i = varRef("i")
            Block(
              const(srcArray, src.u),
              genCallHelper("arraycopyCheckBounds",
                  srcArray.length, srcPos, dest.u.length, destPos, length),
              For(let(i, 0), i < length, i := ((i + 1) | 0), {
                Apply(dest DOT "set", List((destPos + i) | 0, BracketSelect(srcArray, (srcPos + i) | 0)))
              })
            )
          })
        },

        defineFunction5("systemArraycopyFull") { (src, srcPos, dest, destPos, length) =>
          val ObjectArray = globalVar("ac", ObjectClass)
          val srcData = varRef("srcData")

          Block(
            const(srcData, src && (src DOT classData)),
            If(srcData === (dest && (dest DOT classData)), {
              // Both values have the same "data" (could also be falsy values)
              If(srcData && genIdentBracketSelect(srcData, "isArrayClass"), {
                // Fast path: the values are array of the same type
                if (esVersion >= ESVersion.ES2015)
                  Apply(src DOT "copyTo", List(srcPos, dest, destPos, length))
                else
                  genCallHelper("systemArraycopy", src, srcPos, dest, destPos, length)
              }, {
                genCallHelper("throwArrayStoreException", Null())
              })
            }, {
              /* src and dest are of different types; the only situation that
               * can still be valid is if they are two reference array types.
               */
              If((src instanceof ObjectArray) && (dest instanceof ObjectArray), {
                genCallHelper("systemArraycopyRefs", src, srcPos, dest, destPos, length)
              }, {
                genCallHelper("throwArrayStoreException", Null())
              })
            })
          )
        }
      )),

      // systemIdentityHashCode
      locally {
        val WeakMapRef = globalRef("WeakMap")

        val lastIDHash = fileLevelVar("lastIDHash")
        val idHashCodeMap = fileLevelVar("idHashCodeMap")

        val obj = varRef("obj")
        val biHash = varRef("biHash")
        val description = varRef("description")
        val hash = varRef("hash")

        def functionSkeleton(defaultImpl: Tree): Function = {
          def genHijackedMethodApply(className: ClassName, arg: Tree): Tree =
            Apply(globalVar("f", (className, hashCodeMethodName)), arg :: Nil)

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

        Block(
          let(lastIDHash, 0),
          const(idHashCodeMap,
              if (esVersion >= ESVersion.ES2015) New(WeakMapRef, Nil)
              else If(typeof(WeakMapRef) !== str("undefined"), New(WeakMapRef, Nil), Null())),
          if (esVersion >= ESVersion.ES2015) {
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
                ((v !== v) || (genCallPolyfillableBuiltin(FroundBuiltin, v) === v)))
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
          MethodDef(static = false, Ident("constructor"), paramList(arg), None, {
            Block(
                if (useClassesForRegularClasses) Apply(Super(), Nil) else Skip(),
                genArrayClassConstructorBody(arg, componentTypeRef)
            )
          })
        }

        val getAndSet = if (arrayErrors != CheckedBehavior.Unchecked) {
          val i = varRef("i")
          val v = varRef("v")

          val boundsCheck = {
            If((i < 0) || (i >= This().u.length),
                genCallHelper("throwArrayIndexOutOfBoundsException", i))
          }

          List(
              MethodDef(static = false, Ident("get"), paramList(i), None, {
                Block(
                    boundsCheck,
                    Return(BracketSelect(This().u, i))
                )
              }),
              MethodDef(static = false, Ident("set"), paramList(i, v), None, {
                Block(
                    boundsCheck,
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
          val methodDef = MethodDef(static = false, Ident("copyTo"),
              paramList(srcPos, dest, destPos, length), None, {
            if (isTypedArray) {
              Block(
                  if (semantics.arrayErrors != CheckedBehavior.Unchecked) {
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

        val clone = MethodDef(static = false, Ident(genName(cloneMethodName)), Nil, None, {
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
                ctor.args, ctor.restParam, ctor.body)),
            (ArrayClass.prototype := New(globalVar("h", ObjectClass), Nil)),
            (ArrayClass.prototype DOT "constructor" := ArrayClass),
            assignES5ClassMembers(ArrayClass, members)
          )

          componentTypeRef match {
            case _: ClassRef =>
              Block(
                clsDef,
                extractWithGlobals(globalFunctionDef("ah", ObjectClass, Nil, None, Skip())),
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
        MethodDef(static = false, Ident("constructor"), Nil, None, {
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
              privateFieldSet("isJSType", bool(false)),

              publicFieldSet("name", str("")),
              publicFieldSet("isPrimitive", bool(false)),
              publicFieldSet("isInterface", bool(false)),
              publicFieldSet("isArrayClass", bool(false)),
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
            paramList(zero, arrayEncodedName, displayName, arrayClass, typedArrayClass), None, {
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
                isJSType, parentData, isInstance), None, {
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
            paramList(componentData, arrayClass, typedArrayClass, isAssignableFromFun), None, {
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

            val set = if (arrayErrors != CheckedBehavior.Unchecked) {
              val i = varRef("i")
              val v = varRef("v")

              val boundsCheck = {
                If((i < 0) || (i >= This().u.length),
                    genCallHelper("throwArrayIndexOutOfBoundsException", i))
              }

              val storeCheck = {
                If((v !== Null()) && !(componentData DOT "isJSType") &&
                    !Apply(genIdentBracketSelect(componentData, "isInstance"), v :: Nil),
                    genCallHelper("throwArrayStoreException", v))
              }

              List(
                MethodDef(static = false, Ident("set"), paramList(i, v), None, {
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
              val methodDef = MethodDef(static = false, Ident("copyTo"),
                  paramList(srcPos, dest, destPos, length), None, {
                genCallHelper("arraycopyGeneric", This().u, srcPos,
                    dest.u, destPos, length)
              })
              methodDef :: Nil
            } else {
              Nil
            }

            val clone = MethodDef(static = false, Ident(genName(cloneMethodName)), Nil, None, {
              Return(New(ArrayClass,
                  Apply(genIdentBracketSelect(This().u, "slice"), Nil) :: Nil))
            })

            val members = set ::: copyTo ::: clone :: Nil

            if (useClassesForRegularClasses) {
              ClassDef(Some(ArrayClass.ident), Some(globalVar("ac", ObjectClass)),
                  ctor :: members)
            } else {
              Block(
                  FunctionDef(ArrayClass.ident, ctor.args, ctor.restParam, ctor.body),
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
        MethodDef(static = false, Ident("getArrayOf"), Nil, None, {
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
        MethodDef(static = false, Ident("getClassOf"), Nil, None, {
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
            paramList(that), None, {
          Return(
              (This() === that) || // fast path
              Apply(This() DOT "isAssignableFromFun", that :: Nil))
        })
      }

      def checkCast = {
        val obj = varRef("obj")
        MethodDef(static = false, StringLiteral("checkCast"), paramList(obj), None,
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
        MethodDef(static = false, StringLiteral("getSuperclass"), Nil, None, {
          Return(If(This() DOT "parentData",
              Apply(This() DOT "parentData" DOT "getClassOf", Nil),
              Null()))
        })
      }

      def getComponentType = {
        MethodDef(static = false, StringLiteral("getComponentType"), Nil, None, {
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
            paramList(lengths), None, {
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
          } else if (arrayErrors != CheckedBehavior.Unchecked) {
            List(
              isAssignableFrom
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

      val forObj = extractWithGlobals(globalFunctionDef("isArrayOf", ObjectClass, paramList(obj, depth), None, {
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
        extractWithGlobals(globalFunctionDef("isArrayOf", primRef, paramList(obj, depth), None, {
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
          extractWithGlobals(globalFunctionDef("asArrayOf", typeRef, paramList(obj, depth), None, {
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
      extractWithGlobals(globalFunctionDef(name, CoreVar, args, None, body))

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
  }
}
