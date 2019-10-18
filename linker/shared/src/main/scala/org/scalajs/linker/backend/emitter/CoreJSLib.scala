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

import org.scalajs.ir.ScalaJSVersions
import org.scalajs.ir.Position
import org.scalajs.ir.Definitions._
import org.scalajs.ir.Trees.{JSUnaryOp, JSBinaryOp}
import org.scalajs.ir.Types._

import org.scalajs.linker.interface.{CheckedBehavior, ModuleKind}
import org.scalajs.linker.backend.javascript.Trees._

import EmitterDefinitions._

private[emitter] object CoreJSLib {

  def build(jsGen: JSGen): WithGlobals[Tree] =
    new CoreJSLibBuilder(jsGen).build()

  private class CoreJSLibBuilder(jsGen: JSGen) {
    import jsGen._
    import esFeatures._
    import semantics._
    import TreeDSL._

    implicit private val noPosition: Position = Position.NoPosition

    private val buf = List.newBuilder[Tree]
    private var trackedGlobalRefs = Set.empty[String]

    private def globalRef(name: String): VarRef = {
      // We never access dangerous global refs from the core JS lib
      assert(!GlobalRefUtils.isDangerousGlobalRef(name))
      if (trackAllGlobalRefs)
        trackedGlobalRefs += name
      varRef(name)
    }

    // Unconditional global references
    private val ObjectRef = globalRef("Object")
    private val ArrayRef = globalRef("Array")
    private val StringRef = globalRef("String")
    private val MathRef = globalRef("Math")
    private val TypeErrorRef = globalRef("TypeError")

    // Conditional global references that we often use
    private def BigIntRef = globalRef("BigInt")

    private val classData = Ident("$classData")

    private val orderedPrimitiveCharCodeStrs = {
      val primRefs = List(VoidRef, BooleanRef, CharRef, ByteRef, ShortRef,
          IntRef, LongRef, FloatRef, DoubleRef)
      primRefs.map(fieldNameOfPrimRef(_))
    }

    def build(): WithGlobals[Tree] = {
      defineLinkingInfo()
      defineJSBuiltinsSnapshotsAndPolyfills()
      declareCachedL0()
      definePropertyName()
      defineCharClass()
      defineRuntimeFunctions()
      defineDispatchFunctions()
      defineArithmeticOps()
      defineES2015LikeHelpers()
      defineModuleHelpers()
      defineIntrinsics()
      defineIsPrimitiveFunctions()
      defineBoxFunctions()
      defineTypedArrayConversions()
      defineTypeDataClass()
      defineIsArrayOfPrimitiveFunctions()
      defineAsArrayOfPrimitiveFunctions()
      definePrimitiveTypeDatas()

      WithGlobals(Block(buf.result()), trackedGlobalRefs)
    }

    private def defineLinkingInfo(): Unit = {
      // must be in sync with scala.scalajs.runtime.LinkingInfo

      def objectFreeze(tree: Tree): Tree =
        Apply(genIdentBracketSelect(ObjectRef, "freeze"), tree :: Nil)

      def checkedBehaviorInt(behavior: CheckedBehavior): IntLiteral = {
        int(behavior match {
          case CheckedBehavior.Compliant => 0
          case CheckedBehavior.Fatal     => 1
          case CheckedBehavior.Unchecked => 2
        })
      }

      val linkingInfo = objectFreeze(ObjectConstr(List(
          str("semantics") -> objectFreeze(ObjectConstr(List(
              str("asInstanceOfs") -> checkedBehaviorInt(asInstanceOfs),
              str("arrayIndexOutOfBounds") -> checkedBehaviorInt(arrayIndexOutOfBounds),
              str("moduleInit") -> checkedBehaviorInt(moduleInit),
              str("strictFloats") -> bool(strictFloats),
              str("productionMode") -> bool(productionMode)
          ))),
          str("assumingES6") -> bool(useECMAScript2015),
          str("linkerVersion") -> str(ScalaJSVersions.current),
          str("globalThis") -> This()
      )))

      buf += const(envField("linkingInfo"), linkingInfo)
    }

    private def defineJSBuiltinsSnapshotsAndPolyfills(): Unit = {
      def genPolyfillFor(builtinName: String): Tree = builtinName match {
        case "is" =>
          val x = varRef("x")
          val y = varRef("y")
          Function(arrow = false, paramList(x, y), Return {
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
          Function(arrow = false, paramList(a, b), Block(
              const(ah, a >>> 16),
              const(al, a & 0xffff),
              const(bh, b >>> 16),
              const(bl, b & 0xffff),
              Return((al * bl) + (((ah * bl + al * bh) << 16) >>> 0) | 0)
          ))

        case "fround" =>
          val v = varRef("v")
          if (!strictFloats) {
            Function(arrow = false, paramList(v), Return(+v))
          } else {
            val Float32ArrayRef = globalRef("Float32Array")

            val array = varRef("array")
            val typedArrayPolyfill = Function(arrow = false, paramList(v), {
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

            val noTypedArrayPolyfill = Function(arrow = false, paramList(v), {
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
          Function(arrow = false, paramList(i), Block(
              // See Hacker's Delight, Section 5-3
              If(i === 0, Return(32), Skip()),
              let(r, 1),
              If((i & 0xffff0000) === 0, Block(i := i << 16, r := r + 16), Skip()),
              If((i & 0xff000000) === 0, Block(i := i << 8, r := r + 8), Skip()),
              If((i & 0xf0000000) === 0, Block(i := i << 4, r := r + 4), Skip()),
              If((i & 0xc0000000) === 0, Block(i := i << 2, r := r + 2), Skip()),
              Return(r + (i >> 31))
          ))
      }

      if (!useECMAScript2015) {
        buf += const(envField("is"),
            genIdentBracketSelect(ObjectRef, "is") || genPolyfillFor("is"))
      }

      buf ++= List("imul", "fround", "clz32").map { builtinName =>
        val rhs0 = genIdentBracketSelect(MathRef, builtinName)
        val rhs =
          if (useECMAScript2015) rhs0
          else rhs0 || genPolyfillFor(builtinName)
        const(envField(builtinName), rhs)
      }
    }

    private def declareCachedL0(): Unit = {
      if (!allowBigIntsForLongs)
        buf += genEmptyMutableLet(envFieldIdent("L0"))
    }

    private def definePropertyName(): Unit = {
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
      val obj = varRef("obj")
      val prop = varRef("prop")
      buf += envFunctionDef("propertyName", paramList(obj),
          ForIn(genEmptyImmutableLet(prop.ident), obj, Return(prop))
      )
    }

    private def defineCharClass(): Unit = {
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

      buf += genClassDef(envFieldIdent("Char"), None, List(ctor, toStr))
    }

    private def defineRuntimeFunctions(): Unit = {
      if (asInstanceOfs != CheckedBehavior.Unchecked) {
        // throwClassCastException
        val instance = varRef("instance")
        val classFullName = varRef("classFullName")
        buf += envFunctionDef("throwClassCastException", paramList(instance, classFullName), {
          Throw(maybeWrapInUBE(asInstanceOfs, {
            genScalaClassNew(ClassCastExceptionClass, StringArgConstructorName,
                instance + str(" is not an instance of ") + classFullName)
          }))
        })

        // throwArrayCastException
        val classArrayEncodedName = varRef("classArrayEncodedName")
        val depth = varRef("depth")
        buf += envFunctionDef("throwArrayCastException",
            paramList(instance, classArrayEncodedName, depth), {
          Block(
              While(depth.prefix_--, {
                classArrayEncodedName := (str("[") + classArrayEncodedName)
              }),
              genCallHelper("throwClassCastException", instance, classArrayEncodedName)
          )
        })
      }

      if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
        // throwArrayIndexOutOfBoundsException
        val i = varRef("i")
        val msg = varRef("msg")
        buf += envFunctionDef("throwArrayIndexOutOfBoundsException", paramList(i), {
          Throw(maybeWrapInUBE(arrayIndexOutOfBounds, {
            genScalaClassNew(ArrayIndexOutOfBoundsExceptionClass,
                StringArgConstructorName,
                If(i === Null(), Null(), str("") + i))
          }))
        })
      }

      // noIsInstance
      locally {
        val instance = varRef("instance")
        buf += envFunctionDef("noIsInstance", paramList(instance), {
          Throw(New(TypeErrorRef,
              str("Cannot call isInstance() on a Class representing a JS trait/object") :: Nil))
        })
      }

      locally {
        val arrayClassData = varRef("arrayClassData")
        val nativeArray = varRef("nativeArray")
        val lengths = varRef("lengths")
        val lengthIndex = varRef("lengthIndex")

        // makeNativeArrayWrapper
        buf += envFunctionDef("makeNativeArrayWrapper", paramList(arrayClassData, nativeArray), {
          Return(New(arrayClassData DOT "constr", nativeArray :: Nil))
        })

        // newArrayObject
        buf += envFunctionDef("newArrayObject", paramList(arrayClassData, lengths), {
          Return(genCallHelper("newArrayObjectInternal", arrayClassData, lengths, int(0)))
        })

        // newArrayObjectInternal
        val result = varRef("result")
        val subArrayClassData = varRef("subArrayClassData")
        val subLengthIndex = varRef("subLengthIndex")
        val underlying = varRef("underlying")
        val i = varRef("i")
        buf += envFunctionDef("newArrayObjectInternal",
            paramList(arrayClassData, lengths, lengthIndex), {
          Block(
              const(result, New(arrayClassData DOT "constr",
                  BracketSelect(lengths, lengthIndex) :: Nil)),
              If(lengthIndex < ((lengths DOT "length") - 1), {
                Block(
                    const(subArrayClassData, arrayClassData DOT "componentData"),
                    const(subLengthIndex, lengthIndex + 1),
                    const(underlying, result DOT "u"),
                    For(let(i, 0), i < (underlying DOT "length"), i.++, {
                      BracketSelect(underlying, i) :=
                        genCallHelper("newArrayObjectInternal", subArrayClassData, lengths, subLengthIndex)
                    })
                )
              }, Skip()),
              Return(result)
          )
        })
      }

      // objectGetClass
      locally {
        val instance = varRef("instance")
        val v = varRef("v")
        buf += envFunctionDef("objectGetClass", paramList(instance), {
          Switch(typeof(instance), List(
              str("string") -> {
                Return(genClassOf(BoxedStringClass))
              },
              str("number") -> {
                Block(
                    const(v, instance | 0),
                    If(v === instance, { // is the value integral?
                      If(genCallHelper("isByte", v), {
                        Return(genClassOf(BoxedByteClass))
                      }, {
                        If(genCallHelper("isShort", v), {
                          Return(genClassOf(BoxedShortClass))
                        }, {
                          Return(genClassOf(BoxedIntegerClass))
                        })
                      })
                    }, {
                      if (strictFloats) {
                        If(genCallHelper("isFloat", v), {
                          Return(genClassOf(BoxedFloatClass))
                        }, {
                          Return(genClassOf(BoxedDoubleClass))
                        })
                      } else {
                        Return(genClassOf(BoxedFloatClass))
                      }
                    })
                )
              },
              str("boolean") -> {
                Return(genClassOf(BoxedBooleanClass))
              },
              str("undefined") -> {
                Return(genClassOf(BoxedUnitClass))
              }
          ), {
            If(instance === Null(), {
              Return(Apply(instance DOT getClassMethodName, Nil))
            }, {
              If(genIsInstanceOfHijackedClass(instance, BoxedLongClass), {
                Return(genClassOf(BoxedLongClass))
              }, {
                If(genIsInstanceOfHijackedClass(instance, BoxedCharacterClass), {
                  Return(genClassOf(BoxedCharacterClass))
                }, {
                  If(genIsScalaJSObject(instance), {
                    Return(Apply(instance DOT classData DOT "getClassOf", Nil))
                  }, {
                    Return(Null())
                  })
                })
              })
            })
          })
        })
      }
    }

    private def defineDispatchFunctions(): Unit = {
      val instance = varRef("instance")

      def defineDispatcher(methodName: MethodName, args: List[VarRef],
          body: Tree): Unit = {
        buf += envFunctionDef("dp_" + methodName,
            paramList((instance :: args): _*), body)
      }

      // toString()java.lang.String
      locally {
        defineDispatcher(toStringMethodName, Nil, {
          Return(If(instance === Undefined(),
              str("undefined"),
              Apply(instance DOT "toString", Nil)))
        })
      }

      // getClass()java.lang.Class
      locally {
        defineDispatcher(getClassMethodName, Nil, {
          Return(genCallHelper("objectGetClass", instance))
        })
      }

      // clone()java.lang.Object
      locally {
        defineDispatcher(cloneMethodName, Nil, {
          If(genIsScalaJSObjectOrNull(instance), {
            Return(Apply(instance DOT cloneMethodName, Nil))
          }, {
            Throw(genScalaClassNew(CloneNotSupportedExceptionClass,
                NoArgConstructorName))
          })
        })
      }

      // notify()V and notifyAll()V (final and no-op in Object)
      locally {
        for (name <- List(notifyMethodName, notifyAllMethodName)) {
          defineDispatcher(name, Nil, {
            If(instance === Null(), Apply(instance DOT name, Nil), Skip())
          })
        }
      }

      // finalize()V
      locally {
        defineDispatcher(finalizeMethodName, Nil, {
          If(genIsScalaJSObjectOrNull(instance),
              Apply(instance DOT finalizeMethodName, Nil),
              Skip())
        })
      }

      def defineStandardDispatcher(methodName: MethodName, args: List[VarRef],
          implementationInObject: Option[Tree],
          implementingHijackedClasses: List[ClassName]): Unit = {

        def hijackedClassNameToTypeof(className: ClassName): Option[String] = className match {
          case BoxedStringClass  => Some("string")
          case BoxedDoubleClass  => Some("number")
          case BoxedBooleanClass => Some("boolean")
          case BoxedUnitClass    => Some("undefined")
          case _                 => None
        }

        def genHijackedMethodApply(className: ClassName): Tree = {
          val fullName = className + "__" + methodName
          Apply(envField("f", fullName), (instance :: args): List[VarRef])
        }

        def genBodyNoSwitch(implementingHijackedClasses: List[ClassName]): Tree = {
          val normalCall = Apply(instance DOT methodName, args)
          val defaultCall: Tree = Return(implementationInObject.getOrElse(normalCall))

          val allButNormal = implementingHijackedClasses.foldRight(defaultCall) { (className, next) =>
            If(genIsInstanceOfHijackedClass(instance, className),
                Return(genHijackedMethodApply(className)),
                next)
          }

          if (implementationInObject.isDefined)
            If(genIsScalaJSObjectOrNull(instance), Return(normalCall), allButNormal)
          else
            allButNormal
        }

        def genBodyMaybeSwitch(): Tree = {
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
        }

        defineDispatcher(methodName, args, {
          genBodyMaybeSwitch()
        })
      }

      val rhs = varRef("rhs")

      defineStandardDispatcher(equalsMethodName, varRef("rhs") :: Nil,
          Some(instance === rhs),
          List(BoxedDoubleClass, BoxedLongClass, BoxedCharacterClass))

      defineStandardDispatcher(hashCodeMethodName, Nil,
          Some(genCallHelper("systemIdentityHashCode", instance)),
          List(BoxedStringClass, BoxedDoubleClass, BoxedBooleanClass,
              BoxedUnitClass, BoxedLongClass, BoxedCharacterClass))

      defineStandardDispatcher(compareToMethodName, varRef("rhs") :: Nil,
          None,
          List(BoxedStringClass, BoxedDoubleClass, BoxedBooleanClass,
              BoxedLongClass, BoxedCharacterClass))

      defineStandardDispatcher(lengthMethodName, Nil, None,
          List(BoxedStringClass))

      defineStandardDispatcher(charAtMethodName, varRef("index") :: Nil, None,
          List(BoxedStringClass))

      defineStandardDispatcher(subSequenceMethodName,
          varRef("start") :: varRef("end") :: Nil, None,
          List(BoxedStringClass))

      for {
        methodName <- List(byteValueMethodName, shortValueMethodName,
            intValueMethodName, longValueMethodName, floatValueMethodName,
            doubleValueMethodName)
      } {
        defineStandardDispatcher(methodName, Nil, None,
            List(BoxedDoubleClass, BoxedLongClass))
      }
    }

    private def defineArithmeticOps(): Unit = {
      val x = varRef("x")
      val y = varRef("y")

      val throwDivByZero = {
        Throw(genScalaClassNew(ArithmeticExceptionClass,
            StringArgConstructorName, str("/ by zero")))
      }

      locally {
        buf += envFunctionDef("intDiv", paramList(x, y), {
          If(y === 0, throwDivByZero, {
            Return((x / y) | 0)
          })
        })
        buf += envFunctionDef("intMod", paramList(x, y), {
          If(y === 0, throwDivByZero, {
            Return((x % y) | 0)
          })
        })
      }

      locally {
        buf += envFunctionDef("doubleToInt", paramList(x), {
          Return(If(x > 2147483647, 2147483647, If(x < -2147483648, -2147483648, x | 0)))
        })
      }

      if (allowBigIntsForLongs) {
        def wrapBigInt64(tree: Tree): Tree =
          Apply(genIdentBracketSelect(BigIntRef, "asIntN"), 64 :: tree :: Nil)

        buf += envFunctionDef("longDiv", paramList(x, y), {
          If(y === BigIntLiteral(0), throwDivByZero, {
            Return(wrapBigInt64(x / y))
          })
        })
        buf += envFunctionDef("longMod", paramList(x, y), {
          If(y === BigIntLiteral(0), throwDivByZero, {
            Return(wrapBigInt64(x % y))
          })
        })

        val lo = varRef("lo")
        val rawHi = varRef("rawHi")
        val hi = varRef("hi")
        buf += envFunctionDef("doubleToLong", paramList(x), {
          /* BigInt(x) refuses to work if x is not a "safe integer", i.e., a
           * number with an integral x, whose absolute x is < 2^53. Therefore,
           * we basically use the same algorithm as in RuntimeLong.fromDouble.
           */
          If(x < double(-9223372036854775808.0), { // -2^63
            Return(BigIntLiteral(-9223372036854775808L))
          }, {
            If (x >= double(9223372036854775808.0), { // 2^63
              Return(BigIntLiteral(9223372036854775807L))
            }, {
              Block(
                  const(lo, x | 0),
                  const(rawHi, (x / double(4294967296.0)) | 0), // 2^32
                  const(hi, If((x < 0) && (lo !== 0), (rawHi - 1) | 0, rawHi)),
                  Return(
                      Apply(BigIntRef, hi :: Nil) << BigIntLiteral(32) |
                      Apply(BigIntRef, (lo >>> 0) :: Nil))
              )
            })
          })
        })
      }
    }

    private def defineES2015LikeHelpers(): Unit = {
      // newJSObjectWithVarargs
      locally {
        val ctor = varRef("ctor")
        val args = varRef("args")
        val instance = varRef("instance")
        val result = varRef("result")
        buf += envFunctionDef("newJSObjectWithVarargs", paramList(ctor, args), {
          // This basically emulates the ECMAScript specification for 'new'.
          Block(
              const(instance, Apply(genIdentBracketSelect(ObjectRef, "create"), ctor.prototype :: Nil)),
              const(result, Apply(genIdentBracketSelect(ctor, "apply"), instance :: args :: Nil)),
              Switch(typeof(result),
                  List("string", "number", "boolean", "undefined").map(str(_) -> Skip()) :+
                  str("symbol") -> Return(instance),
                  Return(If(result === Null(), instance, result)))
          )
        })
      }

      // resolveSuperRef
      locally {
        val superClass = varRef("superClass")
        val propName = varRef("propName")
        val getPrototypeOf = varRef("getPrototypeOf")
        val getOwnPropertyDescriptor = varRef("getOwnPropertyDescriptor")
        val superProto = varRef("superProto")
        val desc = varRef("desc")
        buf += envFunctionDef("resolveSuperRef", paramList(superClass, propName), {
          Block(
              const(getPrototypeOf, genIdentBracketSelect(ObjectRef, "getPrototyeOf")),
              const(getOwnPropertyDescriptor, genIdentBracketSelect(ObjectRef, "getOwnPropertyDescriptor")),
              let(superProto, superClass.prototype),
              While(superProto !== Null(), {
                Block(
                    const(desc, Apply(getOwnPropertyDescriptor, superProto :: propName :: Nil)),
                    If(desc !== Undefined(), Return(desc), Skip()),
                    superProto := Apply(getPrototypeOf, superProto :: Nil)
                )
              })
          )
        })
      }

      // superGet
      locally {
        val superClass = varRef("superClass")
        val self = varRef("self")
        val propName = varRef("propName")
        val desc = varRef("desc")
        val getter = varRef("getter")
        buf += envFunctionDef("superGet", paramList(superClass, self, propName), {
          Block(
              const(desc, genCallHelper("resolveSuperRef", superClass, propName)),
              If(desc !== Undefined(), {
                Block(
                    const(getter, genIdentBracketSelect(desc, "get")),
                    Return(If(getter !== Undefined(),
                        Apply(genIdentBracketSelect(getter, "call"), self :: Nil),
                        genIdentBracketSelect(getter, "value")))
                )
              }, {
                Skip()
              })
          )
        })
      }

      // superSet
      locally {
        val superClass = varRef("superClass")
        val self = varRef("self")
        val propName = varRef("propName")
        val value = varRef("value")
        val desc = varRef("desc")
        val setter = varRef("setter")
        buf += envFunctionDef("superSet", paramList(superClass, self, propName, value), {
          Block(
              const(desc, genCallHelper("resolveSuperRef", superClass, propName)),
              If(desc !== Undefined(), {
                Block(
                    const(setter, genIdentBracketSelect(desc, "set")),
                    If(setter !== Undefined(), {
                      Block(
                          Apply(genIdentBracketSelect(setter, "call"), self :: value :: Nil),
                          Return(Undefined())
                      )
                    }, {
                      Skip()
                    })
                )
              }, {
                Skip()
              }),
              Throw(New(TypeErrorRef,
                  List(str("super has no setter '") + propName + str("'."))))
          )
        })
      }
    }

    private def defineModuleHelpers(): Unit = {
      // moduleDefault
      if (moduleKind == ModuleKind.CommonJSModule) {
        val m = varRef("m")
        buf += envFunctionDef("moduleDefault", paramList(m), {
          Return(If(
              m && (typeof(m) === str("object")) && (str("default") in m),
              BracketSelect(m, str("default")),
              m))
        })
      }
    }

    private def defineIntrinsics(): Unit = {
      // systemArraycopy
      locally {
        val src = varRef("src")
        val srcPos = varRef("srcPos")
        val dest = varRef("dest")
        val destPos = varRef("destPos")
        val length = varRef("length")
        val srcu = varRef("srcu")
        val destu = varRef("destu")
        val i = varRef("i")
        buf += envFunctionDef("systemArraycopy", paramList(src, srcPos, dest, destPos, length), {
          Block(
              const(srcu, src DOT "u"),
              const(destu, dest DOT "u"),
              if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
                If((srcPos < 0) || (destPos < 0) || (length < 0) ||
                    (srcPos > (((srcu DOT "length") - length) | 0)) ||
                    (destPos > (((destu DOT "length") - length) | 0)), {
                  genCallHelper("throwArrayIndexOutOfBoundsException", Null())
                }, {
                  Skip()
                })
              } else {
                Skip()
              },
              If((srcu !== destu) || (destPos < srcPos) || (((srcPos + length) | 0) < destPos), {
                For(let(i, 0), i < length, i := ((i + 1) | 0), {
                  BracketSelect(destu, (destPos + i) | 0) := BracketSelect(srcu, (srcPos + i) | 0)
                })
              }, {
                For(let(i, (length - 1) | 0), i >= 0, i := ((i - 1) | 0), {
                  BracketSelect(destu, (destPos + i) | 0) := BracketSelect(srcu, (srcPos + i) | 0)
                })
              })
          )
        })
      }

      // systemIdentityHashCode
      locally {
        val WeakMapRef = globalRef("WeakMap")

        val lastIDHash = envField("lastIDHash")
        val idHashCodeMap = envField("idHashCodeMap")

        buf += let(lastIDHash, 0)
        buf += const(idHashCodeMap,
            if (useECMAScript2015) New(WeakMapRef, Nil)
            else If(typeof(WeakMapRef) !== str("undefined"), New(WeakMapRef, Nil), Null()))

        val obj = varRef("obj")
        val hash = varRef("hash")

        def functionSkeleton(defaultImpl: Tree): Function = {
          Function(arrow = false, paramList(obj), {
            Switch(typeof(obj),
                List("string", "number", "bigint", "boolean").map(str(_) -> Skip()) :+
                str("undefined") -> Return(genCallHelper("dp_" + hashCodeMethodName, obj)),
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

        if (useECMAScript2015) {
          val f = weakMapBasedFunction
          buf += envFunctionDef("systemIdentityHashCode", f.args, f.body)
        } else {
          buf += const(envField("systemIdentityHashCode"),
              If(idHashCodeMap !== Null(), weakMapBasedFunction, fieldBasedFunction))
        }
      }
    }

    private def defineIsPrimitiveFunctions(): Unit = {
      val v = varRef("v")

      def defineIsIntLike(name: String, specificTest: Tree): Unit = {
        buf += envFunctionDef(name, paramList(v), {
          Return((typeof(v) === str("number")) && specificTest &&
              ((int(1) / v) !== (int(1) / double(-0.0))))
        })
      }

      defineIsIntLike("isByte", (v << 24 >> 24) === v)
      defineIsIntLike("isShort", (v << 16 >> 16) === v)
      defineIsIntLike("isInt", (v | 0) === v)

      if (allowBigIntsForLongs) {
        buf += envFunctionDef("isLong", paramList(v), {
          Return((typeof(v) === str("bigint")) &&
              (Apply(genIdentBracketSelect(BigIntRef, "asIntN"), int(64) :: v :: Nil) === v))
        })
      }

      if (strictFloats) {
        buf += envFunctionDef("isFloat", paramList(v), {
          Return((typeof(v) === str("number")) &&
              ((v !== v) || (genCallHelper("fround", v) === v)))
        })
      }
    }

    private def defineBoxFunctions(): Unit = {
      // Boxes for Chars
      locally {
        val c = varRef("c")
        buf += envFunctionDef("bC", paramList(c), {
          Return(New(envField("Char"), c :: Nil))
        })
        buf += const(envField("bC0"), genCallHelper("bC", 0))
      }

      val v = varRef("v")

      if (asInstanceOfs != CheckedBehavior.Unchecked) {
        // Unboxes for everything
        def defineUnbox(name: String, boxedClassName: ClassName, resultExpr: Tree): Unit = {
          val fullName = decodeClassName(boxedClassName)
          buf += envFunctionDef(name, paramList(v), Return {
            If(genIsInstanceOfHijackedClass(v, boxedClassName) || (v === Null()),
                resultExpr,
                genCallHelper("throwClassCastException", v, str(fullName)))
          })
        }

        defineUnbox("uV", BoxedUnitClass, Undefined())
        defineUnbox("uZ", BoxedBooleanClass, !(!v))
        defineUnbox("uC", BoxedCharacterClass, If(v === Null(), 0, v DOT "c"))
        defineUnbox("uB", BoxedByteClass, v | 0)
        defineUnbox("uS", BoxedShortClass, v | 0)
        defineUnbox("uI", BoxedIntegerClass, v | 0)
        defineUnbox("uJ", BoxedLongClass, If(v === Null(), genLongZero(), v))

        /* Since the type test ensures that v is either null or a float, we can
         * use + instead of fround.
         */
        defineUnbox("uF", BoxedFloatClass, +v)

        defineUnbox("uD", BoxedDoubleClass, +v)
        defineUnbox("uT", BoxedStringClass, If(v === Null(), StringLiteral(""), v))
      } else {
        // Unboxes for Chars and Longs
        buf += envFunctionDef("uC", paramList(v), {
          Return(If(v === Null(), 0, v DOT "c"))
        })
        buf += envFunctionDef("uJ", paramList(v), {
          Return(If(v === Null(), genLongZero(), v))
        })
      }
    }

    private def defineTypedArrayConversions(): Unit = {
      val list = List(
          (ByteRef, "byte", "Int8Array"),
          (ShortRef, "short", "Int16Array"),
          (CharRef, "char", "Uint16Array"),
          (IntRef, "int", "Int32Array"),
          (FloatRef, "float", "Float32Array"),
          (DoubleRef, "double", "Float64Array")
      )

      val value = varRef("value")

      for ((primRef, shortName, typedArrayName) <- list) {
        val typedArrayClass = globalRef(typedArrayName)
        val shortNameUpperCase = "" + shortName.head.toUpper + shortName.tail

        buf += envFunctionDef(shortName + "Array2TypedArray", paramList(value), {
          Return(New(typedArrayClass, (value DOT "u") :: Nil))
        })
        buf += envFunctionDef("typedArray2" + shortNameUpperCase + "Array", paramList(value), {
          Return(New(genClassDataOf(ArrayTypeRef(primRef, 1)) DOT "constr",
              New(typedArrayClass, value :: Nil) :: Nil))
        })
      }
    }

    private def defineTypeDataClass(): Unit = {
      def privateFieldSet(fieldName: String, value: Tree): Tree =
        This() DOT fieldName := value

      def publicFieldSet(fieldName: String, value: Tree): Tree =
        genIdentBracketSelect(This(), fieldName) := value

      val ctor = {
        MethodDef(static = false, Ident("constructor"), Nil, {
          Block(
              privateFieldSet("constr", Undefined()),
              privateFieldSet("parentData", Undefined()),
              privateFieldSet("ancestors", Null()),
              privateFieldSet("componentData", Null()),
              privateFieldSet("arrayBase", Null()),
              privateFieldSet("arrayDepth", int(0)),
              privateFieldSet("zero", Null()),
              privateFieldSet("arrayEncodedName", str("")),
              privateFieldSet("_classOf", Undefined()),
              privateFieldSet("_arrayOf", Undefined()),
              privateFieldSet("isArrayOf", Undefined()),
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
        val isArrayOf = varRef("isArrayOf")
        val obj = varRef("obj")
        val depth = varRef("depth")
        MethodDef(static = false, Ident("initPrim"),
            paramList(zero, arrayEncodedName, displayName, isArrayOf), {
          Block(
              privateFieldSet("ancestors", ObjectConstr(Nil)),
              privateFieldSet("zero", zero),
              privateFieldSet("arrayEncodedName", arrayEncodedName),
              privateFieldSet("isArrayOf", isArrayOf),
              publicFieldSet("name", displayName),
              publicFieldSet("isPrimitive", bool(true)),
              publicFieldSet("isInstance",
                  Function(arrow = false, paramList(obj), Return(bool(false)))),
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
        val isArrayOf = varRef("isArrayOf")
        val internalName = varRef("internalName")
        val obj = varRef("obj")
        val depth = varRef("depth")
        MethodDef(static = false, Ident("initClass"),
            paramList(internalNameObj, isInterface, fullName, ancestors,
                isJSType, parentData, isInstance, isArrayOf), {
          Block(
              const(internalName, genCallHelper("propertyName", internalNameObj)),
              privateFieldSet("parentData", parentData),
              privateFieldSet("ancestors", ancestors),
              privateFieldSet("arrayEncodedName", str("L") + fullName + str(";")),
              privateFieldSet("isArrayOf", isArrayOf || {
                Function(arrow = false, paramList(obj, depth), {
                  Return(!(!(obj && (obj DOT classData) &&
                      ((obj DOT classData DOT "arrayDepth") === depth) &&
                      BracketSelect(obj DOT classData DOT "arrayBase" DOT "ancestors", internalName))))
                })
              }),
              publicFieldSet("name", fullName),
              publicFieldSet("isInterface", isInterface),
              publicFieldSet("isJSType", !(!isJSType)),
              publicFieldSet("isInstance", isInstance || {
                Function(arrow = false, paramList(obj), {
                  Return(!(!(obj && (obj DOT classData) &&
                      BracketSelect(obj DOT classData DOT "ancestors", internalName))))
                })
              }),
              Return(This())
          )
        })
      }

      val initArray = {
        val componentData = varRef("componentData")
        val componentZero = varRef("componentZero")
        val ArrayClass = varRef("ArrayClass")
        val encodedName = varRef("encodedName")
        val componentBase = varRef("componentBase")
        val arrayDepth = varRef("arrayDepth")
        val obj = varRef("obj")
        MethodDef(static = false, Ident("initArray"),
            paramList(componentData), {
          val ArrayClassDef = {
            val ctor = {
              val arg = varRef("arg")
              val i = varRef("i")
              MethodDef(static = false, Ident("constructor"),
                  paramList(arg), {
                Block(
                    Apply(Super(), Nil),
                    If(typeof(arg) === str("number"), {
                      // arg is the length of the array
                      Block(
                          privateFieldSet("u", New(ArrayRef, arg :: Nil)),
                          For(let(i, 0), i < arg, i.++, {
                            BracketSelect(This() DOT "u", i) := componentZero
                          })
                      )
                    }, {
                      // arg is a native array that we wrap
                      privateFieldSet("u", arg)
                    })
                )
              })
            }

            val getAndSet = if (arrayIndexOutOfBounds != CheckedBehavior.Unchecked) {
              val i = varRef("i")
              val v = varRef("v")

              val boundsCheck = {
                If((i < 0) || (i >= (This() DOT "u" DOT "length")),
                    genCallHelper("throwArrayIndexOutOfBoundsException", i),
                    Skip())
              }

              List(
                  MethodDef(static = false, Ident("get"), paramList(i), {
                    Block(
                        boundsCheck,
                        Return(BracketSelect(This() DOT "u", i))
                    )
                  }),
                  MethodDef(static = false, Ident("set"), paramList(i, v), {
                    Block(
                        boundsCheck,
                        BracketSelect(This() DOT "u", i) := v
                    )
                  })
              )
            } else {
              Nil
            }

            val clone = MethodDef(static = false, Ident(cloneMethodName), Nil, {
              Return(New(ArrayClass, {
                If((This() DOT "u") instanceof ArrayRef, {
                  Apply(genIdentBracketSelect(This() DOT "u", "slice"), 0 :: Nil)
                }, {
                  New(This() DOT "u" DOT "constructor", (This() DOT "u") :: Nil)
                })
              } :: Nil))
            })

            genClassDef(ArrayClass.ident,
                Some((encodeClassVar(ObjectClass), envField("h", ObjectClass))),
                ctor :: getAndSet ::: clone :: Nil)
          }

          Block(
              const(componentZero, if (allowBigIntsForLongs) {
                (componentData DOT "zero")
              } else {
                If((componentData DOT "zero") === str("longZero"),
                    genLongZero(), componentData DOT "zero")
              }),
              ArrayClassDef,
              ArrayClass.prototype DOT classData := This(),
              const(encodedName, str("[") + (componentData DOT "arrayEncodedName")),
              const(componentBase, (componentData DOT "arrayBase") || componentData),
              const(arrayDepth, (componentData DOT "arrayDepth") + 1),
              privateFieldSet("constr", ArrayClass),
              privateFieldSet("parentData", genClassDataOf(ObjectClass)),
              privateFieldSet("ancestors", ObjectConstr(List(
                  Ident(ObjectClass) -> 1,
                  Ident(CloneableClass) -> 1,
                  Ident(SerializableClass) -> 1
              ))),
              privateFieldSet("componentData", componentData),
              privateFieldSet("arrayBase", componentBase),
              privateFieldSet("arrayDepth", arrayDepth),
              privateFieldSet("arrayEncodedName", encodedName),
              publicFieldSet("name", encodedName),
              publicFieldSet("isArrayClass", bool(true)),
              publicFieldSet("isInstance", Function(arrow = false, paramList(obj), {
                Return(Apply(componentBase DOT "isArrayOf", obj :: arrayDepth :: Nil))
              })),
              Return(This())
          )
        })
      }

      val getClassOf = {
        MethodDef(static = false, Ident("getClassOf"), Nil, {
          Block(
              If(!(This() DOT "_classOf"),
                  This() DOT "_classOf" := New(encodeClassVar(ClassClass), This() :: Nil),
                  Skip()),
              Return(This() DOT "_classOf")
          )
        })
      }

      val getArrayOf = {
        MethodDef(static = false, Ident("getArrayOf"), Nil, {
          Block(
              If(!(This() DOT "_arrayOf"),
                  This() DOT "_arrayOf" :=
                    Apply(New(envField("TypeData"), Nil) DOT "initArray", This() :: Nil),
                  Skip()),
              Return(This() DOT "_arrayOf")
          )
        })
      }

      val isAssignableFrom = {
        val that = varRef("that")
        val thatFakeInstance = varRef("thatFakeInstance")
        MethodDef(static = false, StringLiteral("isAssignableFrom"),
            paramList(that), {
          If(genIdentBracketSelect(This(), "isPrimitive") ||
              genIdentBracketSelect(that, "isPrimitive"), {
            Return(This() === that)
          }, {
            Block(
                genEmptyMutableLet(thatFakeInstance.ident),
                If(that === genClassDataOf(BoxedStringClass), {
                  thatFakeInstance := str("")
                }, {
                  If(that === genClassDataOf(BoxedBooleanClass), {
                    thatFakeInstance := bool(false)
                  }, {
                    If({
                      List(BoxedByteClass, BoxedShortClass, BoxedIntegerClass,
                          BoxedFloatClass, BoxedDoubleClass).map { cls =>
                        that === genClassDataOf(cls)
                      }.reduceLeft(_ || _)
                    }, {
                      thatFakeInstance := int(0)
                    }, {
                      If(that === genClassOf(BoxedLongClass), {
                        thatFakeInstance := genLongZero()
                      }, {
                        If(that === genClassOf(BoxedUnitClass), {
                          thatFakeInstance := Undefined()
                        }, {
                          thatFakeInstance := ObjectConstr(List(classData -> that))
                        })
                      })
                    })
                  })
                }),
                Return(Apply(genIdentBracketSelect(This(), "isInstance"),
                    thatFakeInstance :: Nil))
            )
          })
        })
      }

      val getSuperclass = {
        MethodDef(static = false, StringLiteral("getSuperclass"), Nil, {
          Return(If(This() DOT "parentData",
              Apply(This() DOT "parentData" DOT "getClassOf", Nil),
              Null()))
        })
      }

      val getComponentType = {
        MethodDef(static = false, StringLiteral("getComponentType"), Nil, {
          Return(If(This() DOT "componentData",
              Apply(This() DOT "componentData" DOT "getClassOf", Nil),
              Null()))
        })
      }

      val newArrayOfThisClass = {
        val lengths = varRef("lengths")
        val arrayClassData = varRef("arrayClassData")
        val i = varRef("i")
        MethodDef(static = false, StringLiteral("newArrayOfThisClass"),
            paramList(lengths), {
          Block(
              let(arrayClassData, This()),
              For(let(i, 0), i < (lengths DOT "length"), i.++, {
                arrayClassData := Apply(arrayClassData DOT "getArrayOf", Nil)
              }),
              Return(genCallHelper("newArrayObject", arrayClassData, lengths))
          )
        })
      }

      buf += genClassDef(envFieldIdent("TypeData"), None, List(
          ctor,
          initPrim,
          initClass,
          initArray,
          getClassOf,
          getArrayOf,
          isAssignableFrom,
          getSuperclass,
          getComponentType,
          newArrayOfThisClass
      ))
    }

    private def defineIsArrayOfPrimitiveFunctions(): Unit = {
      for (charCodeStr <- orderedPrimitiveCharCodeStrs) {
        val obj = varRef("obj")
        val depth = varRef("depth")
        buf += FunctionDef(envFieldIdent("isArrayOf", charCodeStr), paramList(obj, depth), {
          Return(!(!(obj && (obj DOT classData) &&
              ((obj DOT classData DOT "arrayDepth") === depth) &&
              ((obj DOT classData DOT "arrayBase") === genClassDataOf(charCodeStr)))))
        })
      }
    }

    private def defineAsArrayOfPrimitiveFunctions(): Unit = {
      if (asInstanceOfs != CheckedBehavior.Unchecked) {
        for (charCodeStr <- orderedPrimitiveCharCodeStrs) {
          val obj = varRef("obj")
          val depth = varRef("depth")
          buf += FunctionDef(envFieldIdent("asArrayOf", charCodeStr), paramList(obj, depth), {
            If(genCallHelper("isArrayOf_" + charCodeStr, obj, depth) || (obj === Null()), {
              Return(obj)
            }, {
              genCallHelper("throwArrayCastException", obj, str(charCodeStr), depth)
            })
          })
        }
      }
    }

    private def definePrimitiveTypeDatas(): Unit = {
      for {
        (primRef, zero, displayName) <- List(
            (VoidRef, Undefined(), "void"),
            (BooleanRef, bool(false), "boolean"),
            (CharRef, int(0), "char"),
            (ByteRef, int(0), "byte"),
            (ShortRef, int(0), "short"),
            (IntRef, int(0), "int"),
            (LongRef, if (allowBigIntsForLongs) genLongZero() else str("longZero"), "long"),
            (FloatRef, double(0), "float"),
            (DoubleRef, double(0), "double")
        )
      } {
        val charCodeStr = fieldNameOfPrimRef(primRef)
        buf += const(envField("d", charCodeStr), {
          Apply(New(envField("TypeData"), Nil) DOT "initPrim",
              List(zero, str(charCodeStr), str(displayName), envField("isArrayOf", charCodeStr)))
        })
      }
    }

    private def maybeWrapInUBE(behavior: CheckedBehavior, exception: Tree): Tree = {
      if (behavior == CheckedBehavior.Fatal) {
        genScalaClassNew(UndefinedBehaviorErrorClass,
            ThrowableArgConsructorName, exception)
      } else {
        exception
      }
    }

    private def genScalaClassNew(className: ClassName, ctorName: MethodName,
        args: Tree*): Tree = {
      Apply(envField("ct", className + "__" + ctorName),
          New(encodeClassVar(className), Nil) :: args.toList)
    }

    private def genIsScalaJSObject(obj: VarRef): Tree =
      !(!(obj && (obj DOT classData)))

    private def genIsScalaJSObjectOrNull(obj: VarRef): Tree =
      genIsScalaJSObject(obj) || (obj === Null())

    private def envFunctionDef(name: String, args: List[ParamDef],
        body: Tree): FunctionDef = {
      FunctionDef(envFieldIdent(name), args, body)
    }

    private def genClassDef(className: Ident, parent: Option[(Tree, Tree)],
        members: List[Tree]): Tree = {
      if (useECMAScript2015) {
        ClassDef(Some(className), parent.map(_._1), members)
      } else {
        val classRef = VarRef(className)

        val ctor = members.collectFirst {
          case m @ MethodDef(false, Ident("constructor", _), _, _) => m
        }.getOrElse {
          MethodDef(false, Ident("constructor"), Nil, Skip())
        }

        val patchedCtorBody = {
          def patchSuper(stat: Tree): Tree = stat match {
            case Apply(Super(), args) =>
              /* All the super constructor calls we have in the core JS lib are
               * actually no-op, so we just omit them.
               */
              assert(args.isEmpty)
              Skip()
            case _ =>
              stat
          }

          ctor.body match {
            case Block(stats) => Block(stats.map(patchSuper(_)))
            case stat         => patchSuper(stat)
          }
        }

        val ctorFun = FunctionDef(className, ctor.args, patchedCtorBody)

        val prototype = classRef.prototype

        val inheritProto = parent.fold[Tree] {
          Skip()
        } { parent =>
          val inheritableCtor = parent._2
          Block(
              prototype := New(inheritableCtor, Nil),
              (prototype DOT "constructor") := classRef
          )
        }

        val setMembers = for (member <- members) yield {
          (member: @unchecked) match {
            case MethodDef(false, Ident("constructor", _), _, _) =>
              Skip()

            case MethodDef(static, name, args, body) =>
              val target = if (static) classRef else prototype
              genPropSelect(target, name) := Function(arrow = false, args, body)
          }
        }

        Block(ctorFun :: inheritProto :: setMembers)
      }
    }

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
