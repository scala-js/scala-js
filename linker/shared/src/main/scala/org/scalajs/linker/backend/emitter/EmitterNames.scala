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

import org.scalajs.ir.Names._
import org.scalajs.ir.Types._
import org.scalajs.ir.WellKnownNames._

private[emitter] object EmitterNames {
  // Class names

  val JavaScriptExceptionClass =
    ClassName("scala.scalajs.js.JavaScriptException")

  val UndefinedBehaviorErrorClass =
    ClassName("org.scalajs.linker.runtime.UndefinedBehaviorError")

  val FloatingPointBitsPolyfillsClass =
    ClassName("org.scalajs.linker.runtime.FloatingPointBitsPolyfills")

  // Field names

  val exceptionFieldName = FieldName(JavaScriptExceptionClass, SimpleFieldName("exception"))

  // Method names

  val AnyArgConstructorName = MethodName.constructor(List(ClassRef(ObjectClass)))
  val IntArgConstructorName = MethodName.constructor(List(IntRef))
  val StringArgConstructorName = MethodName.constructor(List(ClassRef(BoxedStringClass)))
  val ThrowableArgConsructorName = MethodName.constructor(List(ClassRef(ThrowableClass)))

  val cloneMethodName = MethodName("clone", Nil, ClassRef(ObjectClass))
  val getClassMethodName = MethodName("getClass", Nil, ClassRef(ClassClass))
  val hashCodeMethodName = MethodName("hashCode", Nil, IntRef)
  val toStringMethodName = MethodName("toString", Nil, ClassRef(BoxedStringClass))

  val getNameMethodName = MethodName("getName", Nil, ClassRef(BoxedStringClass))
  val getSuperclassMethodName = MethodName("getSuperclass", Nil, ClassRef(ClassClass))

  val floatToBits = MethodName("floatToBits", List(FloatRef), IntRef)
  val floatFromBits = MethodName("floatFromBits", List(IntRef), DoubleRef) // yes, Double
  val doubleToBits = MethodName("doubleToBits", List(DoubleRef), LongRef)
  val doubleFromBits = MethodName("doubleFromBits", List(LongRef), DoubleRef)
}
