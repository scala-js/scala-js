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

private[emitter] object EmitterNames {
  val ArithmeticExceptionClass =
    ClassName("java.lang.ArithmeticException")

  val ArrayIndexOutOfBoundsExceptionClass =
    ClassName("java.lang.ArrayIndexOutOfBoundsException")

  val ClassCastExceptionClass =
    ClassName("java.lang.ClassCastException")

  val CloneNotSupportedExceptionClass =
    ClassName("java.lang.CloneNotSupportedException")

  val UndefinedBehaviorErrorClass =
    ClassName("scala.scalajs.runtime.UndefinedBehaviorError")

  /* In theory, some of the following could be computed from the Class
   * Hierarchy. However, that would be require dealing with incremental runs,
   * which would be overkill since these things are in fact known to be static.
   */

  val CharSequenceClass = ClassName("java.lang.CharSequence")
  val CloneableClass = ClassName("java.lang.Cloneable")
  val SerializableClass = ClassName("java.io.Serializable")
  val ComparableClass = ClassName("java.lang.Comparable")
  val NumberClass = ClassName("java.lang.Number")

  val ThrowableClass = ClassName("java.lang.Throwable")

  val NonObjectAncestorsOfStringClass =
    Set(CharSequenceClass, ComparableClass, SerializableClass)
  val NonObjectAncestorsOfBoxedCharacterClass =
    Set(ComparableClass, SerializableClass)
  val NonObjectAncestorsOfHijackedNumberClasses =
    Set(NumberClass, ComparableClass, SerializableClass)
  val NonObjectAncestorsOfBoxedBooleanClass =
    Set(ComparableClass, SerializableClass)

  val AncestorsOfHijackedClasses = Set(
      ObjectClass,
      CharSequenceClass,
      SerializableClass,
      ComparableClass,
      NumberClass
  )

  val HijackedClassesAndTheirSuperClasses =
    HijackedClasses ++ Set(ObjectClass, NumberClass)

  // Method names

  val ObjectArgConstructorName = MethodName.constructor(List(ClassRef(ObjectClass)))
  val StringArgConstructorName = MethodName.constructor(List(ClassRef(BoxedStringClass)))
  val ThrowableArgConsructorName = MethodName.constructor(List(ClassRef(ThrowableClass)))

  val getClassMethodName = MethodName("getClass", Nil, ClassRef(ClassClass))
  val cloneMethodName = MethodName("clone", Nil, ClassRef(ObjectClass))
  val finalizeMethodName = MethodName("finalize", Nil, VoidRef)
  val notifyMethodName = MethodName("notify", Nil, VoidRef)
  val notifyAllMethodName = MethodName("notifyAll", Nil, VoidRef)
  val toStringMethodName = MethodName("toString", Nil, ClassRef(BoxedStringClass))
  val equalsMethodName = MethodName("equals", List(ClassRef(ObjectClass)), BooleanRef)
  val hashCodeMethodName = MethodName("hashCode", Nil, IntRef)
  val compareToMethodName = MethodName("compareTo", List(ClassRef(ObjectClass)), IntRef)
  val lengthMethodName = MethodName("length", Nil, IntRef)
  val charAtMethodName = MethodName("charAt", List(IntRef), CharRef)
  val subSequenceMethodName =
    MethodName("subSequence", List(IntRef, IntRef), ClassRef(ClassName("java.lang.CharSequence")))
  val byteValueMethodName = MethodName("byteValue", Nil, ByteRef)
  val shortValueMethodName = MethodName("shortValue", Nil, ShortRef)
  val intValueMethodName = MethodName("intValue", Nil, IntRef)
  val longValueMethodName = MethodName("longValue", Nil, LongRef)
  val floatValueMethodName = MethodName("floatValue", Nil, FloatRef)
  val doubleValueMethodName = MethodName("doubleValue", Nil, DoubleRef)
  val getSuperclassMethodName = MethodName("getSuperclass", Nil, ClassRef(ClassClass))

}
