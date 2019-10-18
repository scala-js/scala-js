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

import org.scalajs.ir.Definitions._

private[emitter] object EmitterDefinitions {
  val ArithmeticExceptionClass =
    ClassName("jl_ArithmeticException")

  val ArrayIndexOutOfBoundsExceptionClass =
    ClassName("jl_ArrayIndexOutOfBoundsException")

  val ClassCastExceptionClass =
    ClassName("jl_ClassCastException")

  val CloneNotSupportedExceptionClass =
    ClassName("jl_CloneNotSupportedException")

  val UndefinedBehaviorErrorClass =
    ClassName("sjsr_UndefinedBehaviorError")

  /* In theory, some of the following could be computed from the Class
   * Hierarchy. However, that would be require dealing with incremental runs,
   * which would be overkill since these things are in fact known to be static.
   */

  val CharSequenceClass = ClassName("jl_CharSequence")
  val CloneableClass = ClassName("jl_Cloneable")
  val SerializableClass = ClassName("Ljava_io_Serializable")
  val ComparableClass = ClassName("jl_Comparable")
  val NumberClass = ClassName("jl_Number")

  val ThrowableClass = ClassName("jl_Throwable")

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

  val ObjectArgConstructorName = MethodName("init___O")
  val StringArgConstructorName = MethodName("init___T")
  val ThrowableArgConsructorName = MethodName("init___jl_Throwable")

  val getClassMethodName = MethodName("getClass__jl_Class")
  val cloneMethodName = MethodName("clone__O")
  val finalizeMethodName = MethodName("finalize__V")
  val notifyMethodName = MethodName("notify__V")
  val notifyAllMethodName = MethodName("notifyAll__V")
  val toStringMethodName = MethodName("toString__T")
  val equalsMethodName = MethodName("equals__O__Z")
  val hashCodeMethodName = MethodName("hashCode__I")
  val compareToMethodName = MethodName("compareTo__O__I")
  val lengthMethodName = MethodName("length__I")
  val charAtMethodName = MethodName("charAt__I__C")
  val subSequenceMethodName = MethodName("subSequence__I__I__jl_CharSequence")
  val byteValueMethodName = MethodName("byteValue__B")
  val shortValueMethodName = MethodName("shortValue__S")
  val intValueMethodName = MethodName("intValue__I")
  val longValueMethodName = MethodName("longValue__J")
  val floatValueMethodName = MethodName("floatValue__F")
  val doubleValueMethodName = MethodName("doubleValue__D")
  val getSuperclassMethodName = MethodName("getSuperclass__jl_Class")

}
