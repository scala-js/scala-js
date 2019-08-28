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
  /* In theory, some of the following could be computed from the Class
   * Hierarchy. However, that would be require dealing with incremental runs,
   * which would be overkill since these things are in fact known to be static.
   */

  final val CharSequenceClass = "jl_CharSequence"
  final val SerializableClass = "Ljava_io_Serializable"
  final val ComparableClass = "jl_Comparable"
  final val NumberClass = "jl_Number"

  final val ThrowableClass = "jl_Throwable"

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

}
