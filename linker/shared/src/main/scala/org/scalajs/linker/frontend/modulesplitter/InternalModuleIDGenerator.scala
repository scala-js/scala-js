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

package org.scalajs.linker.frontend.modulesplitter

import org.scalajs.ir.Names.{ClassName, ObjectClass}
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Generators for internal module IDs. */
private[modulesplitter] object InternalModuleIDGenerator {

  /** Generator based on `ClassName`s. */
  final class ForClassNames(avoid: Iterable[ModuleID]) {
    private val avoidSet: Set[ModuleID] = avoid.toSet

    /** Picks a representative from a list of classes.
     *
     *  Guarantees to return the same value independent of the order of [[names]].
     */
    def representativeClass(names: List[ClassName]): ClassName = {
      require(names.nonEmpty)

      /* Take the lexicographically smallest name as a stable name of the
       * module, with the exception of j.l.Object which identifies the root
       * module.
       *
       * We do this, because it is simple and stable (i.e. does not depend
       * on traversal order).
       */
      if (names.contains(ObjectClass)) ObjectClass
      else names.min
    }

    /** Builds an ID for the class with name [[name]].
     *
     *  The result is guaranteed to be:
     *  - Different from any public module ID.
     *  - Different for each ClassName.
     *  - Deterministic.
     */
    def forClassName(name: ClassName): ModuleID = {
      /* Build a module ID that doesn't collide with others.
       *
       * We observe:
       * - Class names are unique, so they never collide with each other.
       * - Appending a dot ('.') to a class name results in an illegal class name.
       *
       * So we append dots until we hit a ModuleID not used by a public module.
       *
       * Note that this is stable, because it does not depend on the order we
       * iterate over nodes.
       */
      var moduleID = ModuleID(name.nameString)
      while (avoidSet.contains(moduleID))
        moduleID = ModuleID(moduleID.id + ".")
      moduleID
    }
  }

  /** Generator based on digests. */
  final class ForDigests private (internalModuleIDPrefix: String) {
    def this(avoid: Iterable[ModuleID]) =
      this(freeInternalPrefix(avoid))

    def forDigest(digest: Array[Byte]): ModuleID = {
      @inline def hexDigit(digit: Int): Char =
        Character.forDigit(digit & 0x0f, 16)

      val id = new java.lang.StringBuilder(internalModuleIDPrefix)

      for (b <- digest) {
        id.append(hexDigit(b >> 4))
        id.append(hexDigit(b))
      }

      ModuleID(id.toString())
    }
  }

  /** Creates a prefix that is not a prefix of any of the IDs in [[avoid]] */
  private def freeInternalPrefix(avoid: Iterable[ModuleID]): String = {
    Iterator
      .iterate("internal-")(_ + "-")
      .find(p => !avoid.exists(_.id.startsWith(p)))
      .get
  }
}
