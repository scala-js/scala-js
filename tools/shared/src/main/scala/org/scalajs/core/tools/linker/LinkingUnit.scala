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

package org.scalajs.core.tools.linker

import org.scalajs.core.tools.javascript.ESLevel

import org.scalajs.core.ir
import ir.{Definitions, Infos}

final class LinkingUnit private[linker] (
    val semantics: Semantics,
    val esLevel: ESLevel,
    val classDefs: List[LinkedClass],
    private[linker] val infosInternal: Map[String, Infos.ClassInfo],
    val moduleInitializers: List[ModuleInitializer],
    val isComplete: Boolean
) {

  import LinkingUnit._

  /** Creates a `LinkingUnit` without any entry point. */
  @deprecated(
      "The LinkingUnit constructor was not intended to be exposed to user " +
      "code. It will be removed in 1.0.0.",
      "0.6.15")
  def this(semantics: Semantics, esLevel: ESLevel, classDefs: List[LinkedClass],
      infos: Map[String, Infos.ClassInfo], isComplete: Boolean) = {
    this(semantics, esLevel, classDefs, infos, moduleInitializers = Nil,
        isComplete)
  }

  @deprecated(
      "LinkingUnit.infos was not intended to be exposed to user code. " +
      "It will be removed in 1.0.0. " +
      "Use linkingUnit.classDefs.map(_.toInfo) instead.",
      "0.6.15")
  val infos: Map[String, Infos.ClassInfo] = infosInternal

  /* This used to be a `lazy val`, but that causes scalac to complain about
   * uses of GlobalInfo inside globalInfo. The issue also appears with a `val`
   * but not with a `def`.
   * A `def` is binary compatible with a `lazy val`, although not source
   * compatible (a `lazy val` is *stable*, so can be imported from).
   */
  @deprecated(
      "LinkingUnit.globalInfo was not intended to be exposed to user code. " +
      "It will be removed in 1.0.0. " +
      "Inspect the relevant data in linkingUnit.classDefs instead.",
      "0.6.15")
  def globalInfo: GlobalInfo = {
    classDefs.find(_.encodedName == Definitions.ClassClass).fold {
      GlobalInfo(
          isParentDataAccessed = false)
    } { classClassDef =>
      val methodNames = classClassDef.memberMethods.map(_.info.encodedName).toSet
      GlobalInfo(
          isParentDataAccessed = methodNames.contains("getSuperclass__jl_Class"))
    }
  }

  @deprecated(
      "LinkingUnit.updated was not intended to be exposed to user code. " +
      "It will be removed in 1.0.0.",
      "0.6.15")
  def updated(classDefs: List[LinkedClass], isComplete: Boolean): LinkingUnit =
    updatedInternal(classDefs, isComplete)

  /** Non-deprecated version of `updated` for internal use. */
  private[linker] def updatedInternal(classDefs: List[LinkedClass],
      isComplete: Boolean): LinkingUnit = {
    val newInfos =
      infosInternal ++ classDefs.map(cd => cd.encodedName -> cd.toInfo)
    new LinkingUnit(semantics, esLevel, classDefs, newInfos, moduleInitializers,
        isComplete)
  }
}

@deprecated("This object only contains deprecated members.", "0.6.15")
object LinkingUnit {

  @deprecated("See LinkingInfo.globalInfo.", "0.6.15")
  final class GlobalInfo private (
      /** Whether the parent data of class data is accessed.
       *  This is true iff the java.lang.Class.getSuperclass() method exists,
       *  since it is the only one that can do it.
       */
      val isParentDataAccessed: Boolean
  )

  @deprecated("See LinkingInfo.globalInfo.", "0.6.15")
  object GlobalInfo {
    private[LinkingUnit] def apply(
        isParentDataAccessed: Boolean
    ): GlobalInfo = {
      new GlobalInfo(isParentDataAccessed)
    }
  }

}
