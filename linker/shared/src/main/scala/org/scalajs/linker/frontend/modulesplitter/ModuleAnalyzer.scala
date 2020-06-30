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

import org.scalajs.ir.Names._
import org.scalajs.ir.Trees.JSNativeLoadSpec

import org.scalajs.linker.analyzer.Infos
import org.scalajs.linker.frontend.LinkingUnit
import org.scalajs.linker.interface.ModuleInitializer
import org.scalajs.linker.interface.unstable.ModuleInitializerImpl
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID

private[modulesplitter] final class ModuleAnalyzer(unit: LinkingUnit,
    symbolRequirements: SymbolRequirement) {

  lazy val linkedClassesByName = Map(unit.classDefs.map(c => c.className -> c): _*)

  private lazy val moduleInitializersPerModule =
    unit.moduleInitializers.groupBy(i => new ModuleID(i.module))

  private lazy val exportsPerModule =
    unit.topLevelExports.groupBy(e => new ModuleID(e.tree.moduleID))

  private lazy val classesWithStaticInit =
    unit.classDefs.filter(_.hasStaticInitializer).map(_.className)

  lazy val allModules: List[ModuleID] = (
      moduleInitializersPerModule.keys ++
      exportsPerModule.keys
  ).toList

  def topLevelExports(module: ModuleID): List[LinkedTopLevelExport] =
    exportsPerModule.getOrElse(module, Nil)

  def moduleInitializers(module: ModuleID): Seq[ModuleInitializer.Initializer] = {
    moduleInitializersPerModule.get(module)
      .fold[Seq[ModuleInitializer.Initializer]](Nil)(_.map(_.initializer))
  }

  def staticDependencies(className: ClassName): Set[ClassName] = {
    val deps = Set.newBuilder[ClassName]

    /* We use j.l.Object as representation of the core infrastructure.
     * As such, everything depends on j.l.Object and j.l.Object depends on all
     * symbol requirements.
     */
    if (className == ObjectClass) {
      deps ++= HijackedClasses
      deps ++= symbolRequirementDeps(symbolRequirements)
    } else {
      deps += ObjectClass
    }

    deps ++= linkedClassesByName(className).staticDependencies

    deps.result()
  }

  def staticDependencies(module: ModuleID): Set[ClassName] = {
    val deps = Set.newBuilder[ClassName]

    deps += ObjectClass

    /* All static initializers must be reach by all leaf modules.
     *
     * This is because we have lost the semantic information of *why* the things
     * in the static initializer are supposed to happen. In practical terms, at
     * the time of this writing, the Scala.js compiler emits static initializers
     * for two reasons:
     * 
     * - Initialize fields that are exported to the top level:
     *   Needs to be executed for the class definition itself to be correct.
     * - Register a class with scala.scalajs.reflect.Reflect:
     *   Needs to be executed for reflective instantiation to work.
     *
     * PR #4146 and this comment thread provide additional context:
     * https://github.com/scala-js/scala-js/pull/4111#issuecomment-673590827
     */
    deps ++= classesWithStaticInit

    for (topLevelExport <- topLevelExports(module))
      deps ++= topLevelExport.staticDependencies

    for (moduleInitializer <- moduleInitializers(module)) {
      moduleInitializer match {
        case ModuleInitializerImpl.VoidMainMethod(className, _) =>
          deps += className
        case ModuleInitializerImpl.MainMethodWithArgs(className, _, _) =>
          deps += className
      }
    }

    deps.result()
  }

  def externalDependencies(className: ClassName): Set[String] =
    linkedClassesByName(className).externalDependencies

  def externalDependencies(module: ModuleID): Set[String] =
    topLevelExports(module).flatMap(_.externalDependencies).toSet

  def allExternalDependencies(): Set[String] = {
    def clsIter = unit.classDefs.iterator
    val loadspecs = (
        clsIter.flatMap(_.jsNativeMembers).map(_.jsNativeLoadSpec) ++
        clsIter.filter(_.hasInstances).flatMap(_.jsNativeLoadSpec)
    )

    extractModules(loadspecs)
  }

  private def symbolRequirementDeps(requirement: SymbolRequirement): List[ClassName] = {
    import SymbolRequirement.Nodes._

    requirement match {
      case AccessModule(_, moduleName)        => List(moduleName)
      case InstantiateClass(_, className, _)  => List(className)
      case InstanceTests(_, className)        => List(className)
      case ClassData(_, className)            => List(className)
      case CallStaticMethod(_, className, _)  => List(className)

      case _: CallMethod | NoRequirement =>
        Nil

      case Optional(requirement) =>
        symbolRequirementDeps(requirement)
          .filter(linkedClassesByName.contains(_))

      case Multiple(requirements) =>
        requirements.flatMap(symbolRequirementDeps(_))
    }
  }

  private def extractModules(specs: Iterator[JSNativeLoadSpec]): Set[String] = {
    val builder = Set.newBuilder[String]

    specs.foreach {
      case JSNativeLoadSpec.Import(module, _) =>
        builder += module

      case JSNativeLoadSpec.ImportWithGlobalFallback(
        JSNativeLoadSpec.Import(module, _), _) =>
        builder += module

      case _ =>
    }

    builder.result()
  }
}
