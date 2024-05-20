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

import scala.collection
import scala.collection.mutable

import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.standard.ModuleSet.ModuleID

/** Build smallest modules for packages, largest modules for the rest.
 *
 *  For all classes matching any of packages (prefix match), this generates one
 *  module per class, except when there are circular dependencies.
 *
 *  For all other classes, it creates large modules (while avoiding dependency
 *  cycles due to the grouping of the small modules).
 *
 *  All in all, this is essentially the [[SmallModulesAnalyzer]] followed by the
 *  [[FewestModulesAnalyzer]].
 */
private final class SmallModulesForAnalyzer(
    packages: List[ClassName]) extends ModuleAnalyzer {
  def analyze(info: ModuleAnalyzer.DependencyInfo): ModuleAnalyzer.Analysis = {
    val (targetClassToRepr, reprToModuleID) = smallRun(info, packages)

    val modulesToAvoid = info.publicModuleDependencies.keys ++ reprToModuleID.values
    val largeModuleMap =
      new SmallestModulesForTagger(info, excludedClasses = targetClassToRepr.keySet).tagAll(modulesToAvoid)

    new SmallModulesForAnalyzer.Analysis(targetClassToRepr, reprToModuleID, largeModuleMap)
  }

  private def smallRun(info: ModuleAnalyzer.DependencyInfo, packages: List[ClassName]) = {
    val run = new SmallModulesForAnalyzer.SmallRun(info, packages)
    run.analyze()

    // Only return relevant fields for better GC.
    (run.targetClassToRepr, run.reprToModuleID)
  }
}

private object SmallModulesForAnalyzer {

  private final class Analysis(targetClassToRepr: collection.Map[ClassName, ClassName],
      reprToModuleID: collection.Map[ClassName, ModuleID],
      largeModuleMap: collection.Map[ClassName, ModuleID]) extends ModuleAnalyzer.Analysis {
    def moduleForClass(className: ClassName): Option[ModuleID] = {
      largeModuleMap.get(className).orElse {
        targetClassToRepr.get(className).map(reprToModuleID(_))
      }
    }
  }

  private final class SmallRun(info: ModuleAnalyzer.DependencyInfo,
      packages: List[ClassName]) extends StrongConnect(info) {

    private val internalModIDGenerator =
      new InternalModuleIDGenerator.ForClassNames(info.publicModuleDependencies.keys)

    /* We expect this to contain relatively few classes.
     *
     * So instead of keeping the underlying graph and relying on [[moduleIndex]],
     * we create this ad-hoc map.
     */
    val targetClassToRepr = mutable.Map.empty[ClassName, ClassName]

    val reprToModuleID = mutable.Map.empty[ClassName, ModuleID]

    protected def emitModule(moduleIndex: Int, classNames: List[ClassName]): Unit = {
      // Target classes contained in this strongly connected component.
      val targetNames = classNames.filter(clazz => packages.exists(inPackage(clazz, _)))

      if (targetNames.nonEmpty) {
        val repr = internalModIDGenerator.representativeClass(targetNames)
        val id = internalModIDGenerator.forClassName(repr)
        reprToModuleID(repr) = id
        for (className <- classNames)
          targetClassToRepr(className) = repr
      }
    }

    private def inPackage(clazz: ClassName, pkg: ClassName): Boolean = {
      val clazzEnc = clazz.encoded
      val clazzLen = clazzEnc.length
      val pkgEnc = pkg.encoded
      val pkgLen = pkgEnc.length

      if (clazzLen > pkgLen && clazzEnc(pkgLen) == '.') {
        var i = 0
        while (i != pkgLen && clazzEnc(i) == pkgEnc(i))
          i += 1
        i == pkgLen
      } else {
        false
      }
    }
  }
}
