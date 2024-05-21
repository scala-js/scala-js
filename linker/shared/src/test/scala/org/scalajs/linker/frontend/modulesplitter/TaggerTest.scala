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

import org.junit.Assert._
import org.junit.Test
import org.scalajs.ir.Names.ClassName
import org.scalajs.linker.frontend.modulesplitter.ModuleAnalyzer.{ClassInfo, DependencyInfo}
import org.scalajs.linker.standard.ModuleSet.ModuleID

import scala.collection.immutable.{ListMap, ListSet}

class TaggerTest {

  @Test def testLinkedDynamics(): Unit = {
    val mainModuleID = ModuleID("main")

    val publicClassName = ClassName("public")

    val routeClassNames = (1 to 100).map(n => ClassName(s"route$n")).toSet

    val classDependencies = ListMap(
      publicClassName -> new ClassInfo(Set.empty, routeClassNames)
    ) ++ routeClassNames.map(className => className -> new ClassInfo(Set.empty, routeClassNames - className))

    val publicModuleDependencies = ListMap(mainModuleID -> ListSet(publicClassName))

    val info = new DependencyInfo(classDependencies, publicModuleDependencies)

    val excludedClasses = Set.empty[ClassName]

    val tagger = new Tagger(info, excludedClasses)

    val moduleMap = tagger.tagAll(Iterable.empty)

    val subjectModuleID = moduleMap(publicClassName)
    assertSame(mainModuleID, subjectModuleID)
  }

  @Test def testMaxExcludedHopCount(): Unit = {
    val mainModuleID = ModuleID("main")

    val publicClassName = ClassName("public")
    val forkClassName = ClassName("fork")
    val excludedClassName = ClassName("excluded")
    val nonExcludedClassName = ClassName("non-excluded")
    val subjectClassName = ClassName("subject")

    val classDependencies = ListMap(
      publicClassName -> new ClassInfo(ListSet(forkClassName), Set.empty),
      forkClassName -> new ClassInfo(ListSet(nonExcludedClassName, excludedClassName), Set.empty),
      excludedClassName -> new ClassInfo(ListSet(nonExcludedClassName), Set.empty),
      nonExcludedClassName -> new ClassInfo(ListSet(subjectClassName), Set.empty),
      subjectClassName -> new ClassInfo(ListSet.empty, Set.empty)
    )

    val publicModuleDependencies = ListMap(mainModuleID -> ListSet(publicClassName))

    val info = new DependencyInfo(classDependencies, publicModuleDependencies)

    val excludedClasses = Set(excludedClassName)

    val tagger = new Tagger(info, excludedClasses)

    val moduleMap = tagger.tagAll(Iterable.empty)

    val subjectModuleID = moduleMap(subjectClassName)
    assertSame(mainModuleID, subjectModuleID)
  }
}
