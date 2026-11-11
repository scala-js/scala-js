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

  @Test def testModuleIDOfStaticUnchangedWithNewDirectPathSameModule(): Unit = {
    // Initially the class has one direct path.
    // Ensure that adding a direct path from the same public modules will not change the module ID.

    val moduleAID = ModuleID("modA")

    val publicAClassName = ClassName("publicA")
    val publicBClassName = ClassName("publicB")
    val staticClassName = ClassName("static")

    val classDependencies = ListMap(
      publicAClassName -> new ClassInfo(ListSet(staticClassName), Set.empty),
      publicBClassName -> new ClassInfo(ListSet(staticClassName), Set.empty),
      staticClassName -> new ClassInfo(Set.empty, Set.empty)
    )

    val initialModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicAClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicAClassName, publicBClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialStaticModuleID = initialModuleMap(staticClassName)

    val finalPublicModuleID = finalModuleMap(publicAClassName)
    val finalStaticModuleID = finalModuleMap(staticClassName)

    assertEquals(initialStaticModuleID, finalStaticModuleID)
    assertEquals(finalPublicModuleID, finalStaticModuleID)
  }

  @Test def testModuleIDOfStaticChangesWithNewDirectPathNewModule(): Unit = {
    // Initially the class has one direct path.
    // Ensure that adding a direct paths from a new public module will change the module ID.

    val moduleAID = ModuleID("modA")
    val moduleBID = ModuleID("modB")

    val publicClassName = ClassName("public")
    val staticClassName = ClassName("static")

    val classDependencies = ListMap(
      publicClassName -> new ClassInfo(ListSet(staticClassName), Set.empty),
      staticClassName -> new ClassInfo(Set.empty, Set.empty)
    )

    val initialModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName),
        moduleBID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialStaticModuleID = initialModuleMap(staticClassName)

    val finalStaticModuleID = finalModuleMap(staticClassName)
    val finalPublicModuleID = finalModuleMap(publicClassName)

    assertNotEquals(initialStaticModuleID, finalStaticModuleID)
    assertEquals(finalPublicModuleID, finalStaticModuleID)
  }

  @Test def testModuleIDOfStaticChangesWithNewTransitiveDynamicPathSameModule(): Unit = {
    // Initially the class has one direct path.
    // Ensure that adding a dynamic path from the same public modules will change the module ID.

    val moduleAID = ModuleID("modA")

    val publicClassName = ClassName("public")
    val dynamicClassName = ClassName("dynamic")
    val staticClassName = ClassName("static")

    val initialModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(ListSet(staticClassName), Set.empty),
        staticClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(ListSet(staticClassName), ListSet(dynamicClassName)),
        dynamicClassName -> new ClassInfo(ListSet(staticClassName), Set.empty),
        staticClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialStaticModuleID = initialModuleMap(staticClassName)

    val finalPublicModuleID = finalModuleMap(publicClassName)
    val finalDynamicModuleID = finalModuleMap(dynamicClassName)
    val finalStaticModuleID = finalModuleMap(staticClassName)

    assertNotEquals(initialStaticModuleID, finalStaticModuleID)
    assertNotEquals(finalPublicModuleID, finalStaticModuleID)
    // TODO: Why are there 3 modules and not 2?
    assertNotEquals(finalDynamicModuleID, finalStaticModuleID)
  }

  @Test def testModuleIDOfStaticChangesWithNewDynamicPathSameModule(): Unit = {
    // Initially the class has one direct path.
    // Ensure that adding a dynamic path from the same public modules will change the module ID.

    val moduleAID = ModuleID("modA")

    val publicClassName = ClassName("public")
    val dynamicClassName = ClassName("dynamic")
    val staticClassName = ClassName("static")

    val initialModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(ListSet(staticClassName), Set.empty),
        staticClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(ListSet(staticClassName), ListSet(dynamicClassName)),
        dynamicClassName -> new ClassInfo(Set.empty, ListSet(staticClassName)),
        staticClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialStaticModuleID = initialModuleMap(staticClassName)

    val finalPublicModuleID = finalModuleMap(publicClassName)
    val finalDynamicModuleID = finalModuleMap(dynamicClassName)
    val finalStaticModuleID = finalModuleMap(staticClassName)

    assertNotEquals(initialStaticModuleID, finalStaticModuleID)
    assertNotEquals(finalPublicModuleID, finalStaticModuleID)
    // TODO: Why are there 3 modules and not 2?
    assertNotEquals(finalDynamicModuleID, finalStaticModuleID)
  }

  @Test def testModuleIDOfDynamicChangesWithNewDirectPathSameModule(): Unit = {
    // Initially the class has one dynamic path.
    // Ensure that adding a direct path from the same public modules will change the module ID (the first time only).

    val moduleAID = ModuleID("modA")

    val publicAClassName = ClassName("publicA")
    val publicBClassName = ClassName("publicB")
    val publicCClassName = ClassName("publicC")
    val dynamicClassName = ClassName("dynamic")

    val classDependencies = ListMap(
      publicAClassName -> new ClassInfo(Set.empty, ListSet(dynamicClassName)),
      publicBClassName -> new ClassInfo(ListSet(dynamicClassName), Set.empty),
      publicCClassName -> new ClassInfo(ListSet(dynamicClassName), Set.empty),
      dynamicClassName -> new ClassInfo(Set.empty, Set.empty)
    )

    val initialModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicAClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val midModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicAClassName, publicBClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicAClassName, publicBClassName, publicCClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialDynamicModuleID = initialModuleMap(dynamicClassName)

    val midPublicModuleID = midModuleMap(publicAClassName)
    val midDynamicModuleID = midModuleMap(dynamicClassName)

    val finalDynamicModuleID = finalModuleMap(dynamicClassName)

    assertNotEquals(initialDynamicModuleID, midDynamicModuleID)
    assertEquals(midDynamicModuleID, finalDynamicModuleID)
    assertNotEquals(midPublicModuleID, midDynamicModuleID)
  }

  @Test def testModuleIDOfDynamicChangesWithNewDirectPathNewModule(): Unit = {
    // Initially the class has one dynamic path.
    // Ensure that adding a direct paths from a new public module will change the module ID.

    val moduleAID = ModuleID("modA")
    val moduleBID = ModuleID("modB")

    val publicAClassName = ClassName("publicA")
    val publicBClassName = ClassName("publicB")
    val dynamicClassName = ClassName("dynamic")

    val classDependencies = ListMap(
      publicAClassName -> new ClassInfo(Set.empty, ListSet(dynamicClassName)),
      publicBClassName -> new ClassInfo(ListSet(dynamicClassName), Set.empty),
      dynamicClassName -> new ClassInfo(Set.empty, Set.empty)
    )

    val initialModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicAClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicAClassName),
        moduleBID -> ListSet(publicBClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialDynamicModuleID = initialModuleMap(dynamicClassName)

    val finalDynamicModuleID = finalModuleMap(dynamicClassName)
    val finalPublicModuleID = finalModuleMap(publicAClassName)

    assertNotEquals(initialDynamicModuleID, finalDynamicModuleID)
    assertNotEquals(finalPublicModuleID, finalDynamicModuleID)
  }

  @Test def testModuleIDOfDynamicChangesWithNewTransitiveDynamicPathSameModule(): Unit = {
    // Initially the class has one dynamic path.
    // Ensure that adding a dynamic path from the same public modules will change the module ID.

    val moduleAID = ModuleID("modA")

    val publicClassName = ClassName("public")
    val dynamicAClassName = ClassName("dynamicA")
    val dynamicBClassName = ClassName("dynamicB")

    val initialModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(Set.empty, ListSet(dynamicBClassName)),
        dynamicBClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(Set.empty, ListSet(dynamicAClassName, dynamicBClassName)),
        dynamicAClassName -> new ClassInfo(ListSet(dynamicBClassName), Set.empty),
        dynamicBClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialDynamicBModuleID = initialModuleMap(dynamicBClassName)

    val finalPublicModuleID = finalModuleMap(publicClassName)
    val finalDynamicAModuleID = finalModuleMap(dynamicAClassName)
    val finalDynamicBModuleID = finalModuleMap(dynamicBClassName)

    assertNotEquals(initialDynamicBModuleID, finalDynamicBModuleID)
    assertNotEquals(finalPublicModuleID, finalDynamicBModuleID)
    // TODO: Why are there 3 modules and not 2?
    assertNotEquals(finalDynamicAModuleID, finalDynamicBModuleID)
  }

  @Test def testModuleIDOfDynamicUnchangedWithNewDynamicPathSameModule(): Unit = {
    // Initially the class has one dynamic path.
    // Ensure that adding a dynamic path from the same public modules will not change the module ID.
    // This does not change because a non-transitive dynamic only has itself as the path end.

    val moduleAID = ModuleID("modA")

    val publicClassName = ClassName("public")
    val dynamicAClassName = ClassName("dynamicA")
    val dynamicBClassName = ClassName("dynamicB")

    val initialModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(Set.empty, ListSet(dynamicBClassName)),
        dynamicBClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val finalModuleMap = locally {
      val classDependencies = ListMap(
        publicClassName -> new ClassInfo(Set.empty, ListSet(dynamicAClassName, dynamicBClassName)),
        dynamicAClassName -> new ClassInfo(Set.empty, ListSet(dynamicBClassName)),
        dynamicBClassName -> new ClassInfo(Set.empty, Set.empty)
      )
      val publicModuleDependencies = ListMap(
        moduleAID -> ListSet(publicClassName)
      )
      val info = new DependencyInfo(classDependencies, publicModuleDependencies)
      val tagger = new Tagger(info, Set.empty)
      tagger.tagAll(Iterable.empty)
    }

    val initialDynamicBModuleID = initialModuleMap(dynamicBClassName)

    val finalPublicModuleID = finalModuleMap(publicClassName)
    val finalDynamicAModuleID = finalModuleMap(dynamicAClassName)
    val finalDynamicBModuleID = finalModuleMap(dynamicBClassName)

    assertEquals(initialDynamicBModuleID, finalDynamicBModuleID)
    assertNotEquals(finalPublicModuleID, finalDynamicBModuleID)
    // TODO: Why are there 3 modules and not 2?
    assertNotEquals(finalDynamicAModuleID, finalDynamicBModuleID)
  }

  @Test(timeout = 1000) def testLinkedDynamics(): Unit = {
    val mainModuleID = ModuleID("main")

    val publicClassName = ClassName("public")

    val routeClassNames = (1 to 100).map(n => ClassName(s"route$n")).toSet

    val classDependencies = ListMap(
      publicClassName -> new ClassInfo(Set.empty, routeClassNames)
    ) ++ routeClassNames.map(_ -> new ClassInfo(Set.empty, routeClassNames))

    val publicModuleDependencies = ListMap(mainModuleID -> ListSet(publicClassName))

    val info = new DependencyInfo(classDependencies, publicModuleDependencies)

    val excludedClasses = Set.empty[ClassName]

    val tagger = new Tagger(info, excludedClasses)

    val moduleMap = tagger.tagAll(Iterable.empty)

    val subjectModuleID = moduleMap(publicClassName)
    assertSame(mainModuleID, subjectModuleID)
  }
}
