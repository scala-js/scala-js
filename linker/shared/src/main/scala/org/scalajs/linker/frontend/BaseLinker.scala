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

package org.scalajs.linker.frontend

import scala.concurrent._

import org.scalajs.logging._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.standard.ModuleSet.ModuleID
import org.scalajs.linker.checker._
import org.scalajs.linker.analyzer._

import org.scalajs.ir
import org.scalajs.ir.Names.ClassName
import org.scalajs.ir.Trees.{ClassDef, MethodDef}
import org.scalajs.ir.Version

import Analysis._

/** Links the information from [[interface.IRFile IRFile]]s into
 *  [[standard.LinkedClass LinkedClass]]es. Does a dead code elimination pass.
 */
final class BaseLinker(config: CommonPhaseConfig, checkIR: Boolean) {
  import BaseLinker._

  private val irLoader = new FileIRLoader
  private val analyzer =
    new Analyzer(config, initial = true, checkIR = checkIR, failOnError = true, irLoader)
  private val desugarTransformer = new DesugarTransformer(config.coreSpec)
  private val methodSynthesizer = new MethodSynthesizer(irLoader)

  def link(irInput: Seq[IRFile],
      moduleInitializers: Seq[ModuleInitializer], logger: Logger,
      symbolRequirements: SymbolRequirement)(
      implicit ec: ExecutionContext): Future[LinkingUnit] = {

    val result = for {
      _ <- irLoader.update(irInput)
      analysis <- logger.timeFuture("Linker: Compute reachability") {
        analyzer.computeReachability(moduleInitializers, symbolRequirements, logger)
      }
      linkResult <- logger.timeFuture("Linker: Assemble LinkedClasses") {
        assemble(moduleInitializers, analysis)
      }
    } yield {
      if (checkIR) {
        logger.time("Linker: Check IR") {
          val errorCount = IRChecker.check(linkResult, logger)
          if (errorCount != 0) {
            throw new LinkingException(
                s"There were $errorCount IR checking errors.")
          }
        }
      }

      linkResult
    }

    result.andThen { case _ =>
      irLoader.cleanAfterRun()
    }
  }

  private def assemble(moduleInitializers: Seq[ModuleInitializer],
      analysis: Analysis)(implicit ec: ExecutionContext): Future[LinkingUnit] = {
    def assembleClass(info: ClassInfo) = {
      val version = irLoader.irFileVersion(info.className)
      val syntheticMethods = methodSynthesizer.synthesizeMembers(info, analysis)

      for {
        classDef <- irLoader.loadClassDef(info.className)
        syntheticMethods <- syntheticMethods
      } yield {
        BaseLinker.linkClassDef(classDef, version, syntheticMethods, analysis,
            Some(desugarTransformer))
      }
    }

    for {
      assembled <- Future.traverse(analysis.classInfos.values)(assembleClass)
    } yield {
      val (linkedClassDefs, linkedTopLevelExports) = assembled.unzip

      val globalInfo = new LinkedGlobalInfo(
        analysis.isClassSuperClassUsed
      )

      new LinkingUnit(linkedClassDefs.toList,
          linkedTopLevelExports.flatten.toList,
          moduleInitializers.toList,
          globalInfo)
    }
  }
}

private[frontend] object BaseLinker {

  private final class DesugarTransformer(coreSpec: CoreSpec)
      extends ir.Transformers.ClassTransformer {

    import ir.Trees._

    override def transform(tree: Tree): Tree = {
      tree match {
        case prop: LinkTimeProperty =>
          coreSpec.linkTimeProperties.transformLinkTimeProperty(prop)

        case LinkTimeIf(cond, thenp, elsep) =>
          val cond1 = transform(cond)
          coreSpec.linkTimeProperties.tryEvalLinkTimeBooleanExpr(cond1) match {
            case Some(result) =>
              if (result)
                transform(thenp)
              else
                transform(elsep)
            case None =>
              throw new AssertionError(
                  s"Invalid link-time condition should not have passed the reachability analysis: " +
                  tree.show)
          }

        case _ =>
          super.transform(tree)
      }
    }

    /* Transfer Version from old members to transformed members.
     * We can do this because the transformation only depends on the
     * `coreSpec`, which is immutable.
     */

    override def transformMethodDef(methodDef: MethodDef): MethodDef = {
      val newMethodDef = super.transformMethodDef(methodDef)
      newMethodDef.copy()(newMethodDef.optimizerHints, methodDef.version)(newMethodDef.pos)
    }

    override def transformJSConstructorDef(jsConstructor: JSConstructorDef): JSConstructorDef = {
      val newJSConstructor = super.transformJSConstructorDef(jsConstructor)
      newJSConstructor.copy()(newJSConstructor.optimizerHints, jsConstructor.version)(
          newJSConstructor.pos)
    }

    override def transformJSMethodDef(jsMethodDef: JSMethodDef): JSMethodDef = {
      val newJSMethodDef = super.transformJSMethodDef(jsMethodDef)
      newJSMethodDef.copy()(newJSMethodDef.optimizerHints, jsMethodDef.version)(
          newJSMethodDef.pos)
    }

    override def transformJSPropertyDef(jsPropertyDef: JSPropertyDef): JSPropertyDef = {
      val newJSPropertyDef = super.transformJSPropertyDef(jsPropertyDef)
      newJSPropertyDef.copy()(jsPropertyDef.version)(newJSPropertyDef.pos)
    }
  }

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private[frontend] def refineClassDef(classDef: ClassDef, version: Version,
      analysis: Analysis): (LinkedClass, List[LinkedTopLevelExport]) = {
    linkClassDef(classDef, version, syntheticMethodDefs = Nil, analysis,
        desugarTransformer = None)
  }

  /** Takes a ClassDef and DCE infos to construct a stripped down LinkedClass.
   */
  private def linkClassDef(classDef: ClassDef, version: Version,
      syntheticMethodDefs: List[MethodDef], analysis: Analysis,
      desugarTransformer: Option[DesugarTransformer]): (LinkedClass, List[LinkedTopLevelExport]) = {
    import ir.Trees._

    def requireDesugarTransformer(): DesugarTransformer = {
      desugarTransformer.getOrElse {
        throw new AssertionError(
            s"Unexpected desugaring needed in refiner in class ${classDef.className.nameString}")
      }
    }

    val classInfo = analysis.classInfos(classDef.className)

    val fields = classDef.fields.filter {
      case field: FieldDef =>
        if (field.flags.namespace.isStatic)
          classInfo.staticFieldsRead(field.name.name) || classInfo.staticFieldsWritten(field.name.name)
        else if (classInfo.kind.isJSClass || classInfo.isAnySubclassInstantiated)
          classInfo.fieldsRead(field.name.name) || classInfo.fieldsWritten(field.name.name)
        else
          false

      case field: JSFieldDef =>
        classInfo.isAnySubclassInstantiated
    }

    val methods: List[MethodDef] = classDef.methods.iterator
      .map(m => m -> classInfo.methodInfos(m.flags.namespace)(m.methodName))
      .filter(_._2.isReachable)
      .map { case (m, info) =>
        assert(m.body.isDefined,
            s"The abstract method ${classDef.name.name}.${m.methodName} is reachable.")
        if (!info.needsDesugaring)
          m
        else
          requireDesugarTransformer().transformMethodDef(m)
      }
      .toList

    val (jsConstructor, jsMethodProps) = if (classInfo.isAnySubclassInstantiated) {
      val anyJSMemberNeedsDesugaring = classInfo.anyJSMemberNeedsDesugaring

      if (!anyJSMemberNeedsDesugaring) {
        (classDef.jsConstructor, classDef.jsMethodProps)
      } else {
        val transformer = requireDesugarTransformer()
        (classDef.jsConstructor.map(transformer.transformJSConstructorDef(_)),
            classDef.jsMethodProps.map(transformer.transformJSMethodPropDef(_)))
      }
    } else {
      (None, Nil)
    }

    val jsNativeMembers = classDef.jsNativeMembers
      .filter(m => classInfo.jsNativeMembersUsed.contains(m.name.name))

    val allMethods = methods ++ syntheticMethodDefs

    val ancestors = classInfo.ancestors.map(_.className)

    val linkedClass = new LinkedClass(
        classDef.name,
        classDef.kind,
        classDef.jsClassCaptures,
        classDef.superClass,
        classDef.interfaces,
        classDef.jsSuperClass,
        classDef.jsNativeLoadSpec,
        fields,
        allMethods,
        jsConstructor,
        jsMethodProps,
        jsNativeMembers,
        classDef.optimizerHints,
        classDef.pos,
        ancestors.toList,
        hasInstances = classInfo.isAnySubclassInstantiated,
        hasDirectInstances = classInfo.isInstantiated,
        hasInstanceTests = classInfo.areInstanceTestsUsed,
        hasRuntimeTypeInfo = classInfo.isDataAccessed,
        fieldsRead = classInfo.fieldsRead.toSet,
        staticFieldsRead = classInfo.staticFieldsRead.toSet,
        staticDependencies = classInfo.staticDependencies.toSet,
        externalDependencies = classInfo.externalDependencies.toSet,
        dynamicDependencies = classInfo.dynamicDependencies.toSet,
        version)

    val linkedTopLevelExports = for {
      topLevelExport <- classDef.topLevelExportDefs
    } yield {
      val infos = analysis.topLevelExportInfos(
        (ModuleID(topLevelExport.moduleID), topLevelExport.topLevelExportName))
      val desugared =
        if (!infos.needsDesugaring) topLevelExport
        else requireDesugarTransformer().transformTopLevelExportDef(topLevelExport)
      new LinkedTopLevelExport(classDef.className, desugared,
          infos.staticDependencies.toSet, infos.externalDependencies.toSet)
    }

    (linkedClass, linkedTopLevelExports)
  }
}
