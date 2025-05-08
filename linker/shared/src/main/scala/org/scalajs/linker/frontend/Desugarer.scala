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

import scala.collection.mutable

import org.scalajs.logging._

import org.scalajs.linker.standard._
import org.scalajs.linker.checker._

import org.scalajs.ir.Names._
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.{Position, Version}

/** Desugars a linking unit. */
final class Desugarer(config: CommonPhaseConfig, checkIR: Boolean) {
  import Desugarer._

  private val linkTimeProperties = LinkTimeProperties.fromCoreSpec(config.coreSpec)

  private val desugarTransformer = new DesugarTransformer(linkTimeProperties)

  def desugar(unit: LinkingUnit, logger: Logger): LinkingUnit = {
    val result = logger.time("Desugarer: Desugar") {
      val desugaredClasses = unit.classDefs.map(desugarClass(_))
      val desugaredTopLevelExports = unit.topLevelExports.map(desugarTopLevelExport(_))

      new LinkingUnit(desugaredClasses, desugaredTopLevelExports,
          unit.moduleInitializers, unit.globalInfo)
    }

    if (checkIR) {
      logger.time("Desugarer: Check IR") {
        val errorCount = IRChecker.check(linkTimeProperties, result, logger,
            CheckingPhase.Desugarer)
        if (errorCount != 0) {
          throw new AssertionError(
              s"There were $errorCount IR checking errors after desugaring (this is a Scala.js bug)")
        }
      }
    }

    result
  }

  private def desugarClass(linkedClass: LinkedClass): LinkedClass = {
    import linkedClass._

    if (desugaringRequirements.isEmpty) {
      linkedClass
    } else {
      val newMethods = methods.map { method =>
        if (!desugaringRequirements.containsMethod(method.flags.namespace, method.methodName))
          method
        else
          desugarTransformer.transformMethodDef(method)
      }

      val newJSConstructorDef =
        if (!desugaringRequirements.exportedMembers) jsConstructorDef
        else jsConstructorDef.map(desugarTransformer.transformJSConstructorDef(_))

      val newExportedMembers =
        if (!desugaringRequirements.exportedMembers) exportedMembers
        else exportedMembers.map(desugarTransformer.transformJSMethodPropDef(_))

      new LinkedClass(
        name,
        kind,
        jsClassCaptures,
        superClass,
        interfaces,
        jsSuperClass,
        jsNativeLoadSpec,
        fields,
        methods = newMethods,
        jsConstructorDef = newJSConstructorDef,
        exportedMembers = newExportedMembers,
        jsNativeMembers,
        optimizerHints,
        pos,
        ancestors,
        hasInstances,
        hasDirectInstances,
        hasInstanceTests,
        hasRuntimeTypeInfo,
        fieldsRead,
        staticFieldsRead,
        staticDependencies,
        externalDependencies,
        dynamicDependencies,
        LinkedClass.DesugaringRequirements.Empty,
        version
      )
    }
  }

  private def desugarTopLevelExport(tle: LinkedTopLevelExport): LinkedTopLevelExport = {
    import tle._
    if (!tle.needsDesugaring) {
      tle
    } else {
      val newTree = desugarTransformer.transformTopLevelExportDef(tree)
      new LinkedTopLevelExport(owningClass, newTree, staticDependencies,
          externalDependencies, needsDesugaring = false)
    }
  }
}

private[linker] object Desugarer {

  private final class DesugarTransformer(linkTimeProperties: LinkTimeProperties)
      extends ClassTransformer {

    /* Cache the names generated for lambda classes because computing their
     * `ClassName` is a bit expensive. The constructor names are not expensive,
     * but we might as well cache them together.
     */
    private val syntheticLambdaNamesCache =
      mutable.Map.empty[NewLambda.Descriptor, (ClassName, MethodName)]

    private def syntheticLambdaNamesFor(descriptor: NewLambda.Descriptor): (ClassName, MethodName) =
      syntheticLambdaNamesCache.getOrElseUpdate(descriptor, {
        (LambdaSynthesizer.makeClassName(descriptor), LambdaSynthesizer.makeConstructorName(descriptor))
      })

    override def transform(tree: Tree): Tree = {
      tree match {
        case LinkTimeProperty(name) =>
          implicit val pos = tree.pos
          val value = linkTimeProperties.get(name).getOrElse {
            throw new IllegalArgumentException(
                s"link time property not found: '$name' of type ${tree.tpe}")
          }
          value match {
            case LinkTimeProperties.LinkTimeBoolean(value) => BooleanLiteral(value)
            case LinkTimeProperties.LinkTimeInt(value)     => IntLiteral(value)
            case LinkTimeProperties.LinkTimeString(value)  => StringLiteral(value)
          }

        case LinkTimeIf(cond, thenp, elsep) =>
          LinkTimeEvaluator.tryEvalLinkTimeBooleanExpr(linkTimeProperties, cond) match {
            case Some(result) =>
              if (result)
                transform(thenp)
              else
                transform(elsep)
            case None =>
              throw new AssertionError(
                  s"Invalid link-time condition should not have passed the reachability analysis:\n" +
                  s"${tree.show}\n" +
                  s"at ${tree.pos}.\n" +
                  "Consider running the linker with `withCheckIR(true)` before submitting a bug report.")
          }

        case NewLambda(descriptor, fun) =>
          implicit val pos = tree.pos
          val (className, ctorName) = syntheticLambdaNamesFor(descriptor)
          New(className, MethodIdent(ctorName), List(transform(fun)))

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
}
