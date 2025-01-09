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
import scala.concurrent._

import org.scalajs.logging._

import org.scalajs.linker.interface._
import org.scalajs.linker.standard._
import org.scalajs.linker.checker._

import org.scalajs.ir
import org.scalajs.ir.Names._
import org.scalajs.ir.Printers.IRTreePrinter
import org.scalajs.ir.Transformers._
import org.scalajs.ir.Traversers._
import org.scalajs.ir.Trees._
import org.scalajs.ir.{Position, Version}

/** Desugars a linking unit. */
final class Desugarer(config: CommonPhaseConfig, checkIRFor: Option[CheckingPhase]) {
  import Desugarer._
  import Transients._

  private val desugarTransformer = new DesugarTransformer(config.coreSpec)

  def desugar(unit: LinkingUnit, logger: Logger): LinkingUnit = {
    val result = logger.time("Desugarer: Desugar") {
      val desugaredClasses = unit.classDefs.map(desugarClass(_))
      val desugaredTopLevelExports = unit.topLevelExports.map(desugarTopLevelExport(_))

      new LinkingUnit(desugaredClasses, desugaredTopLevelExports,
          unit.moduleInitializers, unit.globalInfo)
    }

    for (nextPhase <- checkIRFor) {
      logger.time("Desugarer: Check IR") {
        val errorCount = IRChecker.check(result, logger, nextPhase)
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

    val newMethods = methods.mapConserve { method =>
      desugarTransformer.transformMethodDef(method)
    }

    val newJSConstructorDef = jsConstructorDef.mapConserve { jsCtor =>
      desugarTransformer.transformJSConstructorDef(jsCtor)
    }

    val newExportedMembers = exportedMembers.mapConserve { jsMethodProp =>
      desugarTransformer.transformJSMethodPropDef(jsMethodProp)
    }

    if ((newMethods eq methods) && (newJSConstructorDef eq jsConstructorDef) &&
        (newExportedMembers eq exportedMembers)) {
      linkedClass
    } else {
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
        version
      )
    }
  }

  private def desugarTopLevelExport(tle: LinkedTopLevelExport): LinkedTopLevelExport = {
    import tle._
    val newTree = desugarTransformer.transformTopLevelExportDef(tree)
    if (newTree eq tree)
      tle
    else
      new LinkedTopLevelExport(owningClass, newTree, staticDependencies, externalDependencies)
  }
}

private[linker] object Desugarer {

  object Transients {
    final case class Desugar(body: Tree) extends Transient.Value {
      val tpe = body.tpe

      def traverse(traverser: Traverser): Unit =
        traverser.traverse(body)

      def transform(transformer: Transformer)(implicit pos: Position): Tree =
        Transient(Desugar(transformer.transform(body)))

      def printIR(out: IRTreePrinter): Unit = {
        out.print("desugar ")
        out.print(body)
      }
    }
  }

  import Transients._

  private final class DesugarTransformer(coreSpec: CoreSpec)
      extends ClassTransformer {

    override def transform(tree: Tree): Tree = {
      tree match {
        case Transient(Desugar(body)) =>
          transform(body)

        case prop: LinkTimeProperty =>
          coreSpec.linkTimeProperties.transformLinkTimeProperty(prop)

        case _ =>
          super.transform(tree)
      }
    }

    /* Transfer Version from old members to transformed members.
     * We can do this because the transformation only depends on the
     * `coreSpec`, which is immutable.
     */

    override def transformMethodDef(methodDef: MethodDef): MethodDef = {
      methodDef.body match {
        case Some(Transient(Desugar(_))) =>
          val newMethodDef = super.transformMethodDef(methodDef)
          newMethodDef.copy()(newMethodDef.optimizerHints, methodDef.version)(newMethodDef.pos)
        case _ =>
          methodDef
      }
    }

    override def transformJSConstructorDef(jsConstructor: JSConstructorDef): JSConstructorDef = {
      /* We cheat here. A JSConstructorBody has a mandatory super call,
       * statically separated from the other statements. Therefore we cannot
       * wrap the entire body with a `Desugar` node. Instead, we put a
       * `Desugar` node at the beginning of the body, but it still signals that
       * we should transform the whole body.
       */
      jsConstructor.body.beforeSuper match {
        case Transient(Desugar(_)) :: _ =>
          val newJSConstructor = super.transformJSConstructorDef(jsConstructor)
          newJSConstructor.copy()(newJSConstructor.optimizerHints, jsConstructor.version)(
              newJSConstructor.pos)
        case _ =>
          jsConstructor
      }
    }

    override def transformJSMethodDef(jsMethodDef: JSMethodDef): JSMethodDef = {
      jsMethodDef.body match {
        case Transient(Desugar(_)) =>
          val newJSMethodDef = super.transformJSMethodDef(jsMethodDef)
          newJSMethodDef.copy()(newJSMethodDef.optimizerHints, jsMethodDef.version)(
              newJSMethodDef.pos)
        case _ =>
          jsMethodDef
      }
    }

    override def transformJSPropertyDef(jsPropertyDef: JSPropertyDef): JSPropertyDef = {
      val needsDesugaring = jsPropertyDef match {
        case JSPropertyDef(_, _, Some(Transient(Desugar(_))), _)      => true
        case JSPropertyDef(_, _, _, Some((_, Transient(Desugar(_))))) => true
        case _                                                        => false
      }
      if (needsDesugaring) {
        val newJSPropertyDef = super.transformJSPropertyDef(jsPropertyDef)
        newJSPropertyDef.copy()(jsPropertyDef.version)(newJSPropertyDef.pos)
      } else {
        jsPropertyDef
      }
    }

    override def transformTopLevelExportDef(exportDef: TopLevelExportDef): TopLevelExportDef = {
      exportDef match {
        case TopLevelMethodExportDef(exportName, jsMethodDef) =>
          val newJSMethodDef = transformJSMethodDef(jsMethodDef)
          if (newJSMethodDef eq jsMethodDef)
            exportDef
          else
            TopLevelMethodExportDef(exportName, newJSMethodDef)(exportDef.pos)

        case _ =>
          exportDef
      }
    }
  }

  private implicit class OptionMapConserve[+A <: AnyRef](private val self: Option[A])
      extends AnyVal {

    def mapConserve[B >: A <: AnyRef](f: A => B): Option[B] = self match {
      case None =>
        None
      case Some(value) =>
        val newValue = f(value)
        if (newValue eq value)
          self
        else
          Some(newValue)
    }
  }
}
