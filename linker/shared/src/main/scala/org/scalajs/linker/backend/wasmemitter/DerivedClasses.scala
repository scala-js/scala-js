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

package org.scalajs.linker.backend.wasmemitter

import scala.concurrent.{ExecutionContext, Future}

import org.scalajs.ir.ClassKind._
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Position
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._
import org.scalajs.ir.{EntryPointsInfo, Version}

import org.scalajs.linker.interface.IRFile
import org.scalajs.linker.interface.unstable.IRFileImpl

import org.scalajs.linker.standard.LinkedClass

import SpecialNames._

/** Derives `CharacterBox` and `LongBox` from `jl.Character` and `jl.Long`. */
object DerivedClasses {
  def deriveClasses(classes: List[LinkedClass]): List[LinkedClass] = {
    classes.collect {
      case clazz if clazz.className == BoxedCharacterClass || clazz.className == BoxedLongClass =>
        deriveBoxClass(clazz)
    }
  }

  /** Generates the accompanying Box class of `Character` or `Long`.
   *
   *  These box classes will be used as the generic representation of `char`s and `long`s when they
   *  are upcast to `java.lang.Character`/`java.lang.Long` or any of their supertypes.
   *
   *  The generated Box classes mimic the public structure of the corresponding hijacked classes.
   *  Whereas the hijacked classes instances *are* the primitives (conceptually), the box classes
   *  contain an explicit `value` field of the primitive type. They delegate all their instance
   *  methods to the corresponding methods of the hijacked class, applied on the `value` primitive.
   *
   *  For example, given the hijacked class
   *
   *  {{{
   *  hijacked class Long extends java.lang.Number with Comparable {
   *    def longValue;J(): long = this.asInstanceOf[long]
   *    def toString;T(): string = Long$.toString(this.longValue;J())
   *    def compareTo;jlLong;Z(that: java.lang.Long): boolean =
   *      Long$.compare(this.longValue;J(), that.longValue;J())
   *  }
   *  }}}
   *
   *  we generate
   *
   *  {{{
   *  class LongBox extends java.lang.Number with Comparable {
   *    val value: long
   *    def <init>(value: long) = { this.value = value }
   *    def longValue;J(): long = this.value.longValue;J()
   *    def toString;T(): string = this.value.toString;J()
   *    def compareTo;jlLong;Z(that: jlLong): boolean =
   *      this.value.compareTo;jlLong;Z(that)
   *  }
   *  }}}
   */
  private def deriveBoxClass(clazz: LinkedClass): LinkedClass = {
    implicit val pos: Position = clazz.pos

    val EAF = ApplyFlags.empty
    val EMF = MemberFlags.empty
    val EOH = OptimizerHints.empty
    val NON = NoOriginalName
    val NOV = Version.Unversioned

    val className = clazz.className
    val derivedClassName = className.withSuffix("Box")
    val primType = BoxedClassToPrimType(className).asInstanceOf[PrimTypeWithRef]
    val derivedClassType = ClassType(derivedClassName)

    val fieldName = FieldName(derivedClassName, valueFieldSimpleName)
    val fieldIdent = FieldIdent(fieldName)

    val derivedFields: List[FieldDef] = List(
      FieldDef(EMF, fieldIdent, NON, primType)
    )

    val selectField = Select(This()(derivedClassType), fieldIdent)(primType)

    val ctorParamDef =
      ParamDef(LocalIdent(fieldName.simpleName.toLocalName), NON, primType, mutable = false)
    val derivedCtor = MethodDef(
      EMF.withNamespace(MemberNamespace.Constructor),
      MethodIdent(MethodName.constructor(List(primType.primRef))),
      NON,
      List(ctorParamDef),
      NoType,
      Some(Assign(selectField, ctorParamDef.ref))
    )(EOH, NOV)

    val derivedMethods: List[MethodDef] = for {
      method <- clazz.methods if method.flags.namespace == MemberNamespace.Public
    } yield {
      MethodDef(
        method.flags,
        method.name,
        method.originalName,
        method.args,
        method.resultType,
        Some(ApplyStatically(EAF, selectField, className, method.name,
            method.args.map(_.ref))(method.resultType))
      )(method.optimizerHints, method.version)
    }

    new LinkedClass(
      ClassIdent(derivedClassName),
      Class,
      jsClassCaptures = None,
      clazz.superClass,
      clazz.interfaces,
      jsSuperClass = None,
      jsNativeLoadSpec = None,
      derivedFields,
      derivedCtor :: derivedMethods,
      jsConstructorDef = None,
      exportedMembers = Nil,
      jsNativeMembers = Nil,
      EOH,
      pos,
      ancestors = derivedClassName :: clazz.ancestors.tail,
      hasInstances = true,
      hasDirectInstances = true,
      hasInstanceTests = true,
      hasRuntimeTypeInfo = true,
      fieldsRead = Set(fieldName),
      staticFieldsRead = Set.empty,
      staticDependencies = Set.empty,
      externalDependencies = Set.empty,
      dynamicDependencies = Set.empty,
      clazz.version
    )
  }
}
