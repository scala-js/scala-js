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

package org.scalajs.linker.backend.emitter

import org.scalajs.ir.Trees._
import org.scalajs.ir.WellKnownNames._

/** Load-time patches for the private lib IR.
 *
 *  This introduces Transients into selected methods of the private lib. They
 *  cannot be serialized, so they cannot be adapted when producing the sjsir.
 */
object PrivateLibPatches {
  def patchClassDef(classDef: ClassDef): ClassDef = {
    if (classDef.className == LongImpl.RuntimeLongModClass)
      patchRuntimeLongModClass(classDef)
    else
      classDef
  }

  /** Patches the `RuntimeLong` module class.
   *
   *  Replaces the body of `pack()` by a `Transients.PackLong`.
   */
  private def patchRuntimeLongModClass(classDef: ClassDef): ClassDef = {
    import classDef._

    val newMethods = methods.map { methodDef =>
      if (methodDef.methodName == LongImpl.pack)
        patchRTLongPack(methodDef)
      else
        methodDef
    }

    ClassDef(
      name,
      originalName,
      kind,
      jsClassCaptures,
      superClass,
      interfaces,
      jsSuperClass,
      jsNativeLoadSpec,
      fields,
      newMethods,
      jsConstructor,
      jsMethodProps,
      jsNativeMembers,
      topLevelExportDefs
    )(optimizerHints)
  }

  private def patchRTLongPack(methodDef: MethodDef): MethodDef = {
    import methodDef._
    val newBody = {
      implicit val pos = body.get.pos
      Transient(Transients.PackLong(args(0).ref, args(1).ref))
    }
    MethodDef(flags, name, originalName, args, resultType, Some(newBody))(
        optimizerHints, version)(pos)
  }
}
