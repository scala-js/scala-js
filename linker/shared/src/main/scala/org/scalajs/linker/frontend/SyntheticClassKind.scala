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

import org.scalajs.ir.{ClassKind, Hashers, Position, SHA1, UTF8String, Version}
import org.scalajs.ir.Names._
import org.scalajs.ir.OriginalName.NoOriginalName
import org.scalajs.ir.Trees._
import org.scalajs.ir.Types._

sealed abstract class SyntheticClassKind {
  def synthesize(): ClassDef
}

object SyntheticClassKind {
  /* Everything we create has a constant version because the names are derived
   * from the synthetic kinds themselves.
   */
  private[frontend] val constantVersion = Version.fromByte(0)

  final case class Lambda(descriptor: NewLambda.Descriptor) extends SyntheticClassKind {
    def synthesize(): ClassDef = {
      implicit val pos = Position.NoPosition

      import descriptor._

      val className = makeClassName()

      val thisType = ClassType(className, nullable = false)
      val closureTypeNull = ClosureType(paramTypes, resultType, nullable = true)
      val closureTypeNonNull = closureTypeNull.toNonNullable

      val fFieldName = FieldName(className, SimpleFieldName("f"))
      val ctorName = MethodName.constructor(TransientTypeRef(closureTypeNonNull) :: Nil)

      val thiz = This()(thisType)

      val fFieldDef = FieldDef(MemberFlags.empty, FieldIdent(fFieldName), NoOriginalName, closureTypeNull)

      val methodParamDefs = paramTypes.zipWithIndex.map { case (paramType, index) =>
        ParamDef(LocalIdent(LocalName("x" + index)), NoOriginalName, paramType, mutable = false)
      }

      val ctorParamDef = ParamDef(LocalIdent(LocalName("f")), NoOriginalName,
          closureTypeNonNull, mutable = false)

      val ctorDef = MethodDef(
        MemberFlags.empty.withNamespace(MemberNamespace.Constructor),
        MethodIdent(ctorName),
        NoOriginalName,
        ctorParamDef :: Nil,
        VoidType,
        Some(
          Block(
            Assign(Select(thiz, FieldIdent(fFieldName))(closureTypeNull), ctorParamDef.ref),
            ApplyStatically(ApplyFlags.empty.withConstructor(true), thiz,
                superClass, MethodIdent(NoArgConstructorName), Nil)(VoidType)
          )
        )
      )(OptimizerHints.empty, constantVersion)

      val methodDef = MethodDef(
        MemberFlags.empty,
        MethodIdent(methodName),
        NoOriginalName,
        methodParamDefs,
        resultType,
        Some(
          ApplyTypedClosure(
            ApplyFlags.empty,
            Select(thiz, FieldIdent(fFieldName))(closureTypeNull),
            methodParamDefs.map(_.ref)
          )
        )
      )(OptimizerHints.empty, constantVersion)

      val classDef = ClassDef(
        ClassIdent(className),
        NoOriginalName,
        ClassKind.Class,
        jsClassCaptures = None,
        superClass = Some(ClassIdent(superClass)),
        interfaces = interfaces.map(ClassIdent(_)),
        jsSuperClass = None,
        jsNativeLoadSpec = None,
        fields = List(fFieldDef),
        methods = List(ctorDef, methodDef),
        jsConstructor = None,
        jsMethodProps = Nil,
        jsNativeMembers = Nil,
        topLevelExportDefs = Nil
      )(OptimizerHints.empty.withInline(true))

      Hashers.hashClassDef(classDef)
    }

    private def makeClassName(): ClassName = {
      // Choose a base class name that will "makes sense" for debugging purposes
      val baseClassName = {
        if (descriptor.superClass == ObjectClass && descriptor.interfaces.nonEmpty)
          descriptor.interfaces.head
        else
          descriptor.superClass
      }

      val digestBuilder = new SHA1.DigestBuilder()
      digestBuilder.updateUTF8String(descriptor.superClass.encoded)
      for (intf <- descriptor.interfaces)
        digestBuilder.updateUTF8String(intf.encoded)

      // FIXME This is not efficient
      digestBuilder.updateUTF8String(UTF8String(descriptor.methodName.nameString))

      // No need the hash the paramTypes and resultType because they derive from the method name

      val digest = digestBuilder.finalizeDigest()

      /* The "$$Lambda" segment is meant to match the way LambdaMetaFactory
       * names generated classes. This is mostly for test compatibility
       * purposes (like partest's that test the class name to tell whether a
       * lambda was indeed encoded as an LMF).
       */
      val suffixBuilder = new java.lang.StringBuilder(".$$Lambda$")
      for (b <- digest) {
        val i = b & 0xff
        suffixBuilder.append(Character.forDigit(i >> 4, 16)).append(Character.forDigit(i & 0x0f, 16))
      }

      ClassName(baseClassName.encoded ++ UTF8String(suffixBuilder.toString()))
    }
  }
}
