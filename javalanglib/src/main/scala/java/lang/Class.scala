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

package java.lang

import scala.scalajs.js

@js.native
private trait ScalaJSClassData[A] extends js.Object {
  val name: String = js.native
  val isPrimitive: scala.Boolean = js.native
  val isInterface: scala.Boolean = js.native
  val isArrayClass: scala.Boolean = js.native

  def isInstance(obj: Object): scala.Boolean = js.native
  def isAssignableFrom(that: ScalaJSClassData[_]): scala.Boolean = js.native
  def checkCast(obj: Object): scala.Unit = js.native

  def getSuperclass(): Class[_ >: A] = js.native
  def getComponentType(): Class[_] = js.native

  def newArrayOfThisClass(dimensions: js.Array[Int]): AnyRef = js.native
}

final class Class[A] private (data0: Object) extends Object {
  private[this] val data: ScalaJSClassData[A] =
    data0.asInstanceOf[ScalaJSClassData[A]]

  /** Acces to `data` for other instances or `@inline` methods.
   *
   *  Directly accessing the `data` field from `@inline` methods will cause
   *  scalac to make the field public and mangle its name. Since the Emitter
   *  relies on the field being called exactly `data` in some of its
   *  optimizations, we must avoid that.
   *
   *  This non-`@noinline` method can be used to access the field without
   *  triggering scalac's mangling. Since it is a trivial accessor, the
   *  Scala.js optimizer will inline it anyway.
   */
  private def getData(): ScalaJSClassData[A] = data

  override def toString(): String = {
    (if (isInterface()) "interface " else if (isPrimitive()) "" else "class ") + getName()
  }

  def isInstance(obj: Object): scala.Boolean =
    data.isInstance(obj)

  def isAssignableFrom(that: Class[_]): scala.Boolean =
    this.data.isAssignableFrom(that.getData())

  def isInterface(): scala.Boolean =
    data.isInterface

  def isArray(): scala.Boolean =
    data.isArrayClass

  def isPrimitive(): scala.Boolean =
    data.isPrimitive

  def getName(): String =
    data.name

  def getSimpleName(): String = {
    val name = data.name
    var idx = name.length - 1
    while (idx >= 0 && name.charAt(idx) == '$') {
      idx -= 1
    }
    while (idx >= 0 && {
          val currChar = name.charAt(idx)
          currChar != '.' && currChar != '$'
        }) {
      idx -= 1
    }
    name.substring(idx + 1)
  }

  def getSuperclass(): Class[_ >: A] =
    data.getSuperclass()

  def getComponentType(): Class[_] =
    data.getComponentType()

  @inline
  def cast(obj: Object): A = {
    getData().checkCast(obj)
    obj.asInstanceOf[A]
  }

  // java.lang.reflect.Array support

  private[lang] def newArrayOfThisClass(dimensions: js.Array[Int]): AnyRef =
    data.newArrayOfThisClass(dimensions)
}
