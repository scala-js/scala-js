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

package java.security

abstract class Permission(name: String) extends Guard with Serializable {
  //def checkGuard(a: Any): Unit
  def implies(p: Permission): Boolean
  def equals(obj: Any): Boolean
  def hashCode(): Int
  //final def getName(): String
  def getActions(): String
  //def newPermissionCollection(): PermissionCollection
  override def toString(): String =
    s"ClassName ${this.getClass.getName} ${getActions}"
}
