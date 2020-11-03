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

package org.scalajs.linker.interface

import scala.collection.mutable

private[interface] trait Fingerprint[T] {

  /** Generate a fingerprint of an object.
   *
   *  A fingerprint is an injective one-way serialization representing the
   *  object.
   *
   *  @param obj Object to fingerprint
   *  @return A fingerprint of the object
   */
  def fingerprint(obj: T): String
}

private[interface] object Fingerprint {
  def fingerprint[T: Fingerprint](obj: T): String =
    implicitly[Fingerprint[T]].fingerprint(obj)

  final class FingerprintBuilder(name: String) {
    private val fields = new mutable.ListBuffer[String]

    def addField[T: Fingerprint](name: String, value: T): FingerprintBuilder = {
      fields.append(s"$name=${fingerprint(value)}")
      this
    }

    def build(): String = fields.mkString(s"$name(", ",", ")")
  }

  implicit object BooleanFingerprint extends Fingerprint[Boolean] {
    override def fingerprint(bool: Boolean): String = bool.toString
  }

  implicit object IntFingerprint extends Fingerprint[Int] {
    override def fingerprint(int: Int): String = int.toString
  }

  implicit object StringFingerprint extends Fingerprint[String] {
    override def fingerprint(str: String): String = str
  }

  implicit def optionFingerprint[T: Fingerprint]: Fingerprint[Option[T]] = {
    new Fingerprint[Option[T]] {
      override def fingerprint(opt: Option[T]): String = opt match {
        case Some(x) =>
          s"Some(${implicitly[Fingerprint[T]].fingerprint(x)})"
        case None => "None"
      }
    }
  }

  implicit def listFingerprint[T: Fingerprint]: Fingerprint[List[T]] = {
    new Fingerprint[List[T]] {
      override def fingerprint(list: List[T]): String = {
        list
          .map(implicitly[Fingerprint[T]].fingerprint)
          .mkString("List(", ",", ")")
      }
    }
  }
}
