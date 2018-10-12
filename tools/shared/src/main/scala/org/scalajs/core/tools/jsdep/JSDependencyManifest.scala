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

package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.json._
import org.scalajs.core.tools.io._

import scala.collection.immutable.{Seq, Traversable}

import java.io.{Reader, Writer}

/** The information written to a "JS_DEPENDENCIES" manifest file. */
final class JSDependencyManifest private[scalajs] (
    val origin: Origin,
    val libDeps: List[JSDependency],
    private[scalajs] val requiresDOMInternal: Boolean,
    val compliantSemantics: List[String],
    internal: Unit) {

  import JSDependencyManifest._

  def this(origin: Origin, libDeps: List[JSDependency],
      compliantSemantics: List[String]) = {
    this(origin, libDeps, false, compliantSemantics, internal = ())
  }

  @deprecated(
      "requiresDOM will be removed in 1.x, use the overload without it.",
      "0.6.20")
  def this(origin: Origin, libDeps: List[JSDependency], requiresDOM: Boolean,
      compliantSemantics: List[String]) = {
    this(origin, libDeps, requiresDOM, compliantSemantics, internal = ())
  }

  @deprecated("requiresDOM will be removed in 1.x", "0.6.20")
  val requiresDOM: Boolean = requiresDOMInternal

  override def equals(that: Any): Boolean = that match {
    case that: JSDependencyManifest =>
      this.origin == that.origin &&
      this.libDeps == that.libDeps &&
      this.requiresDOMInternal == that.requiresDOMInternal &&
      this.compliantSemantics == that.compliantSemantics
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, origin.##)
    acc = mix(acc, libDeps.##)
    acc = mix(acc, requiresDOMInternal.##)
    acc = mixLast(acc, compliantSemantics.##)
    finalizeHash(acc, 4)
  }

  override def toString(): String = {
    val b = new StringBuilder
    b ++= s"JSDependencyManifest(origin=$origin"
    if (libDeps.nonEmpty)
      b ++= s", libDeps=$libDeps"
    if (requiresDOMInternal)
      b ++= s", requiresDOM=$requiresDOMInternal"
    if (compliantSemantics.nonEmpty)
      b ++= s", compliantSemantics=$compliantSemantics"
    b ++= ")"
    b.result()
  }
}

object JSDependencyManifest {

  // "org.scalajs.core.tools.jsdep.JSDependencyManifest".##
  private final val HashSeed = 943487940

  final val ManifestFileName = "JS_DEPENDENCIES"

  implicit object JSDepManJSONSerializer extends JSONSerializer[JSDependencyManifest] {
    @inline def optList[T](x: List[T]): Option[List[T]] =
      if (x.nonEmpty) Some(x) else None

    def serialize(x: JSDependencyManifest): JSON = {
      new JSONObjBuilder()
        .fld("origin",  x.origin)
        .opt("libDeps", optList(x.libDeps))
        .opt("requiresDOM", if (x.requiresDOMInternal) Some(true) else None)
        .opt("compliantSemantics", optList(x.compliantSemantics))
        .toJSON
    }
  }

  implicit object JSDepManJSONDeserializer extends JSONDeserializer[JSDependencyManifest] {
    def deserialize(x: JSON): JSDependencyManifest = {
      val obj = new JSONObjExtractor(x)
      new JSDependencyManifest(
          obj.fld[Origin]            ("origin"),
          obj.opt[List[JSDependency]]("libDeps").getOrElse(Nil),
          obj.opt[Boolean]           ("requiresDOM").getOrElse(false),
          obj.opt[List[String]]      ("compliantSemantics").getOrElse(Nil),
          internal = ())
    }
  }

  def write(dep: JSDependencyManifest, output: WritableVirtualTextFile): Unit = {
    val writer = output.contentWriter
    try write(dep, writer)
    finally writer.close()
  }

  def write(dep: JSDependencyManifest, writer: Writer): Unit =
    writeJSON(dep.toJSON, writer)

  def read(file: VirtualTextFile): JSDependencyManifest = {
    val reader = file.reader
    try read(reader)
    finally reader.close()
  }

  def read(reader: Reader): JSDependencyManifest =
    fromJSON[JSDependencyManifest](readJSON(reader))

}
