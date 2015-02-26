package org.scalajs.core.tools.jsdep

import org.scalajs.core.tools.json._
import org.scalajs.core.tools.io._

import scala.collection.immutable.{Seq, Traversable}

import java.io.{Reader, Writer}

/** The information written to a "JS_DEPENDENCIES" manifest file. */
final class JSDependencyManifest(
    val origin: Origin,
    val libDeps: List[JSDependency],
    val requiresDOM: Boolean,
    val compliantSemantics: List[String]) {

  import JSDependencyManifest._

  @deprecated("flatten doesn't resolve partial paths, use your own code instead", "0.6.1")
  def flatten: List[FlatJSDependency] = libDeps.map(_.withOrigin(origin))

  override def equals(that: Any): Boolean = that match {
    case that: JSDependencyManifest =>
      this.origin == that.origin &&
      this.libDeps == that.libDeps &&
      this.requiresDOM == that.requiresDOM &&
      this.compliantSemantics == that.compliantSemantics
    case _ =>
      false
  }

  override def hashCode(): Int = {
    import scala.util.hashing.MurmurHash3._
    var acc = HashSeed
    acc = mix(acc, origin.##)
    acc = mix(acc, libDeps.##)
    acc = mix(acc, requiresDOM.##)
    acc = mixLast(acc, compliantSemantics.##)
    finalizeHash(acc, 4)
  }
}

object JSDependencyManifest {

  // "org.scalajs.core.tools.jsdep.JSDependencyManifest".##
  private final val HashSeed = 943487940

  final val ManifestFileName = "JS_DEPENDENCIES"

  def createIncludeList(
      flatDeps: Traversable[FlatJSDependency]): List[ResolutionInfo] = {
    val jsDeps = mergeManifests(flatDeps)

    // Verify all dependencies are met
    for {
      lib <- flatDeps
      dep <- lib.dependencies
      if !jsDeps.contains(dep)
    } throw new MissingDependencyException(lib, dep)

    // Sort according to dependencies and return

    // Very simple O(n²) topological sort for elements assumed to be distinct
    // Copied :( from GenJSExports (but different exception)
    @scala.annotation.tailrec
    def loop(coll: List[ResolutionInfo],
      acc: List[ResolutionInfo]): List[ResolutionInfo] = {

      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (selected, pending) = coll.partition { x =>
          coll forall { y => (x eq y) || !y.dependencies.contains(x.relPath) }
        }

        if (selected.nonEmpty)
          loop(pending, selected ::: acc)
        else
          throw new CyclicDependencyException(pending)
      }
    }

    loop(jsDeps.values.toList, Nil)
  }

  /** Merges multiple JSDependencyManifests into a map of map:
   *  resourceName -> ResolutionInfo
   */
  private def mergeManifests(flatDeps: Traversable[FlatJSDependency]) = {
    @inline
    def hasConflict(x: FlatJSDependency, y: FlatJSDependency) = (
      x.commonJSName.isDefined &&
      y.commonJSName.isDefined &&
      (x.relPath == y.relPath ^
          x.commonJSName == y.commonJSName)
    )

    val conflicts = flatDeps.filter(x =>
      flatDeps.exists(y => hasConflict(x,y)))

    if (conflicts.nonEmpty)
      throw new ConflictingNameException(conflicts.toList)

    flatDeps.groupBy(_.relPath).mapValues { sameName =>
      new ResolutionInfo(
        relPath = sameName.head.relPath,
        dependencies = sameName.flatMap(_.dependencies).toSet,
        origins = sameName.map(_.origin).toList,
        commonJSName = sameName.flatMap(_.commonJSName).headOption
      )
    }
  }

  implicit object JSDepManJSONSerializer extends JSONSerializer[JSDependencyManifest] {
    @inline def optList[T](x: List[T]): Option[List[T]] =
      if (x.nonEmpty) Some(x) else None

    def serialize(x: JSDependencyManifest): JSON = {
      new JSONObjBuilder()
        .fld("origin",  x.origin)
        .opt("libDeps", optList(x.libDeps))
        .opt("requiresDOM", if (x.requiresDOM) Some(true) else None)
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
          obj.opt[List[String]]      ("compliantSemantics").getOrElse(Nil))
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
