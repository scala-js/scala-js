package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._
import scala.scalajs.tools.io._

import scala.collection.immutable.{Seq, Traversable}

import java.io._

import org.json.simple.JSONValue

/** The information written to a "JS_DEPENDENCIES" manifest file. */
final case class JSDependencyManifest(
    origin: Origin,
    libDeps: List[JSDependency],
    requiresDOM: Boolean) {
  def flatten: List[FlatJSDependency] = libDeps.map(_.withOrigin(origin))
}

object JSDependencyManifest {

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
          coll forall { y => (x eq y) || !y.dependencies.contains(x.resourceName) }
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
      (x.resourceName == y.resourceName ^
       x.commonJSName == y.commonJSName)
    )

    val conflicts = flatDeps.filter(x =>
      flatDeps.exists(y => hasConflict(x,y)))

    if (conflicts.nonEmpty)
      throw new ConflictingNameException(conflicts.toList)

    flatDeps.groupBy(_.resourceName).mapValues { sameName =>
      ResolutionInfo(
        resourceName = sameName.head.resourceName,
        dependencies = sameName.flatMap(_.dependencies).toSet,
        origins = sameName.map(_.origin).toList,
        commonJSName = sameName.flatMap(_.commonJSName).headOption
      )
    }
  }

  implicit object JSDepManJSONSerializer extends JSONSerializer[JSDependencyManifest] {
    def serialize(x: JSDependencyManifest): Object = {
      new JSONObjBuilder()
        .fld("origin",  x.origin)
        .fld("libDeps", x.libDeps)
        .opt("requiresDOM", Some(x.requiresDOM))
        .toJSON
    }
  }

  implicit object JSDepManJSONDeserializer extends JSONDeserializer[JSDependencyManifest] {
    def deserialize(x: Object): JSDependencyManifest = {
      val obj = new JSONObjExtractor(x)
      JSDependencyManifest(
          obj.fld[Origin]            ("origin"),
          obj.fld[List[JSDependency]]("libDeps"),
          obj.opt[Boolean]           ("requiresDOM").getOrElse(false))
    }
  }

  def write(dep: JSDependencyManifest, output: WritableVirtualTextFile): Unit = {
    val writer = output.contentWriter
    try write(dep, writer)
    finally writer.close()
  }

  def write(dep: JSDependencyManifest, file: File): Unit = {
    val writer = new BufferedWriter(new FileWriter(file))
    try write(dep, writer)
    finally writer.close()
  }

  def write(dep: JSDependencyManifest, writer: Writer): Unit =
    JSONValue.writeJSONString(dep.toJSON, writer)

  def read(file: File): JSDependencyManifest = {
    val reader = new BufferedReader(new FileReader(file))
    try read(reader)
    finally reader.close()
  }

  def read(file: VirtualTextFile): JSDependencyManifest = {
    val reader = file.reader
    try read(reader)
    finally reader.close()
  }

  def read(reader: Reader): JSDependencyManifest =
    fromJSON[JSDependencyManifest](JSONValue.parse(reader))

}
