package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._
import scala.scalajs.tools.io._

import scala.collection.immutable.{Seq, Traversable}

import java.io._

import org.json.simple.JSONValue

/** The information written to a "JS_DEPENDENCIES" manifest file. */
final case class JSDependencyManifest(
    origin: Origin,
    libDeps: List[JSDependency]) {
  def flatten: List[FlatJSDependency] = libDeps.map(_.withOrigin(origin))
}

object JSDependencyManifest {

  final val ManifestFileName = "JS_DEPENDENCIES"

  def createIncludeList(
      flatDeps: Traversable[FlatJSDependency]): List[String] = {
    val jsDeps = mergeManifests(flatDeps)

    // Verify all dependencies are met
    for {
      lib <- flatDeps
      dep <- lib.dependencies
      if !jsDeps.contains(dep)
    } yield sys.error(s"The JS dependency ${lib.resourceName} declared " +
        s"from ${lib.origin} has an unmet transitive dependency $dep")

    // Sort according to dependencies and return

    // Very simple O(n²) topological sort for elements assumed to be distinct
    // Copied :( from GenJSExports (but different exception)
    @scala.annotation.tailrec
    def loop(coll: List[String], acc: List[String]): List[String] = {
      if (coll.isEmpty) acc
      else if (coll.tail.isEmpty) coll.head :: acc
      else {
        val (selected, pending) = coll.partition { x =>
          coll forall { y => (x eq y) || !jsDeps(y).contains(x) }
        }

        if (selected.nonEmpty)
          loop(pending, selected ::: acc)
        else {
          // We can't sort the set. Make a nice error message.
          val msg = new StringBuilder()
          msg.append("There is a loop in the following JS dependencies:\n")

          for (dep <- pending) {
            msg.append(s"  '$dep' which depends on\n")
            for ((rdep, origins) <- jsDeps(dep)) {
              msg.append(s"    - $rdep from: ${origins.mkString(", ")}\n")
            }
          }

          sys.error(msg.toString())
        }
      }
    }

    loop(jsDeps.keys.toList, Nil)
  }

  /** Merges multiple JSDependencyManifests into a map of map:
   *  dependency -> includeAfter -> origins
   */
  private def mergeManifests(flatDeps: Traversable[FlatJSDependency]) = {
    def mergedIncludeAfter(tups: Traversable[FlatJSDependency]) = {
      val flat = for {
        flatDep         <- tups
        includeAfter    <- flatDep.dependencies
      } yield (includeAfter, flatDep)

      flat.groupBy(_._1).mapValues(_.map(_._2))
    }

    flatDeps.groupBy(_.resourceName).mapValues(mergedIncludeAfter)
  }

  implicit object JSDepManJSONSerializer extends JSONSerializer[JSDependencyManifest] {
    def serialize(x: JSDependencyManifest): Object = {
      new JSONObjBuilder()
        .fld("origin",  x.origin)
        .fld("libDeps", x.libDeps)
        .toJSON
    }
  }

  implicit object JSDepManJSONDeserializer extends JSONDeserializer[JSDependencyManifest] {
    def deserialize(x: Object): JSDependencyManifest = {
      val obj = new JSONObjExtractor(x)
      JSDependencyManifest(
          obj.fld[Origin]            ("origin"),
          obj.fld[List[JSDependency]]("libDeps"))
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
