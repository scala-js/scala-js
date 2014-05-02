package scala.scalajs.tools.jsdep

import scala.scalajs.tools.json._
import scala.scalajs.tools.io._

import org.json.simple.JSONValue

final case class JSDependencyManifest(
    origin: Origin,
    libDeps: List[JSDependency])

object JSDependencyManifest {

  final val ManifestFileName = "JS_DEPENDENCIES"

  def createIncludeList(manifests: Seq[JSDependencyManifest]): List[String] = {
    val jsDeps = mergeManifests(manifests)

    // Verify all dependencies are met
    for {
      manifest <- manifests
      lib      <- manifest.libDeps
      dep      <- lib.dependencies
      if !jsDeps.contains(dep)
    } yield sys.error(s"The JS dependency ${lib.resourceName} declared " +
        s"from ${manifest.origin} has an unmet transitive dependency $dep")

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
  private def mergeManifests(manifests: Seq[JSDependencyManifest]) = {
    val flatDeps = for {
      manifest <- manifests
      libDep   <- manifest.libDeps
    } yield (libDep, manifest.origin)

    def mergedIncludeAfter(tups: Seq[(JSDependency, Origin)]) = {
      val flat = for {
        (jsdep, origin) <- tups
        includeAfter    <- jsdep.dependencies
      } yield (includeAfter, origin)

      flat.groupBy(_._1).mapValues(_.map(_._2))
    }

    flatDeps.groupBy(_._1.resourceName).mapValues(mergedIncludeAfter)
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

  def write(dep: JSDependencyManifest, writer: VirtualTextFileWriter): Unit = {
    // caller is responsible to close file
    JSONValue.writeJSONString(dep.toJSON, writer.contentWriter)
  }

  def read(file: VirtualTextFile): JSDependencyManifest = {
    val reader = file.reader
    try fromJSON[JSDependencyManifest](JSONValue.parse(reader))
    finally reader.close()
  }

}
