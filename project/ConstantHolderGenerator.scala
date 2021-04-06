package build

import sbt._

import org.scalajs.ir.ScalaJSVersions

import org.scalajs.jsenv.JSUtils.escapeJS

object ConstantHolderGenerator {
  /** Generate a *.scala file that contains the given values as literals. */
  def generate(dir: File, fqn: String, values: (String, Any)*): Seq[File] = {
    val (fullPkg@(_ :+ pkg)) :+ objectName = fqn.split('.').toSeq

    val out = dir / (objectName + ".scala")

    val defs = for {
      (name, value) <- values
    } yield {
      s"val $name = ${literal(value)}"
    }

    val scalaCode =
      s"""
      package ${fullPkg.mkString(".")}

      private[$pkg] object $objectName {
        ${defs.mkString("\n")}
      }
      """

    IO.write(out, scalaCode)

    Seq(out)
  }

  private final def literal(v: Any): String = v match {
    case s: String  => "\"" + escapeJS(s) + "\"" // abuse escapeJS to escape Scala
    case b: Boolean => b.toString
    case i: Int     => i.toString
    case f: File    => literal(f.getAbsolutePath)

    case m: Map[_, _] =>
      m.map(x => literal(x._1) + " -> " + literal(x._2))
        .mkString("Map(", ", ", ")")

    case _ =>
      throw new IllegalArgumentException(
          "Unsupported value type: " + v.getClass)
  }
}
