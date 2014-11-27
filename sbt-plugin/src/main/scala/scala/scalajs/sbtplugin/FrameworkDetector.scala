package org.scalajs.sbtplugin

import sbt._

import org.scalajs.core.tools.classpath._
import org.scalajs.core.tools.io._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.logging._
import org.scalajs.core.tools.corelib.CoreJSLibs

import org.scalajs.jsenv._

import scala.collection.mutable

final class FrameworkDetector(jsEnv: JSEnv, classpath: CompleteClasspath) {

  /**
   *  Detects which of the test frameworks in [[frameworks]] exists on
   *  the classpath.
   *
   *  Each potential name in each [[TestFramework]] is checked for existance
   *  (on the JavaScript global namespace, using nested bracket select).
   *
   *  Returns a map with found frameworks and the first name with an existing
   *  definition on the classpath.
   *
   *  Note: No JavaScript type tests are performed by this method
   */
  def detect(frameworks: Seq[TestFramework]): Map[TestFramework, String] = {
    val data = frameworks.map(_.implClassNames.toList).toList.toJSON

    val code = s"""
      var data = ${jsonToString(data)};

      function frameworkExists(name) {
        var parts = name.split(".");
        var obj = ${CoreJSLibs.jsGlobalExpr};
        for (var i = 0; i < parts.length; ++i) {
          obj = obj[parts[i]];
          if (obj === void 0)
            return false;
        }
        return true;
      }

      for (var i = 0; i < data.length; ++i) {
        var gotOne = false;
        for (var j = 0; j < data[i].length; ++j) {
          if (frameworkExists(data[i][j])) {
            console.log(data[i][j]);
            gotOne = true;
            break;
          }
        }
        if (!gotOne)
          console.log(""); // print an empty line to zip afterwards
      }
    """

    val vf = new MemVirtualJSFile("frameworkDetector.js").withContent(code)
    val console = new FrameworkDetector.StoreConsole

    val runner = jsEnv.jsRunner(classpath, vf, NullLogger, console)
    runner.run()

    val results = console.buf.toList

    assert(results.size == frameworks.size)

    (frameworks zip results).filter(_._2.nonEmpty).toMap
  }

}

object FrameworkDetector {
  private class StoreConsole extends JSConsole {
    val buf = mutable.Buffer.empty[String]
    def log(msg: Any): Unit = buf += msg.toString
  }
}
