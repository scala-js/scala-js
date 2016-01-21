package org.scalajs.sbtplugin

import sbt._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.logging._

import org.scalajs.jsenv._

import scala.collection.mutable

private[sbtplugin] final class FrameworkDetector(jsEnv: JSEnv) {

  import FrameworkDetector._

  /**
   *  Detects which of the test frameworks in `frameworks` exists on
   *  the classpath.
   *
   *  Each potential name in each [[sbt.TestFramework TestFramework]] is checked
   *  for existance (on the JavaScript global namespace, using nested bracket select).
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
        var obj = ${ScalaJSPluginInternal.jsGlobalExpr};
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
            console.log("$ConsoleFrameworkPrefix" + data[i][j]);
            gotOne = true;
            break;
          }
        }
        if (!gotOne) {
          // print an empty line with prefix to zip afterwards
          console.log("$ConsoleFrameworkPrefix");
        }
      }
    """

    val vf = new MemVirtualJSFile("frameworkDetector.js").withContent(code)
    val console = new StoreConsole

    val runner = jsEnv.jsRunner(vf)
    runner.run(NullLogger, console)

    // Filter jsDependencies unexpected output
    val results = console.buf collect {
      case s if s.startsWith(ConsoleFrameworkPrefix) =>
        s.stripPrefix(ConsoleFrameworkPrefix)
    }

    assert(results.size == frameworks.size)

    (frameworks zip results).filter(_._2.nonEmpty).toMap
  }

}

object FrameworkDetector {
  private class StoreConsole extends JSConsole {
    val buf = mutable.Buffer.empty[String]
    def log(msg: Any): Unit = buf += msg.toString
  }

  private val ConsoleFrameworkPrefix = "@scalajs-test-framework-detector:"
}
