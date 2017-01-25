package org.scalajs.sbtplugin

import sbt._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.json._
import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.linker.backend.ModuleKind

import org.scalajs.jsenv._

import scala.collection.mutable

private[sbtplugin] final class FrameworkDetector(jsEnv: JSEnv,
    moduleKind: ModuleKind, moduleIdentifier: Option[String]) {

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
  def detect(frameworks: Seq[TestFramework],
      logger: Logger): Map[TestFramework, String] = {
    val data = frameworks.map(_.implClassNames.toList).toList.toJSON

    val exportsNamespaceExpr =
      ScalaJSPluginInternal.makeExportsNamespaceExpr(moduleKind, moduleIdentifier)

    val code = s"""
      (function(exportsNamespace) {
        "use strict";

        /* #2752: if there is no testing framework at all on the classpath,
         * the testing interface will not be there, and therefore the
         * `detectFrameworks` function will not exist. We must therefore be
         * careful when selecting it.
         */
        var namespace = exportsNamespace;
        namespace = namespace.org || {};
        namespace = namespace.scalajs || {};
        namespace = namespace.testinterface || {};
        namespace = namespace.internal || {};
        var detectFrameworksFun = namespace.detectFrameworks || (function(data) {
          var results = [];
          for (var i = 0; i < data.length; ++i)
            results.push(void 0);
          return results;
        });

        var data = ${jsonToString(data)};
        var results = detectFrameworksFun(data);
        for (var i = 0; i < results.length; ++i) {
          console.log("$ConsoleFrameworkPrefix" + (results[i] || ""));
        }
      })($exportsNamespaceExpr);
    """

    val vf = new MemVirtualJSFile("frameworkDetector.js").withContent(code)
    val console = new StoreConsole

    val runner = jsEnv.jsRunner(vf)
    runner.run(logger, console)

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
