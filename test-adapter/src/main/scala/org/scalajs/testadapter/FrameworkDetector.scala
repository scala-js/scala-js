package org.scalajs.testadapter

import sbt.testing._

import org.scalajs.core.tools.io._
import org.scalajs.core.tools.logging.Logger
import org.scalajs.core.tools.linker.ModuleKind

import org.scalajs.jsenv._

import org.scalajs.testadapter.json._

import scala.collection.mutable

object FrameworkDetector {

  /** Detects which of the specified test frameworks exists.
   *
   *  Each potential implementation name in `frameworksAndTheirImplNames` is
   *  checked for existence in the module specified by `moduleKind` and
   *  `moduleIdentifier`.
   *
   *  Returns a map with found frameworks and their corresponding
   *  `sbt.testing.Framework`s.
   */
  def detectFrameworks[TestFramework](jsEnv: ComJSEnv,
      jsFiles: Seq[VirtualJSFile], moduleKind: ModuleKind,
      moduleIdentifier: Option[String],
      frameworksAndTheirImplNames: Seq[(TestFramework, List[String])],
      logger: Logger): Map[TestFramework, Framework] = {
    detectImplementationNames(jsEnv, jsFiles, moduleKind, moduleIdentifier,
        frameworksAndTheirImplNames, logger).map {
      case (framework, implName) =>
        (framework, new ScalaJSFramework(implName, jsEnv, jsFiles, moduleKind,
            moduleIdentifier, logger))
    }
  }

  /** Detects which of the specified test frameworks exists.
   *
   *  Each potential implementation name in `frameworksAndTheirImplNames` is
   *  checked for existence in the module specified by `moduleKind` and
   *  `moduleIdentifier`.
   *
   *  Returns a map with found frameworks and their corresponding
   *  implementation names.
   */
  def detectImplementationNames[TestFramework](jsEnv: JSEnv,
      jsFiles: Seq[VirtualJSFile], moduleKind: ModuleKind,
      moduleIdentifier: Option[String],
      frameworksAndTheirImplNames: Seq[(TestFramework, List[String])],
      logger: Logger): Map[TestFramework, String] = {

    val (frameworks, implementationNames) = frameworksAndTheirImplNames.unzip

    val prefix = ScalaJSFramework.optionalExportsNamespacePrefix(
        moduleKind, moduleIdentifier)

    val code = s"""
      (function() {
        "use strict";

        /* #2752: if there is no testing framework at all on the classpath,
         * the testing interface will not be there, and therefore the
         * `detectFrameworks` function will not exist. We must therefore be
         * careful when selecting it.
         */
        var orgNS = ${
          if (prefix == "") "typeof org !== 'undefined' ? org : (void 0)"
          else (prefix + "org")
        };
        var detectFrameworksFun = (
          orgNS &&
          orgNS.scalajs &&
          orgNS.scalajs.testinterface &&
          orgNS.scalajs.testinterface.internal &&
          orgNS.scalajs.testinterface.internal.detectFrameworks
        );
        detectFrameworksFun = detectFrameworksFun || (function(data) {
          var results = [];
          for (var i = 0; i < data.length; ++i)
            results.push(void 0);
          return results;
        });

        var data = ${jsonToString(implementationNames.toList.toJSON)};
        var results = detectFrameworksFun(data);
        for (var i = 0; i < results.length; ++i) {
          console.log("$ConsoleFrameworkPrefix" + (results[i] || ""));
        }
      })();
    """

    val vf = new MemVirtualJSFile("frameworkDetector.js").withContent(code)
    val console = new StoreConsole

    val runner = jsEnv.jsRunner(jsFiles :+ vf)
    runner.run(logger, console)

    // Filter jsDependencies unexpected output
    val results = console.buf collect {
      case s if s.startsWith(ConsoleFrameworkPrefix) =>
        s.stripPrefix(ConsoleFrameworkPrefix)
    }

    assert(results.size == frameworks.size)

    frameworks.zip(results).filter(_._2.nonEmpty).toMap
  }

  private class StoreConsole extends JSConsole {
    val buf = mutable.Buffer.empty[String]
    def log(msg: Any): Unit = buf += msg.toString
  }

  private val ConsoleFrameworkPrefix = "@scalajs-test-framework-detector:"
}
