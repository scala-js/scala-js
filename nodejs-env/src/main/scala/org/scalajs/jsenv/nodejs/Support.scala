/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Node.js env       **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2017, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */


package org.scalajs.jsenv.nodejs

import org.scalajs.io._

object Support {
  def fixPercentConsole: VirtualJSFile = {
    new MemVirtualJSFile("nodeConsoleHack.js").withContent(
      """
        |// Hack console log to duplicate double % signs
        |(function() {
        |  function startsWithAnyOf(s, prefixes) {
        |    for (var i = 0; i < prefixes.length; i++) {
        |      // ES5 does not have .startsWith() on strings
        |      if (s.substring(0, prefixes[i].length) === prefixes[i])
        |        return true;
        |    }
        |    return false;
        |  }
        |  var oldLog = console.log;
        |  var newLog = function() {
        |    var args = arguments;
        |    if (args.length >= 1 && args[0] !== void 0 && args[0] !== null) {
        |      var argStr = args[0].toString();
        |      if (args.length > 1)
        |        argStr = argStr.replace(/%/g, "%%");
        |      args[0] = argStr;
        |    }
        |    oldLog.apply(console, args);
        |  };
        |  console.log = newLog;
        |})();
      """.stripMargin
    )
  }
}
