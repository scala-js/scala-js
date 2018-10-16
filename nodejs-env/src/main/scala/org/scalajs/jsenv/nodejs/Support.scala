/*
 * Scala.js (https://www.scala-js.org/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package org.scalajs.jsenv.nodejs

import org.scalajs.io._

object Support {
  def fixPercentConsole: VirtualBinaryFile = {
    MemVirtualBinaryFile.fromStringUTF8("nodeConsoleHack.js",
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
