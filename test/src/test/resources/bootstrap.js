/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, LAMP/EPFL        **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

/**
 * This file is loaded first and used for patching the JS environment.
 */

function scalaJSStub(name) {
  return function() {
    console.log("Stub for " + name + " called");
  }
};

/* Stub-out timer methods used by Jasmine and not provided by Rhino. */
if (typeof setTimeout == 'undefined') {
  var setTimeout = scalaJSStub('setTimeout');
  var clearTimeout = scalaJSStub('clearTimeout');
  var setInterval = scalaJSStub('setInterval');
  var clearInterval = scalaJSStub('clearInterval');
}
