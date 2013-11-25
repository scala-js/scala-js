/*                     __                                               *\
**     ________ ___   / /  ___      __ ____  Scala.js Test Suite        **
**    / __/ __// _ | / /  / _ | __ / // __/  (c) 2013, Jonas Fonseca    **
**  __\ \/ /__/ __ |/ /__/ __ |/_// /_\ \    http://scala-js.org/       **
** /____/\___/_/ |_/____/_/ | |__/ /____/                               **
**                          |/____/                                     **
\*                                                                      */

/**
 * Console-oriented reporter for Rhino. Colorizes output based on the value of
 * the TERM env variable.
 * 
 * Usage:
 * 
 * jasmine.getEnv().addReporter(new jasmine.RhinoReporter());
 * jasmine.getEnv().execute();
 */

(function() {
  if (!jasmine) {
    throw new Exception("jasmine library does not exist in global namespace!");
  }

  var RhinoReporter = function() {
  };

  RhinoReporter.prototype = {
    reportRunnerResults : function(runner) {
      var out = this.failed ? failed : passed
      out("%.0f spec(s), %.0f failure(s) in %.3fs.", this.executed,
          this.failed, (now() - this.started) / 1000);
    },

    reportRunnerStarting : function(runner) {
      this.started = now();
      this.executed = 0;
      this.passed = 0;
      this.failed = 0;
    },

    reportSpecResults : function(spec) {
      var results = spec.results();
      if (results.passed()) {
        passed("  %s", spec.description);
      } else {
        failed("  %s", spec.description);

        for (var i = 0; i < results.getItems().length; i++) {
          var result = results.getItems()[i];

          if (result.type == 'log') {
            print("    %s", result.toString());
          } else if (result.type == 'expect') {
            if (result.passed && result.passed()) {
              print("    ✓ %s", result.message);
            } else {
              print("    ☹ %s", result.message);
              if (result.trace.stack) {
                print("%s", result.trace.stack);
              }
            }
          }
        }
      }
    },

    reportSpecStarting : function(spec) {
      if (this.suite != spec.suite) {
        this.suite = spec.suite;
        suite(this.suite.description);
      }
    },

    reportSuiteResults : function(suite) {
      var results = suite.results();
      if (results.passedCount != results.totalCount)
        failed("%.0f of %.0f failed", results.failedCount, results.totalCount);
      this.passed += results.passedCount;
      this.failed += results.failedCount;
      this.executed += results.totalCount;
      print();
    },

    log : function(str) {
      print("%s", str);
    }
  };

  function now() {
    return (new Date()).getTime();
  }

  function suite(str) {
    print("%s", format(arguments));
  }

  function passed(str) {
    print("%s%s%s", Color.GREEN, format(arguments), Color.RESET);
  }

  function failed(str) {
    print("%s%s%s", Color.RED, format(arguments), Color.RESET);
  }

  function print() {
    var msg = arguments.length > 0 ? format(arguments) : "";
    console.log(msg);
  }

  function format(args) {
    return java.lang.String.format(args[0], args[1], args[2], args[3], args[4],
        args[5], args[6], args[7], args[8], args[9], args[10]);
  }

  var Color = {
    RESET : "\033[m",
    GREEN : "\033[32m",
    RED : "\033[31m",
    BLUE : "\033[34m"
  };

  var ColorTerminals = [ 'xterm' ];

  function isColorTerm() {
    var term = java.lang.System.getenv("TERM");

    for (var i in ColorTerminals) {
      if (term == ColorTerminals[i]) {
        return true;      
      }
    }
    return false;
  }

  if (!isColorTerm()) {
    for (var i in Color) {
      Color[i] = '';
    }
  }

  jasmine.RhinoReporter = RhinoReporter;
})();
