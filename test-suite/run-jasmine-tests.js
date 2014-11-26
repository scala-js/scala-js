window.onload = function() {
  var framework = scala.scalajs.test.JasmineTestFramework();
  framework.setTags("typedarray")

  // Load tests (we know we only export test modules, so we can use all exports)
  var testPackage = scala.scalajs.test;
  for (var pName in testPackage) {
    for (var testName in testPackage[pName]) {
      if (!(pName == "internal" && testName == "ConsoleTestOutput")) {
        var test = testPackage[pName][testName];
        if (Object.getPrototypeOf(test.prototype) === Object.prototype)
          test();
      }
    }
  }

  // Setup and run Jasmine
  var jasmineEnv = jasmine.getEnv();
  var htmlReporter = new jasmine.HtmlReporter();
  jasmineEnv.addReporter(htmlReporter);
  jasmineEnv.specFilter = function(spec) {
    return htmlReporter.specFilter(spec);
  };
  jasmineEnv.execute();

  framework.clearTags()
};
