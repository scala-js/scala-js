window.onload = function() {
  var framework = org.scalajs.jasminetest.JasmineTestFramework();
  framework.setTags("typedarray")

  // Load tests (we know we only export test modules, so we can use all exports)
  var testPackage = scala.scalajs.testsuite;
  for (var pName in testPackage)
    for (var testName in testPackage[pName])
      testPackage[pName][testName]();

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
