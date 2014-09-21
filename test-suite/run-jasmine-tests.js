window.onload = function() {
  var framework = org.scalajs.jasminetest.JasmineTestFramework();
  framework.setTags("typedarray")

  // Load tests
  // We make sure to use only exported modules (not classes) by checking
  // .prototype of the exporters.
  var testPackage = scala.scalajs.testsuite;
  for (var pName in testPackage) {
    for (var testName in testPackage[pName]) {
      var test = testPackage[pName][testName];
      if (Object.getPrototypeOf(test.prototype) === Object.prototype)
        test(); // this is an exported module, not a class.
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
