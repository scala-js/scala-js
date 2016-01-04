window.onload = function() {
  var context = org.scalajs.jasminetest.TestSuiteContext();
  context.setTags("typedarray", isFullOpt ? "fullopt-stage" : "fastopt-stage");

  // Load tests
  scalajs.TestDetector().loadDetectedTests();

  // Setup and run Jasmine
  var jasmineEnv = jasmine.getEnv();
  var htmlReporter = new jasmine.HtmlReporter();
  jasmineEnv.addReporter(htmlReporter);
  jasmineEnv.specFilter = function(spec) {
    return htmlReporter.specFilter(spec);
  };
  jasmineEnv.execute();
};
