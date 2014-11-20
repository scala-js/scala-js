window.onload = function() {
  var framework = org.scalajs.jasminetest.JasmineTestFramework();
  framework.setTags("typedarray")

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

  framework.clearTags()
};
