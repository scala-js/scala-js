// If not a PR, this is a long-lived branch, which should have a nightly build
def triggers = []
if (!env.CHANGE_ID) {
  // This is the 1.x series: run nightly from Sunday to Friday
  triggers << cron('H H(0-2) * * 0-5')
}

// Setup properties of this job definition
properties([
  parameters([
    string(name: 'matrix', defaultValue: 'auto', description: 'The matrix to build (auto, quick, full)')
  ]),
  pipelineTriggers(triggers)
])

// Check whether the job was started by a timer
// See https://hopstorawpointers.blogspot.ch/2016/10/performing-nightly-build-steps-with.html
@NonCPS
def isJobStartedByTimer() {
  def startedByTimer = false
  def buildCauses = currentBuild.rawBuild.getCauses()
  for (buildCause in buildCauses) {
    if (buildCause != null) {
      def causeDescription = buildCause.getShortDescription()
      echo "shortDescription: ${causeDescription}"
      if (causeDescription.contains("Started by timer")) {
        startedByTimer = true
      }
    }
  }
  return startedByTimer
}
def startedByTimer = isJobStartedByTimer()

// Auto-select a matrix if it was not explicitly specified
def selectedMatrix = params.matrix
if (selectedMatrix == 'auto') {
  def reason = ''
  if (env.CHANGE_ID) {
    reason = "is a PR ${env.CHANGE_ID}"
    selectedMatrix = 'quick'
  } else {
    reason = "is not a PR, startedByTimer = $startedByTimer"
    if (startedByTimer) {
      selectedMatrix = 'full'
    } else {
      selectedMatrix = 'quick'
    }
  }
  echo("Auto-selected matrix: $selectedMatrix ($reason)")
} else {
  echo("Explicit matrix: $selectedMatrix")
}

def CIScriptPrelude = '''
LOCAL_HOME="/localhome/jenkins"
LOC_SBT_BASE="$LOCAL_HOME/scala-js-sbt-homes"
LOC_SBT_BOOT="$LOC_SBT_BASE/sbt-boot"
LOC_SBT_HOME="$LOC_SBT_BASE/sbt-home"

export SBT_OPTS="-J-Xmx5G -J-XX:MaxPermSize=512M -Dsbt.boot.directory=$LOC_SBT_BOOT -Dsbt.ivy.home=$LOC_SBT_HOME -Divy.home=$LOC_SBT_HOME -Dsbt.global.base=$LOC_SBT_BASE"

export NODE_PATH="$HOME/node_modules/"

# Define setJavaVersion

setJavaVersion() {
  export JAVA_HOME=$HOME/apps/java-$1
  export PATH=$JAVA_HOME/bin:$PATH
}

# Define sbtretry

sbtretry() {
  local TIMEOUT=45m
  echo "RUNNING timeout -k 5 $TIMEOUT sbt" "$@"
  timeout -k 5 $TIMEOUT sbt $SBT_OPTS "$@"
  local CODE=$?
  if [ "$CODE" -eq 124 ]; then
    echo "TIMEOUT after" $TIMEOUT
  fi
  if [ "$CODE" -ne 0 ]; then
    echo "RETRYING timeout -k 5 $TIMEOUT sbt" "$@"
    timeout -k 5 $TIMEOUT sbt $SBT_OPTS "$@"
    CODE=$?
    if [ "$CODE" -eq 124 ]; then
      echo "TIMEOUT after" $TIMEOUT
    fi
    if [ "$CODE" -ne 0 ]; then
      echo "FAILED TWICE"
      return $CODE
    fi
  fi
}
'''

def Tasks = [
  "main": '''
    setJavaVersion $java
    npm install &&
    sbtretry ++$scala helloworld/run &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry 'set scalaJSLinkerConfig in helloworld ~= (_.withOptimizer(false))' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry 'set scalaJSLinkerConfig in helloworld ~= (_.withSemantics(_.withAsInstanceOfs(CheckedBehavior.Unchecked)))' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        helloworld/run \
        helloworld/clean &&
    sbtretry ++$scala \
        'set artifactPath in (helloworld, Compile, fastOptJS) := (crossTarget in helloworld).value / "helloworld-fastopt.mjs"' \
        'set jsEnv in helloworld := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSLinkerConfig in helloworld ~= (_.withModuleKind(ModuleKind.ESModule))' \
        helloworld/run &&
    sbtretry ++$scala \
        'set artifactPath in (helloworld, Compile, fullOptJS) := (crossTarget in helloworld).value / "helloworld-opt.mjs"' \
        'set jsEnv in helloworld := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSLinkerConfig in helloworld ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        helloworld/run \
        helloworld/clean &&
    sbtretry ++$scala testingExample/testHtml &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testingExample/testHtml \
        testingExample/clean &&
    sbtretry ++$scala testSuiteJVM/test testSuiteJVM/clean &&
    sbtretry ++$scala testSuite/test &&
    sbtretry ++$scala testSuiteEx/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testSuiteEx/test &&
    sbtretry ++$scala testSuite/test:doc library/test compiler/test reversi/fastOptJS reversi/fullOptJS &&
    sbtretry ++$scala compiler/compile:doc library/compile:doc \
        testInterface/compile:doc &&
    sbtretry ++$scala headerCheck &&
    sbtretry ++$scala partest/fetchScalaSource &&
    sbtretry ++$scala library/mimaReportBinaryIssues testInterface/mimaReportBinaryIssues &&
    sh ci/checksizes.sh $scala &&
    sh ci/check-partest-coverage.sh $scala
  ''',

  "test-suite-ecma-script5": '''
    setJavaVersion $java
    npm install &&
    sbtretry ++$scala jUnitTestOutputsJVM/test jUnitTestOutputsJS/test \
        'set scalaJSStage in Global := FullOptStage' jUnitTestOutputsJS/test &&
    sbtretry ++$scala $testSuite/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withAllowBigIntsForLongs(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--harmony-bigint")))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withAllowBigIntsForLongs(true)).withOptimizer(false))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--harmony-bigint")))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalacOptions in $testSuite += "-Xexperimental"' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalacOptions in $testSuite += "-Xexperimental"' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set artifactPath in ($testSuite, Test, fastOptJS) := (crossTarget in $testSuite).value / "testsuite-fastopt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set MyScalaJSPlugin.wantSourceMaps in $testSuite := false' \
        ++$scala $testSuite/test &&
    sbtretry 'set artifactPath in ($testSuite, Test, fullOptJS) := (crossTarget in $testSuite).value / "testsuite-opt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set MyScalaJSPlugin.wantSourceMaps in $testSuite := false' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test
  ''',

  "test-suite-ecma-script5-force-polyfills": '''
    setJavaVersion $java
    npm install &&
    sbtretry 'set jsEnv in $testSuite := new NodeJSEnvForcePolyfills()' \
        ++$scala $testSuite/test &&
    sbtretry 'set jsEnv in $testSuite := new NodeJSEnvForcePolyfills()' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set jsEnv in $testSuite := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set jsEnv in $testSuite := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        ++$scala $testSuite/test &&
    sbtretry 'set jsEnv in $testSuite := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set jsEnv in $testSuite := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean
  ''',

  "test-suite-ecma-script6": '''
    setJavaVersion $java
    npm install &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true).withAllowBigIntsForLongs(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--harmony-bigint")))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true).withAllowBigIntsForLongs(true)).withOptimizer(false))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--harmony-bigint")))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set artifactPath in ($testSuite, Test, fastOptJS) := (crossTarget in $testSuite).value / "testsuite-fastopt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.ESModule))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set artifactPath in ($testSuite, Test, fullOptJS) := (crossTarget in $testSuite).value / "testsuite-opt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test
  ''',

  /* For the bootstrap tests to be able to call
   * `testSuite/test:fastOptJS`, `scalaJSStage in testSuite` must be
   * `FastOptStage`, even when `scalaJSStage in Global` is `FullOptStage`.
   */
  "bootstrap": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala linker/test &&
    sbt ++$scala irJS/test ioJS/test linkerJS/test &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite := FastOptStage' \
        ++$scala irJS/test ioJS/test linkerJS/test &&
    sbt ++$scala testSuite/bootstrap:test &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite := FastOptStage' \
        ++$scala testSuite/bootstrap:test &&
    sbt ++$scala irJS/mimaReportBinaryIssues ioJS/mimaReportBinaryIssues \
        loggingJS/mimaReportBinaryIssues linkerJS/mimaReportBinaryIssues
  ''',

  "tools": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala ir/test io/test logging/compile linker/compile \
        jsEnvs/test nodeJSEnv/test testAdapter/test \
        ir/mimaReportBinaryIssues io/mimaReportBinaryIssues \
        logging/mimaReportBinaryIssues linker/mimaReportBinaryIssues \
        jsEnvs/mimaReportBinaryIssues jsEnvsTestKit/mimaReportBinaryIssues \
        nodeJSEnv/mimaReportBinaryIssues \
        testAdapter/mimaReportBinaryIssues &&
    sbt ++$scala ir/compile:doc io/compile:doc logging/compile:doc \
        linker/compile:doc jsEnvs/compile:doc \
        jsEnvsTestKit/compile:doc nodeJSEnv/compile:doc \
        testAdapter/compile:doc
  ''',

  "tools-sbtplugin": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala ir/test io/test logging/compile linker/compile \
        jsEnvs/test nodeJSEnv/test testAdapter/test \
        sbtPlugin/package \
        ir/mimaReportBinaryIssues io/mimaReportBinaryIssues \
        logging/mimaReportBinaryIssues linker/mimaReportBinaryIssues \
        jsEnvs/mimaReportBinaryIssues jsEnvsTestKit/mimaReportBinaryIssues \
        nodeJSEnv/mimaReportBinaryIssues \
        testAdapter/mimaReportBinaryIssues \
        sbtPlugin/mimaReportBinaryIssues &&
    sbt ++$scala library/scalastyle javalanglib/scalastyle javalib/scalastyle \
        ir/scalastyle compiler/scalastyle \
        compiler/test:scalastyle io/scalastyle io/test:scalastyle \
        logging/scalastyle logging/test:scalastyle \
        linker/scalastyle linker/test:scalastyle \
        jsEnvs/scalastyle jsEnvsTestKit/scalastyle nodeJSEnv/scalastyle \
        jsEnvs/test:scalastyle nodeJSEnv/test:scalastyle testAdapter/scalastyle \
        sbtPlugin/scalastyle testInterface/scalastyle \
        testSuite/scalastyle testSuite/test:scalastyle \
        testSuiteJVM/test:scalastyle \
        testSuiteEx/test:scalastyle helloworld/scalastyle \
        reversi/scalastyle testingExample/scalastyle \
        testingExample/test:scalastyle \
        jUnitPlugin/scalastyle jUnitRuntime/scalastyle \
        jUnitTestOutputsJVM/scalastyle jUnitTestOutputsJVM/test:scalastyle \
        jUnitTestOutputsJS/scalastyle jUnitTestOutputsJS/test:scalastyle &&
    sbt ++$scala ir/compile:doc io/compile:doc logging/compile:doc \
        linker/compile:doc jsEnvs/compile:doc \
        jsEnvsTestKit/compile:doc nodeJSEnv/compile:doc \
        testAdapter/compile:doc \
        sbtPlugin/compile:doc
  ''',

  "partestc": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala partest/compile
  ''',

  "sbtplugin-test": '''
    setJavaVersion 1.8
    SBT_VER_OVERRIDE=$sbt_version_override
    # Publish Scala.js artifacts locally
    # Then go into standalone project and test
    npm install &&
    sbt ++2.11.12 compiler/publishLocal library/publishLocal \
                  testInterface/publishLocal \
                  jUnitPlugin/publishLocal jUnitRuntime/publishLocal &&
    sbt ++$toolsscala ${SBT_VER_OVERRIDE:+^^$SBT_VER_OVERRIDE} \
        ir/publishLocal io/publishLocal logging/publishLocal \
        linker/publishLocal jsEnvs/publishLocal \
        nodeJSEnv/publishLocal testAdapter/publishLocal \
        sbtPlugin/publishLocal &&
    cd sbt-plugin-test &&
    setJavaVersion $java &&
    if [ -n "$SBT_VER_OVERRIDE" ]; then echo "sbt.version=$SBT_VER_OVERRIDE" > ./project/build.properties; fi &&
    sbt noDOM/run \
        noDOM/testHtml multiTestJS/testHtml \
        test \
        noDOM/testScalaJSModuleInitializers \
        noDOM/clean noDOM/concurrentUseOfLinkerTest \
        multiTestJS/test:testScalaJSSourceMapAttribute &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        noDOM/testHtml multiTestJS/testHtml
  ''',

  "partest-noopt": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala package "partestSuite/testOnly -- --showDiff"
  ''',

  "partest-fastopt": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala package "partestSuite/testOnly -- --fastOpt --showDiff"
  ''',

  "partest-fullopt": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala package "partestSuite/testOnly -- --fullOpt --showDiff"
  '''
]

def mainJavaVersion = "1.8"
def otherJavaVersions = []
def allJavaVersions = otherJavaVersions.clone()
allJavaVersions << mainJavaVersion

def mainScalaVersion = "2.12.6"
def mainScalaVersions = ["2.11.12", "2.12.6"]
def otherScalaVersions = [
  "2.11.0",
  "2.11.1",
  "2.11.2",
  "2.11.4",
  "2.11.5",
  "2.11.6",
  "2.11.7",
  "2.11.8",
  "2.11.11",
  "2.11.12",
  "2.12.1",
  "2.12.2",
  "2.12.3",
  "2.12.4",
  "2.12.5"
]
def noToolsScalaVersions = ["2.13.0-M5"]

// The 'quick' matrix
def quickMatrix = []
mainScalaVersions.each { scalaVersion ->
  allJavaVersions.each { javaVersion ->
    quickMatrix.add([task: "main", scala: scalaVersion, java: javaVersion])
  }
  quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
  quickMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
  quickMatrix.add([task: "bootstrap", scala: scalaVersion, java: mainJavaVersion])
  quickMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion])
}
quickMatrix.add([task: "test-suite-ecma-script5-force-polyfills", scala: mainScalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
noToolsScalaVersions.each { scalaVersion ->
  quickMatrix.add([task: "main", scala: scalaVersion, java: mainJavaVersion])
  quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
}
allJavaVersions.each { javaVersion ->
  quickMatrix.add([task: "tools-sbtplugin", scala: "2.10.7", java: javaVersion])
  quickMatrix.add([task: "tools", scala: "2.11.12", java: javaVersion])
  quickMatrix.add([task: "tools", scala: "2.12.4", java: javaVersion])
}
quickMatrix.add([task: "partestc", scala: "2.11.0", java: mainJavaVersion])
quickMatrix.add([task: "sbtplugin-test", toolsscala: "2.10.7", sbt_version_override: "", java: mainJavaVersion])
quickMatrix.add([task: "sbtplugin-test", toolsscala: "2.12.6", sbt_version_override: "1.0.0", java: mainJavaVersion])

// The 'full' matrix
def fullMatrix = quickMatrix.clone()
otherScalaVersions.each { scalaVersion ->
  fullMatrix.add([task: "main", scala: scalaVersion, java: mainJavaVersion])
}
mainScalaVersions.each { scalaVersion ->
  otherJavaVersions.each { javaVersion ->
    quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: javaVersion, testSuite: "testSuite"])
    quickMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: javaVersion, testSuite: "testSuite"])
  }
  fullMatrix.add([task: "partest-noopt", scala: scalaVersion, java: mainJavaVersion])
  fullMatrix.add([task: "partest-fullopt", scala: scalaVersion, java: mainJavaVersion])
}
otherScalaVersions.each { scalaVersion ->
  // Partest does not compile on Scala 2.11.4 (see #1215).
  if (scalaVersion != "2.11.4") {
    fullMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion])
  }
}

def Matrices = [
  quick: quickMatrix,
  full: fullMatrix
]

if (!Matrices.containsKey(selectedMatrix)) {
  error("Nonexistent matrix '$selectedMatrix'")
}
def matrix = Matrices[selectedMatrix]

buildDefs = [:]
matrix.each { taskDef ->
  def taskName = taskDef.task
  if (!Tasks.containsKey(taskName)) {
    error("Nonexistent task '$taskName'")
  }
  def taskStr = Tasks[taskName]
  def fullTaskName = taskName

  taskDef.each { name, value ->
    if (name != 'task') {
      taskStr = taskStr.replace('$' + name, value)
      fullTaskName += " $name=$value"
    }
  }

  def ciScript = CIScriptPrelude + taskStr

  buildDefs.put(fullTaskName, {
    node('linuxworker') {
      checkout scm
      sh "git clean -fdx && rm -rf partest/fetchedSources/"
      writeFile file: 'ciscript.sh', text: ciScript, encoding: 'UTF-8'
      retry(2) {
        timeout(time: 4, unit: 'HOURS') {
          sh "echo '$fullTaskName' && cat ciscript.sh && sh ciscript.sh"
        }
      }
    }
  })
}

ansiColor('xterm') {
  stage('Test') {
    parallel(buildDefs)
  }
}
