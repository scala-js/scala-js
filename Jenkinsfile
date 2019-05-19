// If not a PR, this is a long-lived branch, which should have a nightly build
def triggers = []
if (!env.CHANGE_ID) {
  // This is the 0.6.x series: run weekly on Saturday
  triggers << cron('H H(0-2) * * 6')
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
  local TIMEOUT=35m
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
    sbtretry ++$scala 'set scalaJSUseRhino in Global := true' helloworld/run &&
    sbtretry ++$scala helloworld/run &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry 'set requiresDOM in helloworld := true' \
        ++$scala helloworld/run &&
    sbtretry 'set requiresDOM in helloworld := true' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry 'set scalaJSOptimizerOptions in helloworld ~= (_.withDisableOptimizer(true))' \
        'set scalaJSUseRhino in Global := true' \
        ++$scala helloworld/run &&
    sbtretry 'set scalaJSOptimizerOptions in helloworld ~= (_.withDisableOptimizer(true))' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry 'set scalaJSSemantics in helloworld ~= (_.withAsInstanceOfs(org.scalajs.core.tools.sem.CheckedBehavior.Unchecked))' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry 'set inScope(ThisScope in helloworld)(jsEnv := new org.scalajs.jsenv.RetryingComJSEnv(PhantomJSEnv().value))' \
        ++$scala helloworld/run &&
    sbtretry 'set inScope(ThisScope in helloworld)(jsEnv := new org.scalajs.jsenv.RetryingComJSEnv(PhantomJSEnv().value))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala helloworld/run \
        helloworld/clean &&
    sbtretry ++$scala \
        'set scalaJSModuleKind in helloworld := ModuleKind.CommonJSModule' \
        helloworld/run \
        helloworld/clean &&
    sbtretry ++$scala \
        'set artifactPath in (helloworld, Compile, fastOptJS) := (crossTarget in helloworld).value / "helloworld-fastopt.mjs"' \
        'set jsEnv in helloworld := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSModuleKind in helloworld := ModuleKind.ESModule' \
        helloworld/run &&
    sbtretry ++$scala \
        'set artifactPath in (helloworld, Compile, fullOptJS) := (crossTarget in helloworld).value / "helloworld-opt.mjs"' \
        'set jsEnv in helloworld := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSModuleKind in helloworld := ModuleKind.ESModule' \
        'set scalaJSStage in Global := FullOptStage' \
        helloworld/run \
        helloworld/clean &&
    sbtretry ++$scala \
        'set Seq(scalaJSUseMainModuleInitializer in helloworld := false, persistLauncher in helloworld := true)' \
        'set scalaJSUseRhino in Global := true' \
        helloworld/run &&
    sbtretry ++$scala \
        'set Seq(scalaJSUseMainModuleInitializer in helloworld := false, persistLauncher in helloworld := true)' \
        helloworld/run \
        helloworld/clean &&
    sbtretry 'set scalaJSUseRhino in Global := true' \
        ++$scala testingExample/test:run testingExample/test &&
    sbtretry ++$scala testingExample/test:run testingExample/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testingExample/test:run testingExample/test \
        testingExample/clean &&
    sbtretry 'set inScope(ThisScope in testingExample)(jsEnv := new org.scalajs.jsenv.RetryingComJSEnv(PhantomJSEnv().value))' \
        ++$scala testingExample/test:run testingExample/test &&
    sbtretry 'set inScope(ThisScope in testingExample)(jsEnv := new org.scalajs.jsenv.RetryingComJSEnv(PhantomJSEnv().value))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala testingExample/test:run testingExample/test \
        testingExample/clean &&
    sbtretry 'set scalaJSOptimizerOptions in testingExample ~= (_.withDisableOptimizer(true))' \
        ++$scala testingExample/test:run testingExample/test &&
    sbtretry 'set inScope(ThisScope in testingExample)(jsEnv := new org.scalajs.jsenv.RetryingComJSEnv(PhantomJSEnv().value))' \
        'set scalaJSOptimizerOptions in testingExample ~= (_.withDisableOptimizer(true))' \
        ++$scala testingExample/test:run testingExample/test &&
    sbtretry ++$scala library/test &&
    sbtretry ++$scala testSuiteJVM/test testSuiteJVM/clean &&
    sbtretry ++$scala 'testSuite/test:runMain org.scalajs.testsuite.junit.JUnitBootstrapTest' &&
    sbtretry ++$scala testSuite/test &&
    sbtretry ++$scala javalibExTestSuite/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala javalibExTestSuite/test &&
    sbtretry ++$scala testSuite/test:doc compiler/test reversi/fastOptJS reversi/fullOptJS &&
    sbtretry ++$scala compiler/compile:doc library/compile:doc javalibEx/compile:doc \
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
    sbtretry ++$scala 'set scalaJSUseRhino in Global := true' jUnitTestOutputsJS/test &&
    sbtretry ++$scala 'set scalaJSUseRhino in Global := true' $testSuite/test &&
    sbtretry ++$scala $testSuite/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set requiresDOM in $testSuite := true' \
        ++$scala $testSuite/test &&
    sbtretry 'set requiresDOM in $testSuite := true' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry ++$scala 'set scalaJSUseRhino in Global := true' noIrCheckTest/test &&
    sbtretry ++$scala noIrCheckTest/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala noIrCheckTest/test \
        noIrCheckTest/clean &&
    sbtretry 'set scalaJSOptimizerOptions in $testSuite ~= (_.withDisableOptimizer(true))' \
        'set scalaJSUseRhino in Global := true' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSOptimizerOptions in $testSuite ~= (_.withDisableOptimizer(true))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        'set scalaJSUseRhino in Global := true' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        'set scalaJSOptimizerOptions in $testSuite ~= (_.withDisableOptimizer(true))' \
        'set scalaJSUseRhino in Global := true' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        'set scalaJSOptimizerOptions in $testSuite ~= (_.withDisableOptimizer(true))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set inScope(ThisScope in $testSuite)(jsEnv := new org.scalajs.jsenv.RetryingComJSEnv(PhantomJSEnv().value))' \
        'set parallelExecution in ($testSuite, Test) := false' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalacOptions in $testSuite += "-Xexperimental"' \
        'set scalaJSUseRhino in Global := true' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalacOptions in $testSuite += "-Xexperimental"' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalacOptions in $testSuite += "-Xexperimental"' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSModuleKind in $testSuite := ModuleKind.CommonJSModule' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSModuleKind in $testSuite := ModuleKind.CommonJSModule' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set artifactPath in ($testSuite, Test, fastOptJS) := (crossTarget in $testSuite).value / "testsuite-fastopt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSModuleKind in $testSuite := ModuleKind.ESModule' \
        ++$scala $testSuite/test &&
    sbtretry 'set artifactPath in ($testSuite, Test, fullOptJS) := (crossTarget in $testSuite).value / "testsuite-opt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSModuleKind in $testSuite := ModuleKind.ESModule' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test
  ''',

  "test-suite-ecma-script6": '''
    setJavaVersion $java
    npm install &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in noIrCheckTest := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        ++$scala noIrCheckTest/test \
        noIrCheckTest/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSOptimizerOptions in $testSuite ~= (_.withDisableOptimizer(true))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSOptimizerOptions in $testSuite ~= (_.withDisableOptimizer(true))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSSemantics in $testSuite ~= makeCompliant' \
        'set scalaJSOptimizerOptions in $testSuite ~= (_.withDisableOptimizer(true))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSModuleKind in $testSuite := ModuleKind.CommonJSModule' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSModuleKind in $testSuite := ModuleKind.CommonJSModule' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set artifactPath in ($testSuite, Test, fastOptJS) := (crossTarget in $testSuite).value / "testsuite-fastopt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSModuleKind in $testSuite := ModuleKind.ESModule' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withESFeatures(_.withUseECMAScript2015(true)))' \
        'set artifactPath in ($testSuite, Test, fullOptJS) := (crossTarget in $testSuite).value / "testsuite-opt.mjs"' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withArgs(List("--experimental-modules")).withSourceMap(false))' \
        'set scalaJSModuleKind in $testSuite := ModuleKind.ESModule' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test
  ''',

  "bootstrap": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala irJS/test toolsJS/test &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        ++$scala irJS/test &&
    sbt ++$scala testSuite/test:fullOptJS &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        ++$scala toolsJS/test
  ''',

  "tools-cli-stubs": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala tools/package ir/test tools/test cli/package cli/assembly \
        stubs/package jsEnvsTestSuite/test testAdapter/test \
        ir/mimaReportBinaryIssues tools/mimaReportBinaryIssues \
        jsEnvs/mimaReportBinaryIssues jsEnvsTestKit/mimaReportBinaryIssues \
        testAdapter/mimaReportBinaryIssues \
        stubs/mimaReportBinaryIssues cli/mimaReportBinaryIssues \
        irJS/mimaReportBinaryIssues toolsJS/mimaReportBinaryIssues &&
    sbt ++$scala ir/compile:doc tools/compile:doc jsEnvs/compile:doc \
        jsEnvsTestKit/compile:doc testAdapter/compile:doc stubs/compile:doc
  ''',

  "tools-cli-stubs-sbtplugin": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala tools/package ir/test tools/test cli/package cli/assembly \
        stubs/package jsEnvsTestSuite/test testAdapter/test \
        sbtPlugin/package \
        ir/mimaReportBinaryIssues tools/mimaReportBinaryIssues \
        jsEnvs/mimaReportBinaryIssues jsEnvsTestKit/mimaReportBinaryIssues \
        testAdapter/mimaReportBinaryIssues \
        stubs/mimaReportBinaryIssues cli/mimaReportBinaryIssues \
        sbtPlugin/mimaReportBinaryIssues \
        irJS/mimaReportBinaryIssues toolsJS/mimaReportBinaryIssues &&
    sbt ++$scala library/scalastyle javalanglib/scalastyle javalib/scalastyle \
        javalibEx/scalastyle ir/scalastyle compiler/scalastyle \
        compiler/test:scalastyle tools/scalastyle tools/test:scalastyle \
        jsEnvs/scalastyle jsEnvsTestKit/scalastyle \
        jsEnvsTestSuite/test:scalastyle testAdapter/scalastyle \
        sbtPlugin/scalastyle testInterface/scalastyle \
        testSuite/scalastyle testSuite/test:scalastyle \
        testSuiteJVM/test:scalastyle \
        javalibExTestSuite/test:scalastyle helloworld/scalastyle \
        reversi/scalastyle testingExample/scalastyle \
        testingExample/test:scalastyle \
        jUnitPlugin/scalastyle jUnitRuntime/scalastyle \
        jUnitTestOutputsJVM/scalastyle jUnitTestOutputsJVM/test:scalastyle \
        jUnitTestOutputsJS/scalastyle jUnitTestOutputsJS/test:scalastyle &&
    sbt ++$scala ir/compile:doc tools/compile:doc jsEnvs/compile:doc \
        jsEnvsTestKit/compile:doc testAdapter/compile:doc stubs/compile:doc \
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
    sbt ++2.11.12 compiler/publishLocal library/publishLocal javalibEx/publishLocal \
                  testInterface/publishLocal stubs/publishLocal \
                  jUnitPlugin/publishLocal jUnitRuntime/publishLocal &&
    sbt ++$toolsscala ${SBT_VER_OVERRIDE:+^^$SBT_VER_OVERRIDE} \
        ir/publishLocal tools/publishLocal jsEnvs/publishLocal \
        testAdapter/publishLocal sbtPlugin/publishLocal &&
    cd sbt-plugin-test &&
    setJavaVersion $java &&
    if [ -n "$SBT_VER_OVERRIDE" ]; then echo "sbt.version=$SBT_VER_OVERRIDE" > ./project/build.properties; fi &&
    sbt noDOM/run noDOM/testHtmlFastOpt noDOM/testHtmlFullOpt \
        withDOM/run withDOM/testHtmlFastOpt withDOM/testHtmlFullOpt \
        multiTestJS/test:run multiTestJS/testHtmlFastOpt multiTestJS/testHtmlFullOpt \
        jetty9/run test \
        jsDependenciesTest/packageJSDependencies \
        jsDependenciesTest/packageMinifiedJSDependencies \
        jsDependenciesTest/regressionTestForIssue2243 \
        jsNoDependenciesTest/regressionTestForIssue2243 \
        multiTestJS/test:testScalaJSSourceMapAttribute \
        'set scalaJSUseRhino in Global := true' test
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

def mainScalaVersion = "2.12.8"
def mainScalaVersions = ["2.10.2", "2.11.12", "2.12.8"]
def otherScalaVersions = [
  "2.10.3",
  "2.10.4",
  "2.10.5",
  "2.10.6",
  "2.10.7",
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
  "2.12.0",
  "2.12.1",
  "2.12.2",
  "2.12.3",
  "2.12.4",
  "2.12.5",
  "2.12.6",
  "2.12.7"
]
def noToolsScalaVersions = ["2.13.0-RC2"]

// The 'quick' matrix
def quickMatrix = []
mainScalaVersions.each { scalaVersion ->
  allJavaVersions.each { javaVersion ->
    quickMatrix.add([task: "main", scala: scalaVersion, java: javaVersion])
  }
  quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  if (!scalaVersion.startsWith("2.10.")) {
    quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
    quickMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
  }
  quickMatrix.add([task: "bootstrap", scala: scalaVersion, java: mainJavaVersion])
  if (!scalaVersion.startsWith("2.10.")) {
    quickMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion])
  }
}
noToolsScalaVersions.each { scalaVersion ->
  quickMatrix.add([task: "main", scala: scalaVersion, java: mainJavaVersion])
  quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
}
allJavaVersions.each { javaVersion ->
  quickMatrix.add([task: "tools-cli-stubs-sbtplugin", scala: "2.12.8", sbt_version_override: "", java: javaVersion])
}
quickMatrix.add([task: "tools-cli-stubs", scala: "2.10.7", sbt_version_override: "0.13.17", java: mainJavaVersion])
quickMatrix.add([task: "partestc", scala: "2.11.0", java: mainJavaVersion])
quickMatrix.add([task: "sbtplugin-test", toolsscala: "2.10.7", sbt_version_override: "0.13.17", java: mainJavaVersion])
quickMatrix.add([task: "sbtplugin-test", toolsscala: "2.12.8", sbt_version_override: "", java: mainJavaVersion])

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
  if (!scalaVersion.startsWith("2.10.")) {
    def javaVersion = scalaVersion.startsWith("2.11.") ? "1.7" : mainJavaVersion
    fullMatrix.add([task: "partest-noopt", scala: scalaVersion, java: mainJavaVersion])
    fullMatrix.add([task: "partest-fullopt", scala: scalaVersion, java: mainJavaVersion])
  }
}
otherScalaVersions.each { scalaVersion ->
  // Partest does not compile on Scala 2.11.4 (see #1215).
  if (!scalaVersion.startsWith("2.10.") && scalaVersion != "2.11.4") {
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
        timeout(time: 3, unit: 'HOURS') {
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
