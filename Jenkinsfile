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
LOC_IVY_HOME="$LOC_SBT_BASE/sbt-home"
LOC_CS_CACHE="$LOC_SBT_BASE/coursier/cache"
TEST_LOCAL_IVY_HOME="$(pwd)/.ivy2-test-local"

rm -rf $TEST_LOCAL_IVY_HOME
mkdir $TEST_LOCAL_IVY_HOME
ln -s "$LOC_IVY_HOME/cache" "$TEST_LOCAL_IVY_HOME/cache"

export SBT_OPTS="-J-Xmx5G -Dsbt.boot.directory=$LOC_SBT_BOOT -Dsbt.ivy.home=$TEST_LOCAL_IVY_HOME -Divy.home=$TEST_LOCAL_IVY_HOME -Dsbt.global.base=$LOC_SBT_BASE"
export COURSIER_CACHE="$LOC_CS_CACHE"

export NODE_PATH="$HOME/node_modules/"

# Define setJavaVersion

setJavaVersion() {
  export JAVA_HOME=$HOME/apps/java-$1
  export PATH=$JAVA_HOME/bin:$PATH
}

# Define sbtretry and sbtnoretry

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
      echo "Command was: sbt" "$@"
      return $CODE
    fi
  fi
}

sbtnoretry() {
  echo "RUNNING sbt" "$@"
  sbt $SBT_OPTS "$@"
  CODE=$?
  if [ "$CODE" -ne 0 ]; then
    echo "FAILED"
    echo "Command was: sbt" "$@"
    return $CODE
  fi
}
'''

def Tasks = [
  "main": '''
    setJavaVersion $java
    npm install &&
    sbtretry ++$scala helloworld$v/run &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withPrettyPrint(true))' \
        ++$scala helloworld$v/run &&
    sbtretry 'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withOptimizer(false))' \
        ++$scala helloworld$v/run &&
    sbtretry 'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withSemantics(_.withAsInstanceOfs(CheckedBehavior.Unchecked)))' \
        ++$scala helloworld$v/run &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withESFeatures(_.withAllowBigIntsForLongs(true)))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("helloworld"))))' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        helloworld$v/run &&
    sbtretry ++$scala testingExample$v/testHtmlJSDom &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in testingExample.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in testingExample.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        testingExample$v/testHtml &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testingExample$v/testHtmlJSDom  &&
    sbtretry ++$scala testSuiteJVM$v/test testSuiteExJVM$v/test &&
    sbtretry ++$scala testSuite$v/test &&
    sbtretry ++$scala \
        testSuite$v/saveForStabilityTest \
        testSuite$v/checkStability \
        testSuite$v/forceRelinkForStabilityTest \
        testSuite$v/checkStability \
        testSuite$v/clean \
        testSuite$v/checkStability &&
    sbtretry ++$scala testSuiteEx$v/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testSuiteEx$v/test &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in testSuiteEx.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in testSuiteEx.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        testSuiteEx$v/test &&
    sbtretry ++$scala testSuite$v/test:doc library$v/test compiler$v/test &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in reversi.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in reversi.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        reversi$v/fastLinkJS \
        reversi$v/fullLinkJS &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in reversi.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("reversi"))))' \
        'set scalaJSLinkerConfig in reversi.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        reversi$v/fastLinkJS \
        reversi$v/fullLinkJS &&
    sbtretry ++$scala \
        reversi$v/fastLinkJS \
        reversi$v/fullLinkJS \
        reversi$v/checksizes &&
    sbtretry ++$scala \
        'set Global/enableMinifyEverywhere := true' \
        reversi$v/checksizes &&
    sbtretry ++$scala javalibintf/compile:doc compiler$v/compile:doc library$v/compile:doc \
        testInterface$v/compile:doc testBridge$v/compile:doc &&
    sbtretry ++$scala headerCheck &&
    sbtretry ++$scala partest$v/fetchScalaSource &&
    sbtretry ++$scala \
        javalibintf/mimaReportBinaryIssues \
        library$v/mimaReportBinaryIssues \
        testInterface$v/mimaReportBinaryIssues \
        jUnitRuntime$v/mimaReportBinaryIssues
  ''',

  "test-suite-default-esversion": '''
    setJavaVersion $java
    npm install &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        jUnitTestOutputsJVM$v/test jUnitTestOutputsJS$v/test testBridge$v/test \
        'set scalaJSStage in Global := FullOptStage' jUnitTestOutputsJS$v/test testBridge$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        $testSuite$v/test $testSuite$v/testHtmlJSDom &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test \
        $testSuite$v/testHtmlJSDom &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withAllowBigIntsForLongs(true)))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withAllowBigIntsForLongs(true)).withOptimizer(false))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withAvoidLetsAndConsts(false).withAvoidClasses(false)))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withAvoidLetsAndConsts(false).withAvoidClasses(false)))' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        $testSuite$v/test &&
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallModulesFor(List("org.scalajs.testsuite"))))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        $testSuite$v/test &&
    # The following tests the same thing whether testMinify is true or false; we also set it for regularity.
    sbtretry ++$scala 'set Global/enableMinifyEverywhere := $testMinify' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test
  ''',

  "test-suite-custom-esversion-force-polyfills": '''
    setJavaVersion $java
    npm install &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set Seq(jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills(ESVersion.$esVersion), MyScalaJSPlugin.wantSourceMaps in $testSuite.v$v := ("$esVersion" != "ES5_1"))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set Seq(jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills(ESVersion.$esVersion), MyScalaJSPlugin.wantSourceMaps in $testSuite.v$v := ("$esVersion" != "ES5_1"))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set Seq(jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills(ESVersion.$esVersion), MyScalaJSPlugin.wantSourceMaps in $testSuite.v$v := ("$esVersion" != "ES5_1"))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set Seq(jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills(ESVersion.$esVersion), MyScalaJSPlugin.wantSourceMaps in $testSuite.v$v := ("$esVersion" != "ES5_1"))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set Seq(jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills(ESVersion.$esVersion), MyScalaJSPlugin.wantSourceMaps in $testSuite.v$v := ("$esVersion" != "ES5_1"))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set Seq(jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills(ESVersion.$esVersion), MyScalaJSPlugin.wantSourceMaps in $testSuite.v$v := ("$esVersion" != "ES5_1"))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test
  ''',

  "test-suite-custom-esversion": '''
    setJavaVersion $java
    npm install &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion).withAllowBigIntsForLongs(true)))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion).withAllowBigIntsForLongs(true)).withOptimizer(false))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleSplitStyle(ModuleSplitStyle.SmallestModules))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test
  ''',

  "test-suite-webassembly": '''
    setJavaVersion $java
    npm install &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withPrettyPrint(true))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in reversi.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        reversi$v/fastLinkJS \
        reversi$v/fullLinkJS &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in jUnitTestOutputsJS.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in testBridge.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        jUnitTestOutputsJS$v/test testBridge$v/test \
        'set scalaJSStage in Global := FullOptStage' \
        jUnitTestOutputsJS$v/test testBridge$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        $testSuite$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        $testSuite$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        $testSuite$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        $testSuite$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        $testSuite$v/test &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        testingExample$v/testHtml &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        'set scalaJSStage in Global := FullOptStage' \
        testingExample$v/testHtml &&
    sbtretry ++$scala \
        'set Global/enableWasmEverywhere := true' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withESVersion(ESVersion.$esVersion)))' \
        irJS$v/fastLinkJS
  ''',

  /* For the bootstrap tests to be able to call
   * `testSuite/test:fastOptJS`, `scalaJSStage in testSuite` must be
   * `FastOptStage`, even when `scalaJSStage in Global` is `FullOptStage`.
   */
  "bootstrap": '''
    setJavaVersion $java
    npm install &&
    sbtnoretry ++$scala linker$v/test &&
    sbtnoretry linkerPrivateLibrary/test &&
    sbtnoretry ++$scala irJS$v/test &&
    sbtnoretry ++$scala linkerInterfaceJS$v/test &&
    sbtnoretry ++$scala linkerJS$v/test &&
    sbtnoretry 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite.v$v := FastOptStage' \
        ++$scala irJS$v/test &&
    sbtnoretry 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite.v$v := FastOptStage' \
        ++$scala linkerInterfaceJS$v/test &&
    sbtnoretry 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite.v$v := FastOptStage' \
        ++$scala linkerJS$v/test &&
    sbtnoretry ++$scala testSuite$v/bootstrap:test &&
    sbtnoretry 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite.v$v := FastOptStage' \
        ++$scala testSuite$v/bootstrap:test &&
    sbtnoretry ++$scala irJS$v/mimaReportBinaryIssues \
        linkerInterfaceJS$v/mimaReportBinaryIssues linkerJS$v/mimaReportBinaryIssues
  ''',

  "tools": '''
    setJavaVersion $java
    npm install &&
    sbtnoretry ++$scala ir$v/test linkerInterface$v/test \
        linker$v/compile testAdapter$v/test \
        ir$v/mimaReportBinaryIssues \
        linkerInterface$v/mimaReportBinaryIssues linker$v/mimaReportBinaryIssues \
        testAdapter$v/mimaReportBinaryIssues &&
    sbtnoretry ++$scala ir$v/compile:doc \
        linkerInterface$v/compile:doc linker$v/compile:doc \
        testAdapter$v/compile:doc
  ''',

  // These are agnostic to the Scala version
  "sbt-plugin-and-scalastyle": '''
    setJavaVersion $java
    npm install &&
    sbtnoretry \
        sbtPlugin2_12/compile:doc \
        sbtPlugin2_12/mimaReportBinaryIssues \
        scalastyleCheck &&
    sbtnoretry sbtPlugin2_12/scripted
  ''',

  "partest-noopt": '''
    setJavaVersion $java
    npm install &&
    sbtnoretry ++$scala partestSuite$v/test:compile &&
    sbtnoretry ++$scala "partestSuite$v/testOnly -- $partestopts --showDiff"
  ''',

  "partest-fastopt": '''
    setJavaVersion $java
    npm install &&
    sbtnoretry ++$scala partestSuite$v/test:compile &&
    sbtnoretry ++$scala "partestSuite$v/testOnly -- $partestopts --fastOpt --showDiff"
  ''',

  "partest-fullopt": '''
    setJavaVersion $java
    npm install &&
    sbtnoretry ++$scala partestSuite$v/test:compile &&
    sbtnoretry ++$scala "partestSuite$v/testOnly -- $partestopts --fullOpt --showDiff"
  ''',

  "scala3-compat": '''
    setJavaVersion $java
    npm install &&
    sbtnoretry ++$scala! ir2_13/test
  '''
]

def mainJavaVersion = "1.8"
def otherJavaVersions = ["11", "17", "21"]
def allJavaVersions = otherJavaVersions.clone()
allJavaVersions << mainJavaVersion

def mainScalaVersion = "2.12.20"
def mainScalaVersions = ["2.12.20", "2.13.16"]
def otherScalaVersions = [
  "2.12.6",
  "2.12.7",
  "2.12.8",
  "2.12.9",
  "2.12.10",
  "2.12.11",
  "2.12.12",
  "2.12.13",
  "2.12.14",
  "2.12.15",
  "2.12.16",
  "2.12.17",
  "2.12.18",
  "2.12.19",
  "2.13.3",
  "2.13.4",
  "2.13.5",
  "2.13.6",
  "2.13.7",
  "2.13.8",
  "2.13.9",
  "2.13.10",
  "2.13.11",
  "2.13.12",
  "2.12.13",
  "2.12.14",
  "2.12.15"
]

def scala3Version = "3.6.3"

def allESVersions = [
  "ES5_1",
  "ES2015",
  // "ES2016", // Technically we have the '**' operator dependent on ES2016, but it's not enough to justify testing this version
  "ES2017",
  "ES2018",
  // "ES2019", // We do not use anything specifically from ES2019
  "ES2020",
  "ES2021" // We do not use anything specifically from ES2021, but always test the latest to avoid #4675
]
def defaultESVersion = "ES2015"
def latestESVersion = "ES2021"

// The 'quick' matrix
def quickMatrix = []
mainScalaVersions.each { scalaVersion ->
  allJavaVersions.each { javaVersion ->
    quickMatrix.add([task: "main", scala: scalaVersion, java: javaVersion])
    quickMatrix.add([task: "tools", scala: scalaVersion, java: javaVersion])
  }
  quickMatrix.add([task: "test-suite-default-esversion", scala: scalaVersion, java: mainJavaVersion, testMinify: "false", testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-default-esversion", scala: scalaVersion, java: mainJavaVersion, testMinify: "true", testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-custom-esversion", scala: scalaVersion, java: mainJavaVersion, esVersion: "ES5_1", testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-webassembly", scala: scalaVersion, java: mainJavaVersion, esVersion: defaultESVersion, testMinify: "false", testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-webassembly", scala: scalaVersion, java: mainJavaVersion, esVersion: latestESVersion, testMinify: "false", testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-webassembly", scala: scalaVersion, java: mainJavaVersion, esVersion: defaultESVersion, testMinify: "false", testSuite: "testSuiteEx"])
  quickMatrix.add([task: "test-suite-default-esversion", scala: scalaVersion, java: mainJavaVersion, testMinify: "false", testSuite: "scalaTestSuite"])
  quickMatrix.add([task: "test-suite-custom-esversion", scala: scalaVersion, java: mainJavaVersion, esVersion: "ES5_1", testSuite: "scalaTestSuite"])
  quickMatrix.add([task: "test-suite-webassembly", scala: scalaVersion, java: mainJavaVersion, esVersion: defaultESVersion, testMinify: "false", testSuite: "scalaTestSuite"])
  quickMatrix.add([task: "bootstrap", scala: scalaVersion, java: mainJavaVersion])
  quickMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion, partestopts: ""])
  quickMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion, partestopts: "--wasm"])
}
allESVersions.each { esVersion ->
  quickMatrix.add([task: "test-suite-custom-esversion-force-polyfills", scala: mainScalaVersion, java: mainJavaVersion, esVersion: esVersion, testSuite: "testSuite"])
}
allJavaVersions.each { javaVersion ->
  // the `scala` version is irrelevant here
  // We exclude JDK 21 because our sbt scripted tests use old sbt versions (on purpose), which do not support JDK 21
  if (javaVersion != '21') {
    quickMatrix.add([task: "sbt-plugin-and-scalastyle", scala: mainScalaVersion, java: javaVersion])
  }
}
quickMatrix.add([task: "scala3-compat", scala: scala3Version, java: mainJavaVersion])

// The 'full' matrix
def fullMatrix = quickMatrix.clone()
otherScalaVersions.each { scalaVersion ->
  fullMatrix.add([task: "main", scala: scalaVersion, java: mainJavaVersion])
}
mainScalaVersions.each { scalaVersion ->
  otherJavaVersions.each { javaVersion ->
    quickMatrix.add([task: "test-suite-default-esversion", scala: scalaVersion, java: javaVersion, testMinify: "false", testSuite: "testSuite"])
    quickMatrix.add([task: "test-suite-webassembly", scala: scalaVersion, java: mainJavaVersion, esVersion: defaultESVersion, testMinify: "false", testSuite: "testSuite"])
  }
  fullMatrix.add([task: "partest-noopt", scala: scalaVersion, java: mainJavaVersion, partestopts: ""])
  fullMatrix.add([task: "partest-noopt", scala: scalaVersion, java: mainJavaVersion, partestopts: "--wasm"])
  fullMatrix.add([task: "partest-fullopt", scala: scalaVersion, java: mainJavaVersion, partestopts: ""])
  fullMatrix.add([task: "partest-fullopt", scala: scalaVersion, java: mainJavaVersion, partestopts: "--wasm"])
}
otherJavaVersions.each { javaVersion ->
  fullMatrix.add([task: "scala3-compat", scala: scala3Version, java: javaVersion])
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

  def suffix = taskDef.scala.split('\\.')[0..1].join('_')
  taskStr = taskStr.replace('$v', suffix)

  def ciScript = CIScriptPrelude + taskStr

  buildDefs.put(fullTaskName, {
    node('linuxworker') {
      checkout scm
      retry(2) {
        sh "git clean -fdx && rm -rf partest/fetchedSources/"
        writeFile file: 'ciscript.sh', text: ciScript, encoding: 'UTF-8'
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
