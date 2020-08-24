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
TEST_LOCAL_IVY_HOME="$(pwd)/.ivy2-test-local"

rm -rf $TEST_LOCAL_IVY_HOME
mkdir $TEST_LOCAL_IVY_HOME
ln -s "$LOC_IVY_HOME/cache" "$TEST_LOCAL_IVY_HOME/cache"

export SBT_OPTS="-J-Xmx5G -J-XX:MaxPermSize=512M -Dsbt.boot.directory=$LOC_SBT_BOOT -Dsbt.ivy.home=$TEST_LOCAL_IVY_HOME -Divy.home=$TEST_LOCAL_IVY_HOME -Dsbt.global.base=$LOC_SBT_BASE"

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
    sbtretry ++$scala helloworld$v/run &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala helloworld$v/run \
        helloworld$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withOptimizer(false))' \
        ++$scala helloworld$v/run \
        helloworld$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withSemantics(_.withAsInstanceOfs(CheckedBehavior.Unchecked)))' \
        ++$scala helloworld$v/run \
        helloworld$v/clean &&
    sbtretry ++$scala \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        helloworld$v/run \
        helloworld$v/clean &&
    sbtretry ++$scala \
        'set artifactPath in (helloworld.v$v, Compile, fastOptJS) := (crossTarget in helloworld.v$v).value / "helloworld-fastopt.mjs"' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        helloworld$v/run &&
    sbtretry ++$scala \
        'set artifactPath in (helloworld.v$v, Compile, fullOptJS) := (crossTarget in helloworld.v$v).value / "helloworld-opt.mjs"' \
        'set scalaJSLinkerConfig in helloworld.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        helloworld$v/run \
        helloworld$v/clean &&
    sbtretry ++$scala testingExample$v/testHtmlJSDom &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testingExample$v/testHtmlJSDom \
        testingExample$v/clean &&
    sbtretry ++$scala testSuiteJVM$v/test testSuiteJVM$v/clean &&
    sbtretry ++$scala testSuite$v/test &&
    sbtretry ++$scala testSuiteEx$v/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testSuiteEx$v/test &&
    sbtretry ++$scala testSuite$v/test:doc library$v/test compiler$v/test reversi$v/fastOptJS reversi$v/fullOptJS &&
    sbtretry ++$scala compiler$v/compile:doc library$v/compile:doc \
        testInterface$v/compile:doc testBridge$v/compile:doc &&
    sbtretry ++$scala headerCheck &&
    sbtretry ++$scala partest$v/fetchScalaSource &&
    sbtretry ++$scala library$v/mimaReportBinaryIssues testInterface$v/mimaReportBinaryIssues &&
    sh ci/checksizes.sh $scala &&
    sh ci/check-partest-coverage.sh $scala
  ''',

  "test-suite-ecma-script2015": '''
    setJavaVersion $java
    npm install &&
    sbtretry ++$scala jUnitTestOutputsJVM$v/test jUnitTestOutputsJS$v/test testBridge$v/test \
        'set scalaJSStage in Global := FullOptStage' jUnitTestOutputsJS$v/test testBridge$v/test &&
    sbtretry ++$scala $testSuite$v/test $testSuite$v/testHtmlJSDom &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
	$testSuite$v/testHtmlJSDom \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withAllowBigIntsForLongs(true)))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withAllowBigIntsForLongs(true)).withOptimizer(false))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalacOptions in $testSuite.v$v += "-Xexperimental"' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalacOptions in $testSuite.v$v += "-Xexperimental"' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set artifactPath in ($testSuite.v$v, Test, fastOptJS) := (crossTarget in $testSuite.v$v).value / "testsuite-fastopt.mjs"' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set artifactPath in ($testSuite.v$v, Test, fullOptJS) := (crossTarget in $testSuite.v$v).value / "testsuite-opt.mjs"' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test
  ''',

  "test-suite-ecma-script5-force-polyfills": '''
    setJavaVersion $java
    npm install &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills()' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills()' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set jsEnv in $testSuite.v$v := new NodeJSEnvForcePolyfills()' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean
  ''',

  "test-suite-ecma-script5": '''
    setJavaVersion $java
    npm install &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withOptimizer(false))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false).withAllowBigIntsForLongs(true)))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false).withAllowBigIntsForLongs(true)).withOptimizer(false))' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test \
        $testSuite$v/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set artifactPath in ($testSuite.v$v, Test, fastOptJS) := (crossTarget in $testSuite.v$v).value / "testsuite-fastopt.mjs"' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        ++$scala $testSuite$v/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withESFeatures(_.withUseECMAScript2015(false)))' \
        'set artifactPath in ($testSuite.v$v, Test, fullOptJS) := (crossTarget in $testSuite.v$v).value / "testsuite-opt.mjs"' \
        'set scalaJSLinkerConfig in $testSuite.v$v ~= (_.withModuleKind(ModuleKind.ESModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite$v/test
  ''',

  /* For the bootstrap tests to be able to call
   * `testSuite/test:fastOptJS`, `scalaJSStage in testSuite` must be
   * `FastOptStage`, even when `scalaJSStage in Global` is `FullOptStage`.
   */
  "bootstrap": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala linker$v/test &&
    sbt linkerPrivateLibrary/test &&
    sbt ++$scala irJS$v/test linkerJS$v/test &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite.v$v := FastOptStage' \
        ++$scala irJS$v/test linkerJS$v/test linkerInterfaceJS$v/test &&
    sbt ++$scala testSuite$v/bootstrap:test &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSStage in testSuite.v$v := FastOptStage' \
        ++$scala testSuite$v/bootstrap:test &&
    sbt ++$scala irJS$v/mimaReportBinaryIssues \
        linkerInterfaceJS$v/mimaReportBinaryIssues linkerJS$v/mimaReportBinaryIssues
  ''',

  "tools": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala ir$v/test linkerInterface$v/test \
        linker$v/compile testAdapter$v/test \
        ir$v/mimaReportBinaryIssues \
        linkerInterface$v/mimaReportBinaryIssues linker$v/mimaReportBinaryIssues \
        testAdapter$v/mimaReportBinaryIssues &&
    sbt ++$scala ir$v/compile:doc \
        linkerInterface$v/compile:doc linker$v/compile:doc \
        testAdapter$v/compile:doc
  ''',

  "tools-sbtplugin": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala ir$v/test linkerInterface$v/compile \
        linker$v/compile testAdapter$v/test \
        sbtPlugin/package \
        ir$v/mimaReportBinaryIssues \
        linkerInterface$v/mimaReportBinaryIssues linker$v/mimaReportBinaryIssues \
        testAdapter$v/mimaReportBinaryIssues \
        sbtPlugin/mimaReportBinaryIssues &&
    sbt ++$scala library$v/scalastyle javalanglib$v/scalastyle javalib$v/scalastyle \
        ir$v/scalastyle compiler$v/scalastyle \
        compiler$v/test:scalastyle \
        linkerInterface$v/scalastyle linkerInterface$v/test:scalastyle \
        linker$v/scalastyle linker$v/test:scalastyle \
        testAdapter$v/scalastyle \
        sbtPlugin/scalastyle testInterface$v/scalastyle testBridge$v/scalastyle \
        testSuite$v/scalastyle testSuite$v/test:scalastyle \
        testSuiteJVM$v/test:scalastyle \
        testSuiteEx$v/test:scalastyle helloworld$v/scalastyle \
        reversi$v/scalastyle testingExample$v/scalastyle \
        testingExample$v/test:scalastyle \
        jUnitPlugin$v/scalastyle jUnitRuntime$v/scalastyle \
        jUnitTestOutputsJVM$v/scalastyle jUnitTestOutputsJVM$v/test:scalastyle \
        jUnitTestOutputsJS$v/scalastyle jUnitTestOutputsJS$v/test:scalastyle &&
    sbt ++$scala ir$v/compile:doc \
        linkerInterface$v/compile:doc linker$v/compile:doc \
        testAdapter$v/compile:doc \
        sbtPlugin/compile:doc &&
    sbt sbtPlugin/scripted
  ''',

  "partestc": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala partest$v/compile
  ''',

  "partest-noopt": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala package "partestSuite$v/testOnly -- --showDiff"
  ''',

  "partest-fastopt": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala package "partestSuite$v/testOnly -- --fastOpt --showDiff"
  ''',

  "partest-fullopt": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala package "partestSuite$v/testOnly -- --fullOpt --showDiff"
  '''
]

def mainJavaVersion = "1.8"
def otherJavaVersions = []
def allJavaVersions = otherJavaVersions.clone()
allJavaVersions << mainJavaVersion

def mainScalaVersion = "2.12.12"
def mainScalaVersions = ["2.11.12", "2.12.12", "2.13.3"]
def otherScalaVersions = [
  "2.11.12",
  "2.12.1",
  "2.12.2",
  "2.12.3",
  "2.12.4",
  "2.12.5",
  "2.12.6",
  "2.12.7",
  "2.12.8",
  "2.12.9",
  "2.12.10",
  "2.12.11",
  "2.13.0",
  "2.13.1",
  "2.13.2"
]

// The 'quick' matrix
def quickMatrix = []
mainScalaVersions.each { scalaVersion ->
  allJavaVersions.each { javaVersion ->
    quickMatrix.add([task: "main", scala: scalaVersion, java: javaVersion])
  }
  quickMatrix.add([task: "test-suite-ecma-script2015", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  quickMatrix.add([task: "test-suite-ecma-script2015", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
  quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
  quickMatrix.add([task: "bootstrap", scala: scalaVersion, java: mainJavaVersion])
  quickMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion])
}
quickMatrix.add([task: "test-suite-ecma-script5-force-polyfills", scala: mainScalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
allJavaVersions.each { javaVersion ->
  quickMatrix.add([task: "tools-sbtplugin", scala: "2.12.12", java: javaVersion])
  quickMatrix.add([task: "tools", scala: "2.11.12", java: javaVersion])
  quickMatrix.add([task: "tools", scala: "2.13.3", java: javaVersion])
}
quickMatrix.add([task: "partestc", scala: "2.12.1", java: mainJavaVersion])

// The 'full' matrix
def fullMatrix = quickMatrix.clone()
otherScalaVersions.each { scalaVersion ->
  fullMatrix.add([task: "main", scala: scalaVersion, java: mainJavaVersion])
}
mainScalaVersions.each { scalaVersion ->
  otherJavaVersions.each { javaVersion ->
    quickMatrix.add([task: "test-suite-ecma-script2015", scala: scalaVersion, java: javaVersion, testSuite: "testSuite"])
    quickMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: javaVersion, testSuite: "testSuite"])
  }
  fullMatrix.add([task: "partest-noopt", scala: scalaVersion, java: mainJavaVersion])
  fullMatrix.add([task: "partest-fullopt", scala: scalaVersion, java: mainJavaVersion])
}
otherScalaVersions.each { scalaVersion ->
  fullMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion])
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
