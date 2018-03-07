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

def triggers = []
if (env.BRANCH_NAME == 'master') {
  // Run nightly from Monday to Saturday
  triggers << cron('H H(0-2) * * 0-5')
} else if (env.BRANCH_NAME == '0.6.x') {
  // Run weekly on Sunday
  triggers << cron('H H(0-2) * * 6')
} else if (env.BRANCH_NAME == 'jenkinsfile') {
  // For test, run hourly
  triggers << cron('H * * * *')
}

properties([
  parameters([
    string(name: 'matrix', defaultValue: 'auto', description: 'The matrix to build (auto, pr, nightly)')
  ]),
  pipelineTriggers(triggers)
])

def selectedMatrix = params.matrix
if (selectedMatrix == 'auto') {
  def reason = ''
  if (env.CHANGE_ID) {
    reason = "is a PR ${env.CHANGE_ID}"
    selectedMatrix = 'pr'
  } else {
    reason = "is not a PR, startedByTimer = $startedByTimer"
    if (startedByTimer) {
      selectedMatrix = 'nightly'
    } else {
      selectedMatrix = 'pr'
    }
  }
  echo("Auto-selected matrix: $selectedMatrix")
} else {
  echo("Explicit matrix: $selectedMatrix")
}

def HOME = "/localhome/jenkins"
def LOC_SBT_BASE = "$HOME/scala-js-sbt-homes"
def LOC_SBT_BOOT = "$LOC_SBT_BASE/sbt-boot"
def LOC_SBT_HOME = "$LOC_SBT_BASE/sbt-home"

def CIScriptPrelude = """
export NODE_PATH="$HOME/node_modules/"

export SBT_OPTS="-J-Xmx3G -J-XX:MaxPermSize=512M -Djline.terminal=jline.UnsupportedTerminal -Dsbt.boot.directory=$LOC_SBT_BOOT -Dsbt.ivy.home=$LOC_SBT_HOME -Divy.home=$LOC_SBT_HOME -Dsbt.global.base=$LOC_SBT_BASE"
""" +
'''
# Define setJavaVersion

setJavaVersion() {
  export JAVA_HOME=$HOME/apps/java-$1
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
    sbtretry ++$scala testingExample/testHtml &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testingExample/testHtml \
        testingExample/clean &&
    sbtretry ++$scala library/test &&
    sbtretry ++$scala testSuiteJVM/test testSuiteJVM/clean &&
    sbtretry ++$scala testSuite/test &&
    sbtretry ++$scala testSuiteEx/test &&
    sbtretry 'set scalaJSStage in Global := FullOptStage' \
        ++$scala testSuiteEx/test &&
    sbtretry ++$scala testSuite/test:doc compiler/test reversi/fastOptJS reversi/fullOptJS &&
    sbtretry ++$scala compiler/compile:doc library/compile:doc \
        testInterface/compile:doc &&
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
    sbtretry 'set scalacOptions in $testSuite += "-Xexperimental"' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalacOptions in $testSuite += "-Xexperimental"' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
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
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= makeCompliant' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSStage in Global := FullOptStage' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withOptimizer(false))' \
        ++$scala $testSuite/test \
        $testSuite/clean &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        ++$scala $testSuite/test &&
    sbtretry 'set scalaJSLinkerConfig in $testSuite ~= (_.withOutputMode(OutputMode.ECMAScript6))' \
        'set jsEnv in $testSuite := new org.scalajs.jsenv.nodejs.NodeJSEnv(org.scalajs.jsenv.nodejs.NodeJSEnv.Config().withSourceMap(false))' \
        'set scalaJSLinkerConfig in $testSuite ~= (_.withModuleKind(ModuleKind.CommonJSModule))' \
        'set scalaJSStage in Global := FullOptStage' \
        ++$scala $testSuite/test
  ''',

  "bootstrap": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala linker/test &&
    sbt ++$scala irJS/test ioJS/test linkerJS/test &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        ++$scala irJS/test ioJS/test linkerJS/test &&
    sbt ++$scala testSuite/test:fastOptJS &&
    sbt ++$scala linkerJS/bootstrapTest &&
    sbt ++$scala testSuite/test:fullOptJS &&
    sbt 'set scalaJSStage in Global := FullOptStage' \
        ++$scala linkerJS/bootstrapTest &&
    sbt ++$scala irJS/mimaReportBinaryIssues ioJS/mimaReportBinaryIssues \
        loggingJS/mimaReportBinaryIssues linkerJS/mimaReportBinaryIssues
  ''',

  "tools-stubs": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala ir/test io/test logging/compile linker/compile \
        stubs/package nodeJSEnv/test testAdapter/test \
        ir/mimaReportBinaryIssues io/mimaReportBinaryIssues \
        logging/mimaReportBinaryIssues linker/mimaReportBinaryIssues \
        jsEnvs/mimaReportBinaryIssues jsEnvsTestKit/mimaReportBinaryIssues \
        nodeJSEnv/mimaReportBinaryIssues \
        testAdapter/mimaReportBinaryIssues \
        stubs/mimaReportBinaryIssues &&
    sbt ++$scala ir/compile:doc io/compile:doc logging/compile:doc \
        linker/compile:doc jsEnvs/compile:doc \
        jsEnvsTestKit/compile:doc nodeJSEnv/compile:doc \
        testAdapter/compile:doc stubs/compile:doc
  ''',

  "tools-stubs-sbtplugin": '''
    setJavaVersion $java
    npm install &&
    sbt ++$scala ir/test io/test logging/compile linker/compile \
        stubs/package nodeJSEnv/test testAdapter/test \
        sbtPlugin/package \
        ir/mimaReportBinaryIssues io/mimaReportBinaryIssues \
        logging/mimaReportBinaryIssues linker/mimaReportBinaryIssues \
        jsEnvs/mimaReportBinaryIssues jsEnvsTestKit/mimaReportBinaryIssues \
        nodeJSEnv/mimaReportBinaryIssues \
        testAdapter/mimaReportBinaryIssues \
        stubs/mimaReportBinaryIssues \
        sbtPlugin/mimaReportBinaryIssues &&
    sbt ++$scala library/scalastyle javalanglib/scalastyle javalib/scalastyle \
        ir/scalastyle compiler/scalastyle \
        compiler/test:scalastyle io/scalastyle io/test:scalastyle \
        logging/scalastyle logging/test:scalastyle \
        linker/scalastyle linker/test:scalastyle \
        jsEnvs/scalastyle jsEnvsTestKit/scalastyle nodeJSEnv/scalastyle \
        nodeJSEnv/test:scalastyle testAdapter/scalastyle \
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
        testAdapter/compile:doc stubs/compile:doc \
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
                  testInterface/publishLocal stubs/publishLocal \
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

def mainScalaVersion = "2.12.4"
def mainJVMScalaVersions = ["2.10.7", "2.11.12", "2.12.4", "2.13.0-M2"]
def mainJSScalaVersions = ["2.11.12", "2.12.4", "2.13.0-M2"]
def otherJSScalaVersions = [
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
  "2.12.3"
]

// The 'pr' matrix
def prMatrix = []
mainJSScalaVersions.each { scalaVersion ->
  prMatrix.add([task: "main", scala: scalaVersion, java: mainJavaVersion])
  prMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  prMatrix.add([task: "test-suite-ecma-script5", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
  prMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
  prMatrix.add([task: "test-suite-ecma-script6", scala: scalaVersion, java: mainJavaVersion, testSuite: "scalaTestSuite"])
  prMatrix.add([task: "bootstrap", scala: scalaVersion, java: mainJavaVersion])
  prMatrix.add([task: "partest-fastopt", scala: scalaVersion, java: mainJavaVersion])
}
prMatrix.add([task: "test-suite-ecma-script5-force-polyfills", scala: mainScalaVersion, java: mainJavaVersion, testSuite: "testSuite"])
prMatrix.add([task: "tools-stubs-sbtplugin", scala: "2.10.7", java: mainJavaVersion])
prMatrix.add([task: "tools-stubs", scala: "2.11.12", java: mainJavaVersion])
prMatrix.add([task: "tools-stubs", scala: "2.12.4", java: mainJavaVersion])
prMatrix.add([task: "partestc", scala: "2.11.0", java: mainJavaVersion])
prMatrix.add([task: "sbtplugin-test", toolsscala: "2.10.7", sbt_version_override: "", java: mainJavaVersion])
prMatrix.add([task: "sbtplugin-test", toolsscala: "2.12.4", sbt_version_override: "1.0.0", java: mainJavaVersion])

// The 'nightly' matrix
def nightlyMatrix = prMatrix.clone()
otherJSScalaVersions.each { scalaVersion ->
  prMatrix.add([task: "main", scala: scalaVersion, java: mainJavaVersion])
}
mainJSScalaVersions.each { scalaVersion ->
  prMatrix.add([task: "partest-noopt", scala: scalaVersion, java: mainJavaVersion])
  prMatrix.add([task: "partest-fullopt", scala: scalaVersion, java: mainJavaVersion])
}

def Matrices = [
  pr: prMatrix,
  nightly: nightlyMatrix
]

def matrix = Matrices[selectedMatrix]

buildDefs = [:]
matrix.each { taskDef ->
  def taskStr = Tasks[taskDef.task]
  def fullTaskName = taskDef.task

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
      writeFile file: 'ciscript.sh', text: ciScript, encoding: 'UTF-8'
      retry(2) {
        sh "echo '$fullTaskName' && cat ciscript.sh && sh ciscript.sh"
      }
    }
  })
}

stage('Test') {
  parallel(buildDefs)
}
