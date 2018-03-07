properties([
  parameters([
    string(name: 'matrix', defaultValue: 'pr', description: '')
  ])
])

def HOME = "/localhome/jenkins"
def LOC_SBT_BASE = "$HOME/scala-js-sbt-homes"
def LOC_SBT_BOOT = "$LOC_SBT_BASE/sbt-boot"
def LOC_SBT_HOME = "$LOC_SBT_BASE/sbt-home"

def SBT_OPTS="-J-Xmx3G -J-XX:MaxPermSize=512M -Djline.terminal=jline.UnsupportedTerminal -Dsbt.boot.directory=$LOC_SBT_BOOT -Dsbt.ivy.home=$LOC_SBT_HOME -Divy.home=$LOC_SBT_HOME -Dsbt.global.base=$LOC_SBT_BASE"

echo("Trying to run matrix ${params.matrix}")

buildDefs = []

def fetchMatrix(ciMatrix, matrixName) {
  def matrix = ciMatrix.getElementById(matrixName)
  def list = matrix.getElementsByTagName("run")
  for (int i = 0; i < list.getLength(); ++i) {
    handleRun(ciMatrix, list.item(i))
  }
}

def handleRun(ciMatrix, run) {
  def attrs = run.getAttributes()
  def matrixAttr = attrs.getNamedItem("matrix")
  def taskAttr = attrs.getNamedItem("task")
  if (matrixAttr != null)
    fetchMatrix(ciMatrix, matrixAttr.getValue())
  else if (taskAttr != null)
    fetchTask(ciMatrix, taskAttr.getValue(), run)
}

def fetchTask(ciMatrix, taskName, runElem) {
  def taskElem = ciMatrix.getElementById(taskName)
  def taskStr = taskElem.getTextContent()
  def fullTaskName = taskName

  runElem.getElementsByTagName("v").each { v ->
    def name = v.getAttribute("n")
    def value = v.getTextContent()
    taskStr = taskStr.replace('$' + name, value)
    fullTaskName += " $name=$value"
  }

  echo("Found task: $fullTaskName")

  buildDefs[fullTaskName] = {
    node('linuxworker') {
      checkout scm
      retry(2) {
        echo fullTaskName
        /*build("scalajs-task-worker",
          refspec: params.refspec,
          sha1: params.sha1,
          taskCommand: taskStr)*/
      }
    }
  }
}

stage('ReadMatrix') {
  node {
    checkout scm

    echo("Loading ci/matrix.xml")

    //def matrixFile = build.properties.moduleRoot.child("ci/matrix.xml")
    def matrixFileContent = readFile(file: "ci/matrix.xml", encoding: "UTF-8")
    def matrixStream = new java.io.ByteArrayInputStream(matrixFileContent.getBytes("UTF-8"))
    def fact = javax.xml.parsers.DocumentBuilderFactory.newInstance()
    def builder = fact.newDocumentBuilder()

    //ciMatrix = builder.parse(matrixFile.read())
    def ciMatrix = builder.parse(matrixStream)

    echo("Loading relevant definitions")

    fetchMatrix(ciMatrix, params.matrix)
  }
}

withEnv(["NODE_PATH=$HOME/node_modules/"]) {
  stage('Test') {
    parallel(buildDefs)
  }
}
