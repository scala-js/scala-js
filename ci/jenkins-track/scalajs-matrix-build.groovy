out.println("Trying to run matrix ${params.matrix}")

out.println("Loading ci/matrix.xml")

def matrixFile = build.properties.moduleRoot.child("ci/matrix.xml")
def fact = javax.xml.parsers.DocumentBuilderFactory.newInstance()
def builder = fact.newDocumentBuilder()

ciMatrix = builder.parse(matrixFile.read())

out.println("Loading relevant definitions")

buildDefs = []

fetchMatrix(params.matrix)

def fetchMatrix(matrixName) {
  def matrix = ciMatrix.getElementById(matrixName)
  def list = matrix.getElementsByTagName("run")
  for (int i = 0; i < list.getLength(); ++i) {
    handleRun(list.item(i))
  }
}

def handleRun(run) {
  def attrs = run.getAttributes()
  def matrixAttr = attrs.getNamedItem("matrix")
  def taskAttr = attrs.getNamedItem("task")
  if (matrixAttr != null)
    fetchMatrix(matrixAttr.getValue())
  else if (taskAttr != null)
    fetchTask(taskAttr.getValue(), run)
}

def fetchTask(taskName, runElem) {
  def taskElem = ciMatrix.getElementById(taskName)
  def taskStr = taskElem.getTextContent()
  def fullTaskName = taskName

  runElem.getElementsByTagName("v").each { v ->
    def name = v.getAttribute("n")
    def value = v.getTextContent()
    taskStr = taskStr.replace('$' + name, value)
    fullTaskName += " $name=$value"
  }

  out.println("Found task: $fullTaskName")

  buildDefs.add({
    retry(2) {
      build("scalajs-task-worker",
        refspec: params.refspec,
        sha1: params.sha1,
        taskCommand: taskStr)
    }
  })
}

parallel(buildDefs)
