import build.Build

val scalajs = Build.root
val ir = Build.irProject
val irJS = Build.irProjectJS
val compiler = Build.compiler
val linkerInterface = Build.linkerInterface
val linkerInterfaceJS = Build.linkerInterfaceJS
val linkerPrivateLibrary = Build.linkerPrivateLibrary
val linker = Build.linker
val linkerJS = Build.linkerJS
val testAdapter = Build.testAdapter
val sbtPlugin = Build.plugin
val javalibintf = Build.javalibintf
val javalib = Build.javalib
val scalalib = Build.scalalib
val libraryAux = Build.libraryAux
val library = Build.library
val testInterface = Build.testInterface
val testBridge = Build.testBridge
val jUnitRuntime = Build.jUnitRuntime
val jUnitTestOutputsJS = Build.jUnitTestOutputsJS
val jUnitTestOutputsJVM = Build.jUnitTestOutputsJVM
val jUnitPlugin = Build.jUnitPlugin
val jUnitAsyncJS = Build.jUnitAsyncJS
val jUnitAsyncJVM = Build.jUnitAsyncJVM
val helloworld = Build.helloworld
val reversi = Build.reversi
val testingExample = Build.testingExample
val testSuite = Build.testSuite
val testSuiteJVM = Build.testSuiteJVM
val javalibExtDummies = Build.javalibExtDummies
val testSuiteEx = Build.testSuiteEx
val testSuiteExJVM = Build.testSuiteExJVM
val testSuiteLinker = Build.testSuiteLinker
val partest = Build.partest
val partestSuite = Build.partestSuite
val scalaTestSuite = Build.scalaTestSuite

inThisBuild(Build.thisBuildSettings)
