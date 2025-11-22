# Developer documentation

## Very important notice about the Javalib

If you haven't read it, ***read the very important notice about the Javalib
in the [Javalib documentation](./JAVALIB.md)*** .

## Building

Scala.js is entirely built with [sbt](https://www.scala-sbt.org/), and also
requires [Node.js](https://nodejs.org/en/) to be installed. For complete
support, Node.js >= 24.0.0 is required.

The first time, or in the rare events where `package.json` changes
([history](https://github.com/scala-js/scala-js/commits/main/package.json)),
you need to run

    $ npm install

from your shell. If you *really* do not want to do this, you can avoid that
step, but you will need to use
`set MyScalaJSPlugin.wantSourceMaps in testSuite := false` within sbt to
by-pass source-map-related tests. In addition, bootstrap tests will not pass.

Otherwise, everything happens within sbt.

Run the normal test suite using the entire Scala.js toolchain using

    > testSuite2_12/test

In order to test the tests themselves, run the cross-compiling tests on the JVM
with:

    > testSuiteJVM2_12/test

If you have changed the IR or the compiler, you typically need to

    > clean

before testing anew.

If you have changed the logging API, the linker interface, the JS environments,
the test adapter or the sbt plugin, you typically need to

    > reload

To test in fullOpt stage:

    > set scalaJSStage in Global := FullOptStage

There are also a few additional tests in a separate testing project:

    > testSuiteEx2_12/test

The compiler tests (mostly verifying expected compile error messages) can be
run with

    > compiler2_12/test

The full partest suite (tests of the Scala language, ported in Scala.js) are
run with:

    > partestSuite2_12/test

or, more typically,

    > partestSuite2_12/testOnly -- --fastOpt

The JUnit tests from scala/scala can be run with

    > scalaTestSuite2_12/test

## Metals-based IDEs

We recommend [Metals](https://scalameta.org/metals/)-based IDEs such as VS Code
to develop Scala.js itself. It can import the Scala.js build out-of-the-box.

After importing the build in Metals, you will need to run `clean` in sbt before
normal sbt commands can correctly work. Metals will continue to provide all its
features after cleaning.

## Eclipse

If you want to develop in Eclipse, use
[sbteclipse](https://github.com/typesafehub/sbteclipse). Projects as created by
the build by default are not suited for Eclipse. You can create *somewhat*
appropriate projects with:

    $ GENERATING_ECLIPSE=true sbt "eclipse with-source=true"

You will still have to fix a few things:

* Uncheck the "Allow output directories per source directory" in Build path
* Add transitive project dependencies in Build path

## Preparing a Pull Request

One common build failure is code styling. Reproduce results locally with:

   $ sbt scalastyleCheck

## Organization of the repository

The repository is organized as follows:

### Compilation pipeline

* `ir/` The Intermediate Representation, produced by the compiler and consumed by the linker
* `compiler/` The scalac compiler plugin
* `linker-private-library/` Some Scala.js files whose compiled .sjsir files are used as resources of the linker (2.12 only)
* `linker-interface/` The linker interface, without its implementation
* `linker/` The linker, optimizer, verifier, etc.: everything that happens at link time

### Library

* `library/` The Scala.js standard library (everything inside `scala.scalajs.*`)
* `javalanglib/` Implementation in Scala.js of the classes in `java.lang.*`
* `javalib/` Implementation in Scala.js of other classes in `java.*`
* `scalalib/` Almost void project for recompiling the Scala library for Scala.js
* `library-aux/` A few files of the Scala library that need to be compiled separately

All of these are packaged in `scalajs-library.jar`.

### Testing infrastructure

There is a generic infrastructure that maps the sbt-testing-interface API
across the JVM/JS boundary, so that Scala.js testing frameworks can be piloted
from JVM processes such as sbt.

* `test-interface/` the JS definition of the sbt-testing-interface API
* `test-bridge/` JS side of the bridge
* `test-adapter/` JVM side of the bridge
* `test-common/` Code common between `test-bridge` and `test-adapter`

This repository also contains a specific implementation of JUnit:

* `junit-runtime/` The run-time library for JUnit
* `junit-plugin/` The JUnit compiler plugin

### sbt plugin

* `sbt-plugin/` The sbt plugin itself (2.12 only)

### Testing projects

* `test-suite/` The main test suite of Scala.js
* `test-suite-ex/` Additional tests
* `partest-suite/` The partest suite of Scala
* `scala-test-suite/` The JUnit suite of Scala

### Example projects/sandboxes

* `examples/helloworld/` A simple Hello World, typically used as sandbox for quick testing
* `examples/reversi/` The historical Reversi demo - we use it to track the impact of changes on the emitted code size
* `examples/testing/` A simple project with tests using the DOM, mostly used to test `testHtml` with DOM interaction

The helloworld and reversi also have HTML pages to run them in real browsers.

### The build itself

The build itself contains the entire sbt plugin (and all its dependencies) as
part of its sources.
If you change any of the linker interface, linker,
test adapter, or the sbt plugin itself, chances are you need to `reload` the
build for your changes to take effect.

## Publish locally

To publish your changes locally to be used in a separate project, use the
following incantations.
`SCALA_VERSION` refers to the Scala version used by the separate project.

    > ;ir2_12/publishLocal;linkerInterface2_12/publishLocal;linker2_12/publishLocal;testAdapter2_12/publishLocal;sbtPlugin2_12/publishLocal;javalib/publishLocal;javalibintf/publishLocal
    > ;library2_12/publishLocal;testInterface2_12/publishLocal;testBridge2_12/publishLocal;jUnitRuntime2_12/publishLocal;jUnitPlugin2_12/publishLocal;scalalib2_12/publishLocal
    > ++SCALA_VERSION compiler2_12/publishLocal

If using a non-2.12.x version for the Scala version, the `2_12` suffixes must be adapted in the second and third command (not in the first command).
