# Developer documentation

## Very important notice about the Javalib

Scala.js contains a reimplementation of part of the JDK in Scala.js itself.

***To contribute to this code, it is strictly forbidden to even look at the
source code of the Oracle JDK or OpenJDK!***

This is for license considerations: these JDKs are under a GPL-based license,
which is not compatible with our BSD 3-clause license.

It is also recommended *not to look at any other JDK implementation* (such as
Apache Harmony), to minimize the chance of copyright debate.

## Building

Scala.js is entirely built with [sbt](https://www.scala-sbt.org/), and also
requires [Node.js](https://nodejs.org/en/) to be installed. For complete
support, Node.js >= 10.0.0 is required.

The first time, or in the rare events where `package.json` changes
([history](https://github.com/scala-js/scala-js/commits/master/package.json)),
you need to run

    $ npm install

from your shell. If you *really* do not want to do this, you can avoid that
step, but you will need to use
`set MyScalaJSPlugin.wantSourceMaps in testSuite := false` within sbt to
by-pass source-map-related tests. In addition, bootstrap tests will not pass.

Otherwise, everything happens within sbt.

Run the normal test suite using the entire Scala.js toolchain using

    > testSuite/test

In order to test the tests themselves, run the cross-compiling tests on the JVM
with:

    > testSuiteJVM/test

If you have changed the IR or the compiler, you typically need to

    > clean

before testing anew.

If you have changed the IR, the linker, the JS environments, the test adapter
or the sbt plugin, you typically need to

    > reload

To test in fullOpt stage:

    > set scalaJSStage in Global := FullOptStage

There are also a few additional tests in a separate testing project:

    > testSuiteEx/test

The compiler tests (mostly verifying expected compile error messages) can be
run with

    > compiler/test

The full partest suite (tests of the Scala language, ported in Scala.js) are
run with:

    > partestSuite/test

or, more typically,

    > partestSuite/testOnly -- --fastOpt

The JUnit tests from scala/scala can be run with

    > scalaTestSuite/test

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

## Organization of the repository

The repository is organized as follows:

### Compilation pipeline

* `ir/` The Intermediate Representation, produced by the compiler and consumed by the linker
* `compiler/` The scalac compiler plugin
* `io/` Virtual I/O abstractions
* `logging/` A tiny logging API
* `linker/` The linker, optimizer, verifier, etc.: everything that happens at link time

### Library

* `library/` The Scala.js standard library (everything inside `scala.scalajs.*`)
* `javalanglib/` Implementation in Scala.js of the classes in `java.lang.*`
* `javalib/` Implementation in Scala.js of other classes in `java.*`
* `scalalib/` Almost void project for recompiling the Scala library for Scala.js
* `library-aux/` A few files of the Scala library that need to be compiled separately

All of these are packaged in `scalajs-library.jar`.

### JS environments

The JS environments are JVM libraries that abstract the details of using a
JavaScript engine to run JS code.

* `js-envs/` The generic definitions of JavaScript environments and runners
* `nodejs-env/` The Node.js environment

Other JS environments are developed in separate repositories under the
`scala-js` organization.

### Testing infrastructure

There is a generic infrastructure that maps the sbt-testing-interface API
across the JVM/JS boundary, so that Scala.js testing frameworks can be piloted
from JVM processes such as sbt.

* `test-interface/` JS side of the bridge, as well as the JS definition of the
  sbt-testing-interface API
* `test-adapter/` JVM side of the bridge

This repository also contains a specific implementation of JUnit:

* `junit-runtime/` The run-time library for JUnit
* `junit-plugin/` The JUnit compiler plugin

### sbt plugin

* `sbt-plugin/` The sbt plugin itself

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
If you change any of the IR, virtual IO, logging API, linker, JS environments,
test adapter, or the sbt plugin itself, chances are you need to `reload` the
build for your changes to take effect.

## Publish locally

To publish your changes locally to be used in a separate project, use the
following incantations.
`SCALA_VERSION` refers to the Scala version used by the separate project.

    > ++SCALA_VERSION
    > ;compiler/publishLocal;library/publishLocal;testInterface/publishLocal;testBridge/publishLocal;jUnitRuntime/publishLocal;jUnitPlugin/publishLocal
    > ++2.10.7
    > ;ir/publishLocal;io/publishLocal;logging/publishLocal;linker/publishLocal;jsEnvs/publishLocal;jsEnvsTestKit/publishLocal;nodeJSEnv/publishLocal;testAdapter/publishLocal;sbtPlugin/publishLocal
