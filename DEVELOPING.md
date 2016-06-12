# Developer documentation

## Very important notice about the Javalib

Scala.js contains a reimplementation of part of the JDK in Scala.js itself.

***To contribute to this code, it is strictly forbidden to even look at the
source code of the Oracle JDK or OpenJDK!***

This is for license considerations: these JDKs are under a GPL-based license,
which is not compatible with our BSD 3-clause license.

The only existing JDK source code that we can look at is the dead Apache
Harmony project.

## Building

Scala.js is entirely built with [sbt](http://www.scala-sbt.org/).
To build a local version of the compiler and standard library, run

    > library/package

To test your changes, run

    > testSuite/test

or test your tests on the JVM.

    > testSuiteJVM/test

If you have changed the IR or the compiler, you typically need to

    > clean

before testing anew.
If you have changed the IR, the tools, the JS environments or the sbt plugin,
you typically need to

    > reload

To test with Node.js instead of Rhino, use the usual Scala.js setting:

    > set scalaJSUseRhino in Global := false

and to run in fullOpt stage:

    > set scalaJSStage in Global := FullOptStage

When running with Node.js, by default, the test suite requires the
`source-map-support` package to be installed in `npm`. You can bypass the
source map tests locally with this setting:

    > set postLinkJSEnv in testSuite := NodeJSEnv().value.withSourceMap(false)

To test with PhantomJS, use this setting:

    > set inScope(ThisScope in testSuite)(Seq(postLinkJSEnv := PhantomJSEnv().value))

The tests for the javalibEx are in a separate testing project:

    > javalibExTestSuite/test

The compiler tests (mostly verifying expected compile error messages) can be
run with

    > compiler/test

The full partest suite (tests of the Scala language, ported in Scala.js) are
run with:

    > partestSuite/test

or, more typically,

    > partestSuite/testOnly -- --fastOpt

## Eclipse

If you want to develop in Eclipse, use
[sbteclipse](https://github.com/typesafehub/sbteclipse). Projects as created by
the build by default are not suited for Eclipse. You can create *somewhat*
appropriate projects with:

    $ sbt tools/sources
    $ GENERATING_ECLIPSE=true sbt "eclipse with-source=true"

You will still have to fix a few things:

* Uncheck the "Allow output directories per source directory" in Build path
* Add transitive project dependencies in Build path

## Organization of the repository

The repository is organized as follows:

### Compilation pipeline

* `ir/` The Intermediate Representation, produced by the compiler and consumed by the linker
* `compiler/` The scalac compiler plugin
* `tools/` The linker, optimizer, verifier, etc.: everything that happens at link time

### Library

* `library/` The Scala.js standard library (everything inside `scala.scalajs.*`)
* `javalanglib/` Implementation in Scala.js of the classes in `java.lang.*`
* `javalib/` Implementation in Scala.js of other classes in `java.*`
* `javalib-ex/` Some more Java classes with non-standard dependencies
* `scalalib/` Almost void project for recompiling the Scala library for Scala.js
* `library-aux/` A few files of the Scala library that need to be compiled separately

All of these, except `javalib-ex`, are packaged in `scalajs-library.jar` as part
of `library/package`.

### sbt plugin

Note that the sbt plugin depends on the IR and the tools.

* `js-envs/` The JavaScript environments and runners (Rhino, Node.js and PhantomJS)
* `sbt-plugin/` The sbt plugin itself

### Testing projects

* `test-suite/` The main test suite of Scala.js
* `javalib-ex-test-suite/` The test suite for the javalib-ex
* `partest-suite/` The partest suite of Scala

### Example projects/sandboxes

* `examples/helloworld/` A simple Hello World, typically used as sandbox for quick testing
* `examples/reversi/` The historical Reversi demo - we use it to track the impact of changes on the emitted code size
* `examples/testing/` A simple project with tests using the DOM, mostly used to test the support for the DOM in Rhino

These example projects also have HTML pages to run them in real browsers.

### The build itself

The build itself contains the entire sbt plugin (and all its dependencies) as
part of its sources.
If you change any of the IR, the tools, the JS environments or the sbt plugin,
chances are you need to `reload` the build for your changes to take effect.

## Publish locally

To publish your changes locally to be used in a separate project, use the
following incantations.
`SCALA_VERSION` refers to the Scala version used by the separate project.

    > ++SCALA_VERSION
    > ;compiler/publishLocal;library/publishLocal;javalibEx/publishLocal;testInterface/publishLocal;stubs/publishLocal;jUnitRuntime/publishLocal;jUnitPlugin/publishLocal
    > ++2.10.6
    > ;ir/publishLocal;tools/publishLocal;jsEnvs/publishLocal;jsEnvsTestKit/publishLocal;testAdapter/publishLocal;sbtPlugin/publishLocal
