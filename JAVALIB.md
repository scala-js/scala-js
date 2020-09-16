# Javalib documentation

## Very important notice about the Javalib

Scala.js contains a reimplementation of part of the JDK in Scala.js itself.

***To contribute to this code, it is strictly forbidden to even look at the
source code of the Oracle JDK or OpenJDK!***

This is for license considerations: these JDKs are under a GPL-based license,
which is not compatible with our Apache 2.0 license.

It is also recommended *not to look at any other JDK implementation* (such as
Apache Harmony), to minimize the chance of copyright debate.

## What goes into the core Scala.js javalib

Parts of the JDK are in Scala.js itself, parts are in separate projects
(see below for examples). This section aims to provide some guidance
on when an implementation should be included in the core repo as
opposed to a separate repo. The guidance is (unfortunately) imprecise
and the decision of the core maintainers applies in case of a
disagreement.

To determine whether a JDK API should be part of Scala.js itself,
traverse the following criteria in order until a decision is reached.

### Does Scala.js core itself depend on the API?

If yes, include it in core.

Examples:
- `java.nio.charset._`
- `java.io.DataOutputStream`

### Does the API need to be implemented in core Scala.js?

If yes, include it in core.

Examples:
- `java.nio.*Buffer` (for typedarray integration)
- `java.lang.Object`

### Can the API be implemented independent of the JS runtime?

Does the implementation only rely on standardized ES 2015 or does it
require some browser/Node.js/etc.-specific support?

If no (i.e. it requires specific support), put it in a separate repo.

Examples:
- Removal of `javalib-ex` that depended on `jszip`.

### Does the core team have the expertise to maintain the API?

If no, put it in a separate repo.

Examples:
- `java.util.Locale` / `java.text._` (https://github.com/cquiroz/scala-java-locales)
- `java.time._` (https://github.com/cquiroz/scala-java-time,
  https://github.com/zoepepper/scalajs-jsjoda,
  https://github.com/scala-js/scala-js-java-time)

### Is the core team willing to take the maintenance burden?

If no, put it in a separate repo.

Examples:
- `java.logging._` (https://github.com/scala-js/scala-js-java-logging)

###  Versioning / Release Frequency / Quality

Is the versioning (i.e. pre-relese v.s. stable) and release frequency
of the core artifacts appropriate for the API?

Are the quality expectations of the core repo appropriate for the
intended implementation?

Is faster iteration than can be provided by the core repo needed?

If yes, yes, no, put it in the core repo, otherwise, put it in a
separate repo.
