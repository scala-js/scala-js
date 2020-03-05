# Versioning

This page describes how we version Scala.js core. Notably what compatibility
guarantees we give with respect to the version numbering.

# Major Changes

The following changes must cause a major version bump.

* Backward incompatible change in the IR
* Backward binary incompatible change in the standard library
* Backward incompatible change in the contract for calling JDK APIs

# Severe Changes

Severe changes can break the ecosystem of sbt plugins and other build tools, but
not the ecosystem of libraries (which would be major). Severe changes should be
done only if absolutely necessary. The following are considered severe changes:

* Backward binary incompatible changes in `logging.*`, `linker.interface.*` or
  `sbtplugin.*`
* Backward binary incompatible changes in `jsenv.*` or `testadapter.*`

Severe changes are difficult from a versioning point of view, since they require
a careful tradeoff:

* if a major bump is made, it forces libraries to re-publish unnecessarily
  (because the IR is not actually affected).
* if no major bump is made, the tooling API versioning breaks the SemVer
  contract.

As such, we leave the best course of action in case of severe changes to the
maintainers. Possible courses of action are:

* Major version bump
* Minor version bump
* Separating versioning of IR and tooling.

# Minor Changes

The following changes must cause a minor version bump.

* Forward incompatible change in the IR
* Backward source incompatible change at the language level or at the standard
  library level (including any addition of public API in the stdlib)
* Backward source incompatible change in `logging.*`, `linker.interface.*` or
  `sbtplugin.*` (including any addition of public API)
* Backward source incompatible changes in `jsenv.*` or `testadapter.*`
* Backward binary incompatible change in `ir.*`, `linker.interface.unstable.*`,
  `linker.*` or `linker.stable.*`

# Patch Changes

All other changes cause a patch version bump only. Explicitly (but not
exhaustively):

* Backward source incompatible change in `ir.*`, `linker.interface.unstable.*`,
  `linker.*` or `linker.stable.*`
* Backward source/binary incompatible changes elsewhere in `linker.**`
* Fixes or additions in the JDK libs (since they are always backward source and
  binary compatible)
