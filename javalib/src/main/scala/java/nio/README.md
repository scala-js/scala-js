Design of java.nio.Buffers
==========================

The subclasses of `java.nio.Buffer` all have very similar APIs, but copy-pasted
from one class to the other so that there is no boxing. This is extremely
annoying when implementing them. We want to implement the APIs and algorithms
in a generic way, but without sacrificing performance.

Moreover, there are several implementation strategies for each kind of buffer:

* Heap buffer, backed by an `Array[ElementType]`
* Heap byte buffer view, backed by an `Array[Byte]`
* Typed-array buffer, backed by one of the JavaScript Typed Arrays subclasses
* Data-view buffers, backed by a JavaScript `DataView`

Even though the implementation strategies are very different, they still share
a lot in terms of error detection and high-level algorithms.

To implement all of the combinations of data type x implementation strategy
without a huge amount of duplication, we use very generic implementations of
things, supported by Scala's ability to deal with arrays of generic types, plus
typeclasses.

Naively, such a strategy would be extremely slow, because of all the overhead
of boxing, generic array dispatching, typeclass virtual calls, etc. Hence, the
implementations rely heavily on the ability of the Scala.js optimizer to
inline and optimize away all these things.

## Generic accessors in `Buffer`

In the JDK API, `Buffer` only exposes a few methods: those that are not
dependent on the buffer's element type. In this implementation, we add
`private[nio]` generic accessors to basically every specialized method. This
is possible because the Scala type system is powerful enough.

These accessors can in theory be used from generic algorithms to avoid
repetition. However, since they are generic, using them as is would cause
boxing and, worse, generic array dispatch at runtime.

They are therefore meant to be used only as generic bridges to be eventually
specialized by the optimizer.

## `GenXXX` helpers

The `GenXXX` value classes provide the implementation of all the algorithms
wrt. to the implementation strategy. They are generic in the element type.
`GenBuffer` is the most generic of them all, and provides implementations of
some methods that work for all implementation strategies.

Again, they would naively cause boxing and generic array dispatch. So they
should also be used only at call sites where they can be inlined and
specialized by the optimizer.

The implementations of `GenXXX` classes use the generic accessors defined in
`Buffer`.

## `load` and `store` methods

The `get` and `put` set of methods exposed by the JDK API have to deal with all
sorts of error condition checking. Once inside, they should be able to call
other buffers without the extra overhead of these conditions.

This is the purpose of the `load` and `store` set of methods. They basically
have the same purpose as their absolute `get` and `set` equivalents, but do not
check for any errors.

## Actual implementations

With all of this set up, we can implement the actual classes of the buffers,
for every data type and every implementation strategy. In these classes,
methods are all one-liners because they call into one of the `GenXXX` helpers.

At this point, the precise implementation strategy and element type are known
to be monomorphic. The typeclasses are also materialized there and hence known
as well. Therefore, the compiler can inline everything and specialize according
to this knowledge.

Eventually, nothing remains of the abstractions.

## So what?

All this means that there is a reason for everything. Although the algorithms
are generic, they are also tighlty coupled with the knowledge that the
optimizer will be able to remove the genericity.

*Every change must therefore be handled with care, and the emitted JavaScript
should be checked manually to detect regressions!*

Some things to keep in mind:

* When inlining is involved, a polymorphic method returning a constant is
  better than a `val`: the method can be inlined away and the value
  constant-folded. This is not true with `val`s.
* Methods cannot be *implemented* in `Buffer` if they reference the
  `ElementType` or any other type member of `Buffer`. They must be implemented
  in each of the subclasses, even if textually the code is the same in all of
  them. At this point, the optimizer can specialize, but not if the
  implementation is in `Buffer`.
