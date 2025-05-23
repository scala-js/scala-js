# WebAssembly Emitter

This directory contains the WebAssembly Emitter, which takes linked IR and produces WebAssembly files.

The entry point is the class `Emitter`.
Overall, this organization of this backend is similar to that of the JavaScript backend.

This readme gives an overview of the compilation scheme.

## WebAssembly features that we use

* The [GC extension](https://github.com/WebAssembly/gc)
* The [exception handling proposal](https://github.com/WebAssembly/exception-handling)
* The [JS string builtins proposal](https://github.com/WebAssembly/js-string-builtins) (currently with polyfills, so it is not required)

All our heap values are allocated as GC data structures.
We do not use the linear memory of WebAssembly at all.

## Type representation

Since WebAssembly is strongly statically typed, we have to convert IR types into Wasm types.
The full compilation pipeline must be type-preserving: a well-typed IR program compiles into a well-typed Wasm module.

In most cases, we also preserve subtyping: if `S <: T` at the IR level, then `transform(S) <: transform(T)` at the Wasm level.
This is however not true in the following cases:

* When `T = void` and `S ≠ void`, we have to `drop` the value of type `S` from the stack.
* When `S` is a primitive and `T` is a reference type (which must be an ancestor of a hijacked class), we have to "box" the primitive.
  We will come back to this in the [hijacked classes](#hijacked-classes) section.
* When `S = null` and `T = jl.String`, we insert an `extern.convert_from_any` operation.
* When `S ∈ {string, jl.String, jl.String!}` and `T` is none of those, we insert an `any.convert_from_extern` operation.

### Primitive types

| IR type         | Wasm type      | Value representation (if non-obvious)   |
|-----------------|----------------|-----------------------------------------|
| `void`          | no type        | no value on the stack                   |
| `boolean`       | `i32`          | `0` for `false`, 1 for `true`           |
| `byte`, `short` | `i32`          | the `.toInt` value, i.e., sign extended |
| `char`          | `i32`          | the `.toInt` value, i.e., 0-extended    |
| `int`           | `i32`          |                                         |
| `long`          | `i64`          |                                         |
| `float`         | `f32`          |                                         |
| `double`        | `f64`          |                                         |
| `undef`         | `(ref any)`    | the global JavaScript value `undefined` |
| `string`        | `(ref extern)` | a JavaScript `string`                   |

### Reference types

We will describe more precisely the representation of reference types in the coming sections.
This table is for reference.

| IR type                                     | Wasm type                             |
|---------------------------------------------|---------------------------------------|
| `C`, a Scala class                          | `(ref null $c.C)` |                   |
| `I`, a Scala interface                      | `(ref null $c.jl.Object)`             |
| `jl.String`                                 | `(ref null extern)` (aka `externref`) |
| `any` and all ancestors of hijacked classes | `(ref null any)` (aka `anyref`)       |
| `PT[]`, a primitive array                   | `(ref null $PTArray)`                 |
| `RT[]`, any reference array type            | `(ref null $ObjectArray)`             |

Non-nullable variants of the reference types are translated to non-nullable Wasm `ref` types.

### Nothing

Wasm does not have a bottom type that we can express at the "user level".
That means we cannot transform `nothing` into any single Wasm type.
However, Wasm has a well-defined notion of [*stack polymorphism*](https://webassembly.github.io/gc/core/valid/instructions.html#polymorphism).
As far as we are concerned, we can think of a stack polymorphic context as officially dead code.
After a *stack-polymorphic instruction*, such as `br` (an unconditional branch), we have dead code which can automatically adapt its type (to be precise: the type of the elements on the stack) to whatever is required to typecheck the following instructions.

A stack-polymorphic context is as close as Wasm gets to our notion of `nothing`.
Our "type representation" for `nothing` is therefore to make sure that we are in a stack-polymorphic context.

## Object model

### Basic structure

We use GC `struct`s to represent instances of classes.
The structs start with a `vtable` reference, which is followed by user-defined fields.
The declared supertypes of those `struct`s follow the *class* hierarchy (ignoring interfaces).

The `vtable` reference is immutable.
User-defined fields are always mutable as the WebAssembly level, since they are mutated in the constructors.

For example, given the following IR classes:

```scala
class A extends jl.Object {
  val x: int
}

class B extends A {
  var y: double
}
```

We define the following GC structs:

```wat
(type $c.A (sub $c.java.lang.Object (struct
  (field $vtable (ref $v.A))
  (field $f.A.x (mut i32)))
))

(type $c.B (sub $c.A (struct
  (field $vtable (ref $v.B))
  (field $f.A.x (mut i32))
  (field $f.B.y (mut f64)))
))
```

As required in Wasm structs, all fields are always repeated in substructs.
Declaring a parent struct type does not imply inheritance of the fields.

### Methods and statically resolved calls

Methods are compiled into Wasm functions in a straightforward way, given the type transformations presented above.
When present, the receiver comes as a first argument.

Statically resolved calls are also compiled straightforwardly as:

1. Push the receiver, if any, on the stack
2. Push the arguments on the stack
3. `call` the target function

Constructors are considered instance methods with a `this` receiver for this purpose, and are always statically resolved.

For example, given the IR

```scala
class A extends java.lang.Object {
  val A::x: int
  def x;I(): int = {
    this.A::x
  }
  def plus;I;I(y: int): int = {
    (this.x;I() +[int] y)
  }
  constructor def <init>;I;V(x: int) {
    this.A::x = x;
    this.java.lang.Object::<init>;V()
  }
}
```

We get the following implementing functions, assuming all method calls are statically resolved.

```wat
;; getter for x
(func $f.A.x_I
  (param $this (ref $c.A)) (result i32)
  ;; field selection: push the object than `struct.get`
  local.get $this
  struct.get $c.A $f.A.x
  ;; there is always an implicit `return` at the end of a Wasm function
)

;; method plus
(func $f.A.plus_I_I
  (param $this (ref $c.A)) (param $y i32) (result i32)
  ;; call to the getter: push receiver, cast null away, then `call`
  local.get $this
  ref.as_non_null
  call $f.A.x_I
  ;; add `y` to the stack and `i32.add` to add it to the result of the call
  local.get $y
  i32.add
)

;; constructor
(func $ct.A.<init>_I_V
  (param $this (ref $c.A)) (param $x i32)
  ;; this.x = x
  local.get $this
  local.get $x
  struct.set $c.A $f.A.x
  ;; call Object.<init>(this)
  local.get $this
  ref.as_non_null
  call $ct.java.lang.Object.<init>_V
)
```

In theory, the call to the getter should have been a virtual call in this case.
In practice, the optimizer turns virtual calls (`Apply`) into statically resolved calls (`ApplyStatically`) when possible.

### typeData

Metadata about IR classes are reified at run-time as values of the struct type `(ref typeData)`.
Documentation for the meaning of each field can be found in `VarGen.genFieldID.typeData`.

### vtable and virtual method calls

The vtable of our object model follows a standard layout:

* The class meta data, then
* Slots for itable pointers (see below for interface method calls), then
* Function pointers for the virtual methods, from `jl.Object` down to the current class.

vtable structs form a subtyping hierarchy that mirrors the class hierarchy, so that `$v.B` is a subtype of `$v.A`.
This is required for `$c.B` to be a valid subtype of `$c.A`, since their first field is of the corresponding vtable types.

The vtable of `jl.Object` is a subtype of `typeData`, which allows to generically manipulate `typeData`s even when they are not full vtables.
For example, the `typeData` of JS types and Scala interfaces do not have a corresponding vtable.

An alternative would have been to make the vtables *contain* the `(ref typeData)` as a first field.
That would however require an additional pointer indirection on every access to the `typeData`, for no benefit in memory usage or code size.
WebAssembly does not have a notion of "flattened" inner structs: a struct cannot contain another struct; it can only contain a *reference* to another struct.

Given

```scala
class A extends Object {
  def foo(x: int): int = x
}

class B extends A {
  val field: int

  def bar(x: double): double = x
  override def foo(x: int): int = x + this.field
}
```

we get

```wat
(type $v.A (sub $v.java.lang.Object (struct
  ;; ... class metadata
  ;; ... itable slots
  ;; ... methods of jl.Object
  (field $m.foo_I_I (ref $4))
)))

(type $v.helloworld.B (sub $v.A (struct
  ;; ... class metadata
  ;; ... itable slots
  ;; ... methods of jl.Object
  (field $m.foo_I_I (ref $4))
  (field $m.bar_D_D (ref $6))
)))

(type $4 (func (param (ref any)) (param i32) (result i32)))
(type $6 (func (param (ref any)) (param f64) (result f64)))
```

Note that the declared type of `this` in the function types is always `(ref any)`.
If we used the enclosing class type, the type of `$m.foo_I_I` would have incompatible types in the two vtables:

* In `$v.A`, it would have type `(func (param (ref $c.A)) ...)`
* In `$v.B`, it would have type `(func (param (ref $c.B)) ...)`

Since the latter is not a subtype of the former, `$v.B` cannot be a subtype of `$v.A` (recall from earlier that we need that subtyping relationship to hold).

Because we use `(ref any)`, we cannot directly put a reference to the implementing functions (e.g., `$f.A.foo_I_I`) in the vtables: their receiver has a precise type.
Instead, we generate bridge forwarders (the `forTableEntry` methods) which:

1. take a receiver of type `(ref any)`,
2. cast it down to the precise type, and
3. call the actual implementation function (with a tail call, because why not)

The table entry forwarder for `A.foo` looks as follows:

```wat
;; this function has an explicit `(type $4)` which ensures it can be put in the vtables
(func $m.A.foo_I_I (type $4)
  (param $this (ref any)) (param $x i32) (result i32)
  ;; get the receiver and cast it down to the precise type
  local.get $this
  ref.cast (ref $c.A)
  ;; load the other arguments and call the actual implementation function
  local.get $x
  return_call $f.A.foo_I_I ;; return_call is a guaranteed tail call
)
```

A virtual call to `a.foo(1)` is compiled as you would expect: lookup the function reference in the vtable and call it.

### itables and interface method calls

Before the function pointers for virtual call dispatch, each vtable contains the method tables for interface call dispatch.
They have one more level of indirection than for virtual calls.
There are `N` immutable fields of type `structref`, which are the *itable slots*:

```wat
(type $v.A (sub $v.java.lang.Object (struct
  ;; ... class metadata
  (field $itableSlot1 structref)
  ...
  (field $itableSlotN structref)
  ;; ... virtual method pointers
)))
```

As a first approximation, we assign a distinct index to every interface in the program.
The index maps to an itable slot.
At the index of a given interface `Intf`, we find a `(ref $it.Intf)` whose fields are the method table entries of `Intf`.
Like for virtual calls, we use the "table entry bridges" in the itables, i.e., the functions where the receiver is of type `(ref any)`.

For example, given

```scala
interface Intf {
  def foo(x: int): int
  def bar(x: double): double
}

class A extends Intf {
  def foo(x: int): int = x
  def bar(x: double): double = x
}
```

the struct type for `Intf` is defined as

```wat
(type $it.Intf (struct
  (field $m.Intf.bar_D_D (ref $6))
  (field $m.Intf.foo_I_I (ref $4))
))

(type $4 (func (param (ref any)) (param i32) (result i32)))
(type $6 (func (param (ref any)) (param f64) (result f64)))
```

In practice, allocating one slot for every interface in the program is wasteful.
We can use the same slot for a set of interfaces that have no concrete class in common.
This slot allocation is implemented in `Preprocessor.assignBuckets`.

Since Wasm structs only support single inheritance in their subtyping relationships, we have to transform every interface type as `(ref null jl.Object)` (the common supertype of all interfaces).
This does not turn out to be a problem for interface method calls, since they pass through the `itables` struct anyway, and use the table entry bridges which take `(ref any)` as argument.

Given the above structure, an interface method call to `intf.foo(1)` is compiled as expected: lookup the function reference in the appropriate itable slot of the vtable, then call it.

### Reflective calls

Calls to reflective proxies use yet another strategy.
Instead of building structs where each reflective proxy appears at a compile-time-constant slot, we use a search-based strategy.

Each reflective proxy name found in the program is allocated a unique integer ID.
The reflective proxy table of a class is an array of pairs `(id, funcRef)`, stored in the class' `typeData`.
In order to call a reflective proxy, we perform the following steps:

1. Load the `typeData` of the receiver.
2. Search the reflective proxy ID in `$reflectiveProxies` (using the `searchReflectiveProxy` helper).
3. Call it (using `call_ref`).

This strategy trades off efficiency for space.
It is slow, but that corresponds to the fact that reflective calls are slow on the JVM as well.
In order to have fixed slots for reflective proxy methods, we would need an `m*n` matrix where `m` is the number of concrete classes and `n` the number of distinct reflective proxy names in the entire program.
With the compilation scheme we use, we only need an array containing the actually implemented reflective proxies per class, but we pay an `O(log n)` run-time cost for lookup (instead of `O(1)`).

## Hijacked classes

Due to our strong interoperability guarantees with JavaScript, the universal (boxed) representation of hijacked classes must be the appropriate JavaScript values.
For example, a boxed `int` must be a JavaScript `number`.
The only Wasm type that can store references to both GC structs and arbitrary JavaScript `number`s is `anyref` (an alias of `(ref null any)`).
That is why we transform the types of ancestors of hijacked classes to the Wasm type `anyref`.

### Boxing

When an `int` is upcast to `jl.Integer` or higher, we must *adapt* the `i32` into `anyref`.
Doing so is not free, since `i32` is not a subtype of `anyref`.
Even worse, no Wasm-only instruction sequence is able to perform that conversion in a way that we always get a JavaScript `number`.

Instead, we ask JavaScript for help.
We use the following JavaScript helper function, which is defined in `LoaderContent`:

```js
__scalaJSHelpers: {
  bI: (x) => x,
}
```

Huh!? That's an identity function.
How does it help?

The magic is to import it into Wasm with a non-identity type.
We import it as

```wat
(import "__scalaJSHelpers" "bI" (func $bI (param i32) (result anyref)))
```

The actual conversion happens at the boundary between Wasm and JavaScript and back.
Conversions are specified in the [Wasm JS Interface](https://webassembly.github.io/gc/js-api/index.html).
The relevant internal functions are [`ToJSValue`](https://webassembly.github.io/gc/js-api/index.html#tojsvalue) and [`ToWebAssemblyValue`](https://webassembly.github.io/gc/js-api/index.html#towebassemblyvalue).

When calling `$bI` with an `i32` value as argument, on the Wasm spec side of things, it is an `i32.const u32` value (Wasm values carry their type from a spec point of view).
`ToJSValue` then specifies that:

> * If `w` is of the form `i32.const u32`,
>   * Let `i32` be `signed_32(u32)`.
>   * Return 𝔽(`i32` interpreted as a mathematical value).

where 𝔽 is the JS spec function that creates a `number` from a mathematical value.

When that `number` *returns* from the JavaScript "identity" function and flows back into Wasm, the spec invokes `ToWebAssemblyValue(v, anyref)`, which specifies:

> * If `type` is of the form `ref null heaptype` (here `heaptype = any`),
>   * [...]
>   * Else,
>     1. Let `map` be the surrounding agent's associated host value cache.
>     2. If a host address `hostaddr` exists such that `map[hostaddr]` is the same as `v`,
>         * Return `ref.host hostaddr`.
>     3. Let host address `hostaddr` be the smallest address such that `map[hostaddr]` exists is `false`.
>     4. Set `map[hostaddr]` to `v`.
>     5. Let `r` be `ref.host hostaddr`.

Therefore, from a spec point of view, we receive back a `ref.host hostaddr` for which the engine remembers that it maps to `v`.
That `ref.host` value is a valid value of type `anyref`, and therefore we can carry it around inside Wasm.

### Unboxing

When we *unbox* an IR `any` into a primitive `int`, we perform perform the inverse operations.
We also use an identity function at the JavaScript for unboxing an `int`:

```js
__scalaJSHelpers: {
  uI: (x) => x,
}
```

However, we swap the Wasm types of parameter and result:

```wat
(import "__scalaJSHelpers" "uI" (func $uI (param anyref) (result i32)))
```

When the `ref.host hostaddr` enters JavaScript, `ToJSValue` specifies:

> * If `w` is of the form `ref.host hostaddr`,
>   * Let `map` be the surrounding agent's associated host value cache.
>   * Assert: `map[hostaddr]` exists.
>   * Return `map[hostaddr]`.

This recovers the JavaScript `number` value we started with.
When it comes back into WebAssembly, the spec invokes `ToWebAssemblyValue(v, i32)`, which specifies:

> * If `type` is `i32`,
>   * Let `i32` be ? `ToInt32(v)`.
>   * Let `u32` be the unsigned integer such that `i32` is `signed_32(u32)`.
>   * Return `i32.const u32`.

Overall, we use `bI`/`uI` as a pair of round-trip functions that perform a lossless conversion from `i32` to `anyref` and back, in a way that JavaScript code would always see the appropriate `number` value.

Note: conveniently, `ToInt32(v)` also takes care of converting `null` into 0, which is a spec trivia we also exploit in the JS backend.

### Efficiency

How is the above not terribly inefficient?
Because implementations do not actually use a "host value cache" map.
Instead, they pass pointer values as is through the boundary.

Concretely, `ToWebAssemblyValue(v, anyref)` and `ToJSValue(ref.host x)` are no-ops.
The conversions involving `i32` are not free, but they are as efficient as it gets for the target JS engines.

However, going through the Wasm-to-JS interface has a significant cost in itself.
For performance, we want to avoid crossing the boundary whenever possible.
In many cases, we can abuse `i31ref`-related operations to perform the conversions.
Indeed, the Wasm-JS conversion spec also guarantees that a `ref.i31` is seen by JavaScript as the corresponding `number`.

In order to box an `int`, we first test whether it is safe to drop the most significant bit.
That is the case if and only if it is equal to the second-most significant bit.
If it is, we "box" the `int` using an `ref.i31` instruction.
Otherwise, we fall back on the JS helper (which is actually called `$bIFallback`).
For unboxing, we first try a `br_on_cast (ref i31)` instruction, and fall back on `$uIFallback` if it does not succeed.

We use similar strategies for the other primitive types.

### Method dispatch

When the receiver of a method call is a primitive or a hijacked class, the call can always be statically resolved by construction, hence no dispatch is necessary.
For strict ancestors of hijacked classes, we must use a type-test-based dispatch similar to what we do in `$dp_` dispatchers in the JavaScript backend.

## Arrays

Like the JS backend, we define a separate `struct` type for each primitive array type: `$IntArray`, `$FloatArray`, etc.
Unlike the JS backend, we merge all the reference array types in a single `struct` type `$ObjectArray`.
We do not really have a choice, since there is a (practically) unbounded amount of them, and we cannot create new `struct` types at run-time.

All array "classes" follow the same structure:

* They actually extend `jl.Object`
* Their vtable type is the same as `jl.Object`
* They each have their own vtable value for the differing metadata, although their method table entries are all the same
  * This is also true for reference types: the vtables are dynamically created at run-time on first use (they are values and share the same type, so that we can do)
* Their virtual method pointers are the same as in `jl.Object`
* Their itable slots are all the same, with entries for `jl.Cloneable` and `j.io.Serializable`
* They have a unique "user-land" field `$underlyingArray`, which is a Wasm array of its values:
  * For primitives, they are primitive arrays, such as `(array mut i32)`
  * For references, they are all the same type `(array mut anyref)`

Concretely, here are the relevant Wasm definitions:

```wat
(type $i8Array (array (mut i8)))
(type $i16Array (array (mut i16)))
(type $i32Array (array (mut i32)))
(type $i64Array (array (mut i64)))
(type $f32Array (array (mut f32)))
(type $f64Array (array (mut f64)))
(type $anyArray (array (mut anyref)))

(type $BooleanArray (sub final $c.java.lang.Object (struct
  (field $vtable (ref $v.java.lang.Object))
  (field $arrayUnderlying (ref $i8Array))
)))
(type $CharArray (sub final $c.java.lang.Object (struct
  (field $vtable (ref $v.java.lang.Object))
  (field $arrayUnderlying (ref $i16Array))
)))
...
(type $ObjectArray (sub final $c.java.lang.Object (struct
  (field $vtable (ref $v.java.lang.Object))
  (field $arrayUnderlying (ref $anyArray))
)))
```

Given the above layout, reading and writing length and elements is straightforward.
The only catch is reading an element of a reference type that is more specific than `jl.Object[]`.
In that case, we must `ref.cast` the element down to its transformed Wasm type to preserve typing.
This is not great, but given the requirement that reference array types be (unsoundly) covariant in their element type, it seems to be the only viable encoding.

The indirection to get at `$arrayUnderlying` elements is not ideal either, but is no different than what we do in the JS backend with the `u` field.
In the future, Wasm might provide the ability to [nest an array in a flat layout at the end of a struct](https://github.com/WebAssembly/gc/blob/main/proposals/gc/Post-MVP.md#nested-data-structures).

## Order of definitions in the Wasm module

For most definitions, Wasm does not care in what order things are defined in a module.
In particular, all functions are declared ahead of time, so that the order in which they are defined is irrelevant.

There are however some exceptions.
The ones that are relevant to our usage of Wasm are the following:

* In a given recursive type group, type definitions can only refer to types defined in that group or in previous groups (recall that all type definitions are part of recursive type groups, even if they are alone).
* Even within a recursive type group, the *supertype* of a type definition must be defined before it.
* The initialization code of `global` definitions can only refer to other global definitions that are defined before.

For type definitions, we use the following ordering:

1. Definitions of the underlying array types (e.g., `(type $i8Array (array (mut i8)))`)
2. The big recursive type group, with:
   1. Some types referred to from `$typeData`, in no particular order.
   2. The `$typeData` struct definition (it is a supertype of the vtable types, so it must come early).
   3. For each Scala class or interface in increasing order of ancestor count (the same order we use in the JS backend), if applicable:
      1. Its vtable type (e.g., `$v.java.lang.Object`)
      2. Its object struct type (e.g., `$c.java.lang.Object`)
      3. Its itable type (e.g., `$it.java.lang.Comparable`)
   4. Function types appearing in vtables and itables, interspersed with the above in no particular order.
   5. The `$XArray` struct definitions (e.g., `$BooleanArray`), which are subtypes of `$c.java.lang.Object`.
3. All the other types, in no particular order, among which:
   * Function types that do not appear in vtables and itables, including the method implementation types and auto-generated function types for block types
   * Closure data struct types

For global definitions, we use the following ordering:

1. The typeData of the primitive types (e.g., `$d.I`)
2. For each linked class, in the same ancestor count-based order:
   1. In no particular order, if applicable:
      * Its typeData/vtable global (e.g., `$d.java.lang.Object`), which may refer to the typeData of ancestors, so the order between classes is important
      * Static field definitions
      * Definitions of `Symbol`s for the "names" of private JS fields
      * The module instance
      * The cached JS class value
3. Cached values of boxed zero values (such as `$bZeroChar`), which refer to the vtable globals of the box classes

## Miscellaneous

### Object instantiation

An IR `New(C, ctor, args)` embeds two steps:

1. Allocate a new instance of `C` with all fields initialized to their zero
2. Call the given `ctor` on the new instance

The second step follows the compilation scheme of a statically resolved method call, which we saw above.
The allocation itself is performed by a `$new.C` function, which we generate for every concrete class.
It looks like the following:

```wat
(func $new.C
  (result (ref $c.C))

  global.get $d.C  ;; the global vtable for class C
  i32.const 0      ;; zero of type int
  f64.const 0.0    ;; zero of type double
  struct.new $c.C  ;; allocate a $c.C initialized with all of the above
)
```

It would be nice to do the following instead:

1. Allocate a `$c.C` entirely initialized with zeros, using `struct.new_default`
2. Set the `$vtable` field

This would have a constant code size cost, irrespective of the amount of fields in `C`.
Unfortunately, we cannot do this because the `$vtable` field is immutable.

We cannot make it mutable since we rely on covariance (which only applies for immutable fields) for class subtyping.
Abandoning this would have much worse consequences.

Wasm may evolve to have [a more flexible `struct.new_default`](https://github.com/WebAssembly/gc/blob/main/proposals/gc/Post-MVP.md#handle-nondefaultable-fields-in-structnew_default), which would solve this trade-off.

### Clone

The IR node `Clone` takes an arbitrary instance of `jl.Cloneable` and returns a shallow copy of it.
Wasm does not have any generic way to clone a reference to a `struct`.
We must statically know what type of `struct` we want to clone instead.

To solve this issue, we add a "magic" `$clone` function pointer in every vtable.
It is only populated for classes that actually extend `jl.Cloneable`.
We then compile a `Clone` node similarly to any virtual method call.

Each concrete implementation `$clone.C` statically knows its corresponding `$c.C` struct type.
It can therefore allocate a new instance and copy all the fields.

### Identity hash code

We implement `IdentityHashCode` in the same way as the JS backend:

* We allocate one global `WeakMap` to store the identity hash codes (`idHashCodeMap`)
* We allocate identity hash codes themselves by incrementing a global counter (`lastIDHashCode`)
* For primitives, which we cannot put in a `WeakMap`, we use their normal `hashCode()` method

This is implemented in the function `identityHashCode` in `CoreWasmLib`.

### Strings

As mentioned above, strings are represented as JS `string`s.
Primitive operations on strings are implemented using [JS String Builtins](https://github.com/WebAssembly/js-string-builtins).
Since the latter are not yet fully supported, notably by Safari, we polyfill the functions we use.
Some conversions to strings, which are part of the semantics of string concatenation, are performed by helper JS functions.

For string constants, we use the builtin imports for string literals offered by the JS string builtins proposal.
This works by configuring a special module name from which strings can be imported.
The import name is the string value.

That means that only valid Unicode strings can be imported that way (import names must be valid UTF-8).
For string constants that are not valid Unicode strings, we generate a dedicated dictionary from our JavaScript loader, in yet another (regular) imported module.

We polyfill the string constants module with a JavaScript `Proxy`.

## JavaScript interoperability

The most difficult aspects of JavaScript interoperability are related to hijacked classes, which we already mentioned.
Other than that, we have:

* a number of IR nodes with JS operation semantics (starting with `JS...`),
* closures, and
* non-native JS classes.

### JS operation IR nodes

For most IR nodes that implement JS operations, we generate dedicated JS helper functions that we call from Wasm.
For example, a node `JSMethodApply(receiver, method, List(arg1, arg2))` is implemented as a call to a helper that looks like:

```js
__scalaJSCustomHelpers: {
  "1": (o, m, arg1, arg2) => o[m](arg1, arg2),
}
```

The `receiver`, `method`, `arg1` and `arg2` are, in general, evaluated on the Wasm side and boxed to `any`.

Often, some of the inputs of these nodes are literals, or other trees that can be evaluated on the JS side.
For example, the `method` is very often a `StringLiteral`.
The receiver might be a `JSGlobalRef` or `LoadJSModule` to a native JS module.

For other nodes, we have to evaluate them on the Wasm side, but the boxing operation can still often be fused with the built-in Wasm-to-JS interoperability semantics.

For example, consider the following Scala.js source code:

```scala
val x: Double = ...
val y: Double = js.Math.cos(x)
```

At the IR level, the call to `js.Math.cos` looks as follows:

```scala
AsInstanceOf(
  JSMethodApply(
    LoadJSModule("js.Math"),
    StringLiteral("cos"),
    List(VarRef("x")(DoubleType))
  ),
  DoubleType
)
```

We can evaluate the `LoadJSModule` and the `StringLiteral` entirely on the JS side.
We have to evaluate `x` on the Wasm side, but we can declare the JS helper as taking an `f64`, which will automatically box the value.
We therefore produce the following JS helper:

```js
__scalaJSCustomHelpers: {
  "2": (x) => Math.cos(x),
}
```

and give it the type `[f64] -> [anyref]` at the Wasm level.

So far, we still have to perform the unboxing operation of the result on the Wasm side, as an additional step.
In the future, we should try to fuse it as well, so that the type of the helper becomes `[f64] -> [f64]`.

### Closures

Wasm can create a function reference to any Wasm function with `ref.func`.
Such a function reference can be passed to JavaScript and will be seen as a JS function.
However, it is not possible to create *closures*; all the arguments to the Wasm function must always be provided.

In order to create closures, we reify captures as a `__captureData` argument to the Wasm function.
It is a reference to a `struct` with values for all the capture params of the IR `Closure` node.
We allocate that struct when creating the `Closure`, then pass it to a JS helper, along with the function reference.
The JS helper then creates an actual closure from the JS side and returns it to Wasm.

We generate a dedicated JS helper for each `Closure` node, in order to cater for the particular amount of parameters and to keep things as monomorphic as possible.
For example, given a `Closure` node taking 2 non-rest parameters, we generate the following helper:

```js
__scalaJSCustomHelpers: {
  "1": (f, data) => (x1, x2) => f(data, x1, x2),
}
```

and we import it in Wasm with the signature `[(ref func), anyref] -> [(ref any)]`.

### Non-native JS classes

For non-native JS classes, we take the above approach to another level.
We generate a dedicated helper for every non-native JS class in the codebase, to define all the fields and methods.
Like for JS interop IR nodes, we fuse as many inputs (typically the names of fields and methods, as well as the initial values of fields) into the generated JS helpers.

The bodies of the methods, getters and setters call dedicated `ref.func`s.
They work similarly to `Closure`s, although the `data` is always the same class captures data struct.

The generated constructor then looks as follows:

```js
__scalaJSCustomHelpers: {
  createJSClass: (data, superClass, preSuperStats, superArgs, postSuperStats, ...otherStuff) => {
    class cls extends superClass {
      constructor(arg1, arg2, ..., argN) {
        var preSuperEnv = preSuperStats(data, new.target, arg1, arg2, ..., argN);
        super(...superArgs(data, preSuperEnv, new.target, arg1, arg2, ..., argN));
        Object.defineProperty(this, "someField", {
          value: 0, // zero of the type, as a JS (boxed) value
          configurable: true,
          enumerable: true,
          writable: true,
        });
        ... // other fields
        postSuperStats(data, preSuperEnv, new.target, this, arg1, arg2, ..., argN);
      }
    };
    return cls;
  },
}
```

Boxed `char`s and `long`s cannot be synthesized on the JS side, and are therefore passed as inputs to the JS helper.
Likewise for the `superClass` and member names, if they are not constants/globals.

Since the `super()` call must lexically appear in the `constructor` of the class, we have to decompose the body of the constructor into 3 functions:

* `preSuperStats` contains the statements before the super call, and returns an environment of the locally declared variables as a `struct` (much like capture data),
* `superArgs` computes an array of the arguments to the super call, and
* `postSuperStats` contains the statements after the super call.

The latter two take the `preSuperEnv` environment computed by `preSuperStats` as parameter.
All functions also receive the class captures `data` and the value of `new.target`.

In the future, we may still want to improve a bit on this scheme.
In particular, we should be able to return multiple values from `superArgs`, one for each argument.
Wasm functions that return multiple Wasm values are exposed to JS as returning a JS array of the results.
We can then statically know the number of elements of that array and how to pass them to the super constructor, avoiding the `...spread` operator.

Furthermore, we could fuse `preSuperStats` and `superArgs` in a single Wasm function, returning both the `preSuperEnv` and the arguments to the super constructor call.

## Exceptions

In Wasm, exceptions consist of a *tag* and a *payload*.
The tag defines the signature of the payload, and must be declared upfront (either imported or defined within Wasm).
Typically, each language defines a unique tag with a payload that matches its native exception type.
For example, a Java-to-Wasm compiler would define a tag `$javaException` with type `[(ref jl.Throwable)]`, indicating that its payload is a unique reference to a non-null instance of `java.lang.Throwable`.

In order to throw an exception, the Wasm `throw` instruction takes a tag and arguments that match its payload type.
Exceptions can be caught in two ways:

* A specific `catch` with a given tag: it only catches exceptions thrown with that tag, and extracts the payload value.
* A catch-all: it catches all exceptions, but the payloads cannot be observed.

Each of those cases comes with a variant that captures an `exnref`, which can be used to re-throw the exception with `throw_ref`.

For Scala.js, our exception model says that we can throw and catch arbitrary values, i.e., `anyref`.
Moreover, our exceptions can be caught by JavaScript, and JavaScript exceptions can be caught from Scala.js.

JavaScript exceptions are reified in Wasm as exceptions with a special tag, namely `WebAssembly.JSTag`, defined in the JS API.
Wasm itself does not know that tag, but it can be `import`ed.
Its payload signature is a single `externref`, which is isomorphic to `anyref` (there is a pair of Wasm instructions to losslessly convert between them).

Instead of defining our own exception tag, we exclusively use `JSTag`, both for throwing and catching.
That makes our exceptions directly interoperable with JavaScript at no extra cost.
The import reads as

```wat
(import "__scalaJSHelpers" "JSTag" (tag $exception (param externref)))
```

Given the above, `Throw` and `TryCatch` have a straightforward implementation.

For `TryFinally`, we have to compile it down to a try-catch-all, because Wasm does not have any notion of `try..finally`.
That compilation scheme is very complicated.
It deserves an entire dedicated explanation, which is covered by the big comment in `FunctionEmitter` starting with `HERE BE DRAGONS`.
