# Coding Style Guide

The Scala.js project has a strict policy regarding coding style.
This is one of the cornerstones that has allowed Scala.js to maintain a clean, consistent and maintainable codebase.

This document tries to document the style in use as much as possible to make it easier for everyone to contribute.

A *few* of these rules are checked automatically using Scalastyle, but most of them are too complex to teach to an automated tool.

The Scala.js core team has decided to designate a single developer to drive and "maintain" the project's coding style.
This allows us to maintain a consistent, yet flexible style.
Everyone in the core team follows the style maintainer's directions.
Currently [@sjrd](https://github.com/sjrd) maintains the Scala.js coding style.
Please follow his directions if they are in conflict with the present document.
Feel free to point out the conflict, though: we are always looking to improve this document.


## General style (whitespaces, braces, etc.)

### Tabs, new lines, and eofs

* Files must not contain tabs
* New lines are UNIX style (\n)
* A final new line must be present at the end of file
* There must not be whitespace characters at the end of a line

### Line length

Lines should be limited at 80 characters.
In some cases, if breaking a line makes it significantly less readable, it can go up to 120 characters.

Rationale: when reviewing on GitHub, only 120 characters are visible; when reviewing on a mobile phone, only 80 characters are visible.
And we do review on mobile phone quite a lot.

#### Where to break a line

A line can be broken after either a `,` or a `(`, or possibly after a binary operator in a long expression.

### Indentation

In general, indentation is 2 spaces, except continuation lines, which are indented 4 spaces.
A continuation line is a line appearing because we broke something that should have been on one line for line length reasons.
Typically, this means inside parentheses (formal or actual parameters of methods and constructors), and a long `extends` clause.

Note that breaking a line right after the `=` sign of an initialization or assignment is *not* considered a continuation line, because it's not really breaking the line: instead, we just choose to put the rhs on its dedicated line, indented 2 spaces (similarly to the branches of an `if`).

### Blank lines

* Never put two blank lines in a row
* (Almost) always put a blank line between two declarations in a class
* Always put blank lines around a `case` whose body spans several lines
* Insert blank lines at will between logical blocks of statements in a method

The blank line between two consecutive declarations in a class can sometimes be omitted, if the declarations are single-line (which also means ScalaDocless) and strongly related.
This happens pretty rarely (mostly a series of private fields).
The rule of thumb is to always put a blank line.

### Braces

Braces should not be used for one-liner methods.
Similarly, you should not use braces for the body of a `while`/`if`/`for` that is a one-liner.

```scala
def plus(x: Int, y: Int): Int =
  x + y

def clamp(v: Int, min: Int, max: Int): Int = {
  if (v < min) min
  else if (v > max) max
  else v
}
```

Every expression that spans more than one line should be enclosed in braces.
For example, this is wrong:

```scala
val iterator =
  new Iterator[A] {
    ...
  }
```

It should be:

```scala
val iterator = {
  new Iterator[A] {
    ...
  }
}
```

#### Two-liner if/else

As an exception to the above rule, when a two-liner `if/else` (see section about `if/else` in general) is used as the right-hand-side of a definition or assignment, i.e., after an `=` sign, the braces around it can and should be omitted.
For example:

```scala
def abs(x: Int): Int =
  if (x >= 0) x
  else -x
```

Note that the following formatting is not considered a two-liner if/else, and is therefore not valid:

```scala
def abs(x: Int): Int =
  if (x >= 0)
    x
  else
    -x
```

#### Long expressions with binary operators

Very long expressions consisting of binary operators at their "top-level" can be broken *without indentation* if they are alone in their brace-delimited block.
This happens mostly for long chains of `&&`s, `||`s, or string concatenations.
Here is an example:

```scala
val isValidIdent = {
  ident != "" &&
  ident.charAt(0).isUnicodeIdentifierStart &&
  ident.tail.forall(_.isUnicodeIdentifierPart)
}
```

#### Braces in lambdas

In lambdas (anonymous functions), the opening brace must be placed before the formal arguments, and not after the `=>`:

```scala
val f = { (x: Int) =>
  body
}
```

If the first line ends up being two long, the parameter list should go the next line, and the body indented with two more spaces:

```scala
val someLongIdentifierWithHighIdentation = {
  (x: Int, ys: List[Traversable[String]]) =>
    body
}
```

### Spaces

There must not be any space before the following tokens: `:` `,` `;` `)`

There must be exactly one space after the following tokens: `:` `,` `;` `if` `for` `while`
Sometimes, it is acceptable to have several spaces, for vertical alignment reasons.

There must be exactly one space before and after `{` and `}`.
With the exception of partial import, where there is no space on either side.

Binary operators, including `=>`, must have a single space on both sides.
Sometimes, spaces can be removed to highlight the relatively higher priority wrt. to a neighboring operator, for easier visual parsing.
For example, instead of `x < len - 1`, it is better to write `x < len-1`, highlighting that `-` has a higher priority than `<`.

Unary operators must not be followed by a space.

### Method call style

Usually, parentheses should be used for actual parameters to a method call.
Braces should be used instead if an argument list has only a lambda, and that lambda does not fit in an inline one-liner.

In general, dot-notation should be used for non-symbolic methods, and infix notation should be used for symbolic methods.
Infix notation is also used if the only argument is a brace lambda.

Examples:

```scala
// inline lambda, hence (), hence dot-notation
list.map(x => x * 2)

// long lambda, hence braces, hence infix notation
list map { x =>
  if (x < 5) x
  else x * 2
}

// symbolic operator, hence infix notation
value :: list
```

Using dot-notation with a brace lambda is possible to force priorities.
This is typically the case if the call is chained to a parameterless method call, as in

```scala
list.map { x =>
  // complicated stuff
}.toMap
```

When calling a method declared with an empty pair of parentheses, use `()`, except if the method is Java-defined *and* does not have side-effects.

### Method definition

All public and protected methods must have an explicit result type.

Procedure syntax must not be used.
`: Unit =` must be used instead.

Side-effect-free methods without formal parameters should be declared without `()`, unless either a) it overrides a method defined with `()` (such as `toString()`) or b) it implements a Java method in the Java libraries.

The signature of a method is technically a single line, and hence, if it has to be broken due to line length reasons, subsequent lines should be indented 4 spaces.
As a reminder, the line can be broken right after a `,` or a `(` (and not, for example, after `implicit` or `:`).
You should avoid breaking the line between the last parameter and the result type; going over the 80 characters limit is preferred in that case.

### `for` comprehensions

`for` comprehensions may only use `()` if they have a single generator without `if`, such as:

```scala
for (i <- 0 until n)
  doStuff(i)
```

Otherwise, it must use `{}`, and there must be one generator per line.
Guards (`if`s) may be either on the same line as a generator, or on a dedicated line.
The body of the `for` is then automatically surrounding by braces too, even if it is a one-liner:

```scala
for {
  i <- 0 until n
  j <- 0 until i
} {
  doStuff(i, j)
}
```

The `yield` keyword should be placed at the end of the `for` line, followed by braces:

```scala
for (x <- xs) yield {
  x * 2
}
```

For short `yield` blocks, the following can be used instead:

```scala
for (x <- xs)
  yield x * 2
```

With the multi-line brace for, the `yield` must be placed like this:

```scala
for {
  i <- 0 until n
  j <- 0 until i
} yield {
  thing(i, j)
}
```

### Imports

Imports must follow the following format:

```scala
import scala.language.implicitConversions

import scala.collection.mutable

import java.{util => ju}

import org.scalajs.core.tools.sem._
import org.scalajs.core.tools.javascript._
import org.scalajs.core.tools.optimizer._
```

Language imports must always come first, and must always be at the top of the file (right after the `package` declaration).
There must not be language imports in narrower scopes.

If you import more than 3 or so items from a namespace, use a wildcard import.

Avoid importing mutable collections directly; prefer importing `mutable` and then use `mutable.ListBuffer`.

### ScalaDoc

ScalaDoc comments that fit in one line must be written as

```scala
/** Returns the maximum of a and b. */
def max(a: Int, b: Int): Int = ???
```

Multi-line ScalaDoc comments must use the following style:

```scala
/** Returns the maximum of a and b.
 *
 *  If a > b, returns a. Otherwise returns b.
 */
def max(a: Int, b: Int): Int = ???
```

### Non-ScalaDoc comments

Normal comments fitting on one-line should use `//`.
A comment that does not fit on one line should use the multi-line comment syntax and follow this style:

```scala
/* This complicated algorithm computes the maximum of two integer values a
 * and b. If a > b, it computes a, otherwise it computes b.
 */
```


## Class declaration

A class declaration, together with its constructor parameters, its `extends` clause, and its self type, is technically a single line.
Example:

```scala
class Foo(val x: Int) extends Bar with Foobar { self =>
```

However, this tends to become too long in many cases.

If the declaration does not fit on one line, the self type must be on dedicated line, indented 2 spaces only, and followed by a blank line:

```scala
class Foo(val x: Int) extends Bar with Foobar {
  self =>

  // declarations start here
```

If too long in itself, the list of constructor parameters should be broken similarly to formal parameters to a method, i.e., indented 4 spaces, and followed by a blank line:

```scala
class Foo(val x: Int, val y: Int,
    val z: Int) extends Bar with Foobar {

  // declarations start here
```

As an exception, if the constructor parameters are a (long) list of "configuration" parameters, the following format should be used instead:

```scala
class Foo(
    val width: Int = 1,
    val height: Int = 1,
    val depthOfField: Int = 3
) extends Bar with Foobar {
```

Note that there is no vertical alignment, neither for the type nor the default value (if any).
If there are several parameter lists (e.g., with an implicit parameter list), each parameter list follows its rules independently of the others, i.e., organizing one parameter list vertically does not mean another list should be organized vertically as well.
For example:

```scala
class Foo[A](
    val width: Int = 1,
    val height: Int = 1,
    val depthOfField: Int = 3
)(implicit ct: ClassTag[A]) extends Bar with Foobar {
```

If too long, the `extends` clause itself should go to the next line, indented 4 spaces, and followed by a blank line:

```scala
class Foo(val x: Int)
    extends Bar with Foobar with AnotherTrait {

  // declarations start here
```

The `extends` clause can be broken further before `with`s, if necessary.
Additional lines are also indented 4 spaces wrt. the `class` keyword.

```scala
class Foo(val x: Int)
    extends Bar with Foobar with AnotherTrait with YetAnotherTrait
    with HowManyTraitsAreThere with TooManyTraits {

  // declarations start here
```


## Usages of higher-order methods

### Option and js.UndefOr

Use the higher-order methods in the APIs of `Option` and `js.UndefOr` rather than doing pattern matching.
Note particularly the `fold` method, which should be used instead of the `map`+`getOrElse` combination.

### Collections

Higher-order methods should be favored over loops and tail-recursive methods wherever possible and readable.

Do not reinvent the wheel: use the most appropriate method in the collection API (e.g., use `forall` instead of a custom-made `foldLeft`).

Methods other than `foreach` should however be avoided if the lambda that is passed to them has side-effects.
In order words, a `foldLeft` with a side-effecting function should be avoided, and a `while` loop or a `foreach` used instead.

Use `xs.map(x => x * 2)` instead of `for (x <- xs) yield x * 2` for short, one-liner `map`s, `flatMap`s and `foreach`es.
Otherwise, favor for comprehensions.

### For comprehensions and ranges

Do not fear for over ranges.
The Scala.js optimizer inlines them away.


## if/else

An `if/else` pair in expression position can be written as

```scala
val x =
  if (condition) someExpr
  else anotherExpr
```

assuming both lines fit in the line length limit.
We call this formatting a two-liner `if/else`.

Otherwise, both expressions should be put on separate lines, and indented 2 spaces.
In addition, the rhs of the `=` must then be surrounded in braces:

```scala
val x = {
  if (condition)
    someExpr
  else
    anotherExpr
}
```

If one of the brances requires braces, then put braces on both branches:

```scala
val x = {
  if (condition) {
    val x = someExpr
    x + 5
  } else {
    anotherExpr
  }
}
```

`if`s and `if/else`s in statement position should always have their branch(es) on dedicated lines:

```scala
if (index >= size)
  throw new IndexOutOfBoundsException
```

If the `condition` of an `if` (or `while`, for that matter) is too long, it can be broken *at most once* with 4 spaces of indentation.
In that case, the if and else parts must surrounded by braces, even if they are single-line.
Obviously, the two-liner `if/else` formatting cannot be applied.

If the condition is so long that two lines are not enough, then it should be extracted in a local `def` before it, such as:

```scala
val ident: String = ???

def isValidIdent = {
  ident != "" &&
  ident.charAt(0).isUnicodeIdentifierStart &&
  ident.tail.forall(_.isUnicodeIdentifierPart)
}

if (isValidIdent)
  doSomething()
else
  doSomethingElse()
```


## Pattern matching

One-liner cases should be written on one line, and the arrows aligned:

```scala
x match {
  case Foo(a, b) => a + b
  case Bar(y)    => 2 * y
}
```

If the body of a case does not fit on the same line, then put the body on the next line, indented 2 spaces, without braces around it.
In that case, also put blank lines around that `case`, and do not align its arrow with the other groups:

```scala
x match {
  case Foo(a, b) => a + b

  case Bar(y) =>
    if (y < 5) y
    else y * 2
}
```

When pattern-matching based on specific subtypes of a value, reuse the same identifier for the refined binding, e.g.,

```scala
that match {
  case that: Foo => that.bar == 5
  case _         => false
}
```

When using type-based pattern matching combined with alternatives (using `|`), remove the space after the `:` operator:

```scala
that match {
  case _:Foo | _:Bar | _:Foobar => true
  case _                        => false
}
```

This is an instantiation of the rule saying that spaces can be removed around a binary operator to highlight its higher priority wrt. its neighbors.

As a reminder, avoid pattern-matching on `Option` types.
Use `fold` instead.


## Implementing the Java lib

Special rules apply to implementing classes of the JDK (typically in `javalanglib/` or `javalib/`).

### Order of declarations

Fields and methods should be declared in the order in which they appear in the JavaDoc.
Note that in the JavaDoc, you first see a *summary* of the members, which is always ordered alphabetically.
This is *not* the order you should follow.
Instead, you should follow the order in which the *full descriptions* of the members appear.

### Parameterless methods

Methods without parameters should *always* be declared with `()`, regardless of whether they have side-effects or not.

### No public members in addition to those in the JavaDoc

There should not be any public members besides those documented in the JavaDoc.
Usually, there should also be no protected members not in the JavaDoc.
