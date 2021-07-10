# Design document for the implementation of `j.u.regex.*`

Java and JavaScript have different support for regular expressions.
In addition to Java having many more features, they also *differ* in the specifics of most of the features they have in common.

For performance and code size reasons, we still want to use the native JavaScript `RegExp` class.
Modern JavaScript engines JIT-compile `RegExp`s to native code, so it is impossible to compete with that using a user-space engine.
For example, see [V8 talking about its Irregexp library](https://blog.chromium.org/2009/02/irregexp-google-chromes-new-regexp.html) and [SpiderMonkey talking about their latest integration of Irregexp](https://hacks.mozilla.org/2020/06/a-new-regexp-engine-in-spidermonkey/).

Therefore, our strategy for `java.util.regex` is to *compile* Java regexes down to JavaScript regexes that behave in the same way.
The compiler is in the file `PatternCompiler.scala`, and is invoked at the time of `Pattern.compile()`.

We can deal with most features in a compliant way using that strategy, while retaining performance, and without sacrificing code size too much compared to directly passing regexes through without caring about the discrepancies.
There are however a few features that are either never supported, or only supported when targeting a recent enough version of ECMAScript.

## Support

The set of supported features depends on the target ECMAScript version, specified in `ESFeatures.esVersion`.

The following features are never supported:

* the `CANON_EQ` flag,
* the `\X`, `\b{g}` and `\N{...}` expressions,
* `\p{In𝘯𝘢𝘮𝘦}` character classes representing Unicode *blocks*,
* the `\G` boundary matcher, *except* if it appears at the very beginning of the regex (e.g., `\Gfoo`),
* embedded flag expressions with inner groups, i.e., constructs of the form `(?idmsuxU-idmsuxU:𝑋)`,
* embedded flag expressions without inner groups, i.e., constructs of the form `(?idmsuxU-idmsuxU)`, *except* if they appear at the very beginning of the regex (e.g., `(?i)abc` is accepted, but `ab(?i)c` is not), and
* numeric "back" references to groups that are defined later in the pattern (note that even Java does not support *named* back references like that).

The following features require `esVersion >= ESVersion.ES2015`:

* the `UNICODE_CASE` flag.

The following features require `esVersion >= ESVersion.ES2018`:

* the `MULTILINE` and `UNICODE_CHARACTER_CLASS` flags,
* look-behind assertions `(?<=𝑋)` and `(?<!𝑋)`,
* the `\b` and `\B` expressions used together with the `UNICODE_CASE` flag,
* `\p{𝘯𝘢𝘮𝘦}` expressions where `𝘯𝘢𝘮𝘦` is not one of the [POSIX character classes](https://docs.oracle.com/en/java/javase/15/docs/api/java.base/java/util/regex/Pattern.html#posix).

It is worth noting that, among others, the following features *are* supported in all cases, even when no equivalent feature exists in ECMAScript at all, or in the target version of ECMAScript:

* correct handling of surrogate pairs (natively supported in ES 2015+),
* the `\G` boundary matcher when it is at the beginning of the pattern (corresponding to the 'y' flag, natively supported in ES 2015+),
* named groups and named back references (natively supported in ES 2018+),
* the `DOTALL` flag (natively supported in ES 2018+),
* ASCII case-insensitive matching (`CASE_INSENSITIVE` on but `UNICODE_CASE` off),
* comments with the `COMMENTS` flag,
* POSIX character classes in ASCII mode, or their Unicode variant with `UNICODE_CHARACTER_CLASS` (if the latter is itself supported, see above),
* complex character classes with unions and intersections (e.g., `[a-z&&[^g-p]]`),
* atomic groups `(?>𝑋)`,
* possessive quantifiers `𝑋*+`, `𝑋++` and `𝑋?+`,
* the `\A`, `\Z` and `\z` boundary matchers,
* the `\R` expression,
* embedded quotations with `\Q` and `\E`, both outside and inside character classes.

All the supported features have the correct semantics from Java.
This is even true for features that exist in JavaScript but with different semantics, among which:

* the `^` and `$` boundary matchers with the `MULTILINE` flag (when the latter is supported),
* the predefined character classes `\h`, `\s`, `\v`, `\w` and their negated variants, respecting the `UNICODE_CHARACTER_CLASS` flag,
* the `\b` and `\B` boundary matchers, respecting the `UNICODE_CHARACTER_CLASS` flag,
* the internal format of `\p{𝘯𝘢𝘮𝘦}` character classes, including the `\p{java𝘔𝘦𝘵𝘩𝘰𝘥𝘕𝘢𝘮𝘦}` classes,
* octal escapes and control escapes.

### Guarantees

If a feature is not supported, a `PatternSyntaxException` is thrown at the time of `Pattern.compile()`.

If `Pattern.compile()` succeeds, the regex is guaranteed to behave exactly like on the JVM, *except* for capturing groups within repeated segments (both for their back references and subsequent calls to `group`, `start` and `end`):

* on the JVM, a capturing group always captures whatever substring was successfully matched last by *that* group during the processing of the regex:
  - even if it was in a previous iteration of a repeated segment and the last iteration did not have a match for that group, or
  - if it was during a later iteration of a repeated segment that was subsequently backtracked;
* in JS, capturing groups within repeated segments always capture what was matched (or not) during the last iteration that was eventually kept.

The behavior of JavaScript is more "functional", whereas that of the JVM is more "imperative".
This imperative nature is also reflected in the `hitEnd()` and `requireEnd()` methods of `Matcher`, which we do not support (they don't link).

The behavior of the JVM does not appear to be specified, and is questionable.
There are several open issues that argue it is buggy:

* https://bugs.openjdk.java.net/browse/JDK-8027747
* https://bugs.openjdk.java.net/browse/JDK-8187083
* https://bugs.openjdk.java.net/browse/JDK-8187080
* https://bugs.openjdk.java.net/browse/JDK-8187082

Therefore, it seems wise to keep the JavaScript behavior, and not try to replicate the JVM behavior at great cost (if that is even possible within our constrains).

## Implementation strategy

Java regexes are compiled to JS regexes in one pass, using a recursive descent approach.
There is a state variable `pIndex` which indicates the position inside the original `pattern`.
Compilation methods parse a subexpression at `pIndex`, advance `pIndex` past what they parsed, and return the result of the compilation.

Parsing is always done at the code point level, and not at the individual `Char` level, using the [WTF-16 encoding](https://simonsapin.github.io/wtf-8/#wtf-16).
See [Handling surrogate pairs without support for the 'u' flag](#handling-surrogate-pairs-without-support-for-the-u-flag) for details about the behavior of lone surrogates.

We first describe the compilation with the assumption that the underlying `RegExp`s support the `u` flag.
This is always true in ES 2015+, and dynamically determined at run-time in ES 5.1.
We will cover later what happens when it is not supported.

### JS RegExp flags and case sensitivity

Irrespective of the Java flags, we always use the following JS flags when they are supported (including through dynamic detection):

- 'u' for correct handling of surrogate pairs and Unicode rules for case folding (introduced in ES2015),
- 's' for the dotAll behavior, i.e., `.` matches any code point (introduced in ES2018).

In addition, we use the 'i' JS flag when both `CASE_INSENSITIVE` and `UNICODE_CASE` are on.
Since `UNICODE_CASE` is only supported in ES 2015+, this implies that 'u' is supported, and hence that we can leave all the handling of case insensitivity to the native RegExp.

When `CASE_INSENSITIVE` is on but `UNICODE_CASE` is off, we must apply ASCII case insensitivity.
This is not supported by JS RegExps, so we implement it ourselves during compilation.
This is represented by the property `asciiCaseInsensitive`.
When it is true:

* any single code point that is an ASCII letter, such as 'g', is compiled to a character class with the uppercase and lowercase variants (e.g., `[Gg]`), in subexpressions or in character classes, and
* any character range in a character class that intersects with the range `A-Z` and/or `a-z` is compiled with additional range(s) to cover the uppercase and lowercase variants.

`PatternCompiler` never uses any other JS RegExp flag.
`Pattern` adds the 'g' flag for its general-purpose instance of `RegExp` (the one used for everything except `Matcher.matches()`), as well as the 'y' flag if the regex is sticky and it is supported.

### Capturing groups

Usually, there is a 1-to-1 relationship between original group numbers and compiled groups numbers.
However, differences are introduced when compiling atomic groups and possessive quantifiers.
Therefore, we maintain a mapping from original group numbers to the corresponding group numbers in the compiled pattern.
We use it for the following purposes:

* when compiling back references of the form `\𝑁`, and
* when using the `Matcher` API to retrieve the groups' contents, start and end positions.

Named capturing groups are always compiled as numbered capturing groups, even in ES 2018+.
We record an additional map of names to the corresponding original group numbers, and use it

* when compiling named back references of the form `\k<𝘯𝘢𝘮𝘦>` (as numbered back references), and
* when using the `Matcher` API with group names.

### Other main "control structures"

The following constructs are translated as is:

* Sequences and alternatives,
* Greedy quantifiers of the form `𝑋*`, `𝑋+` and `𝑋?`,
* Lazy quantifiers of the form `𝑋*?`, `𝑋+?` and `𝑋??`,
* Non-capturing groups of the form `(?:𝑋)`,
* Look-ahead groups of the form `(?=𝑋)` and `(?!𝑋)`,
* Look-behind groups of the form `(?<=𝑋)` and `(?<!𝑋)`, after validating that they are supported.

The following constructs have special handling and will be discussed later:

* Atomic groups of the form `(?>𝑋)`, and
* Possessive quantifiers, for example of the form `𝑋*+`.

### Single code points

Subexpressions that represent a single code point are parsed and normalized as the code point that they represent.
For example, both `a` and `\x65` are compiled as `a`.
Code points that are metacharacters in JS regexes (i.e., `^ $ \ . * + ? ( ) [ ] { } |`) are escaped with a `\`, for example `\$`.
This is implemented in `def literal(cp: Int)`.

Note that a double escape of the form `\uℎℎℎℎ\uℎℎℎℎ` representing a high surrogate and a low surrogate is treated as a single escape for a single supplementary code point.
For example, `\uD834\uDD1E` is considered as a single escape for the code point `𝄞` (U+1D11E Musical Symbol G Clef).

This behavior only applies to `\u` escapes.
A would-be double-escape `\x{D834}\x{DD1E}` constitutes two separate code points.
In practice, such a sequence can never match anything in the input; if the input contained that sequence of code units, it would be considered as a single code point `𝄞`, which is not matched by a pattern meant to match two separate code points U+D834 and U+DD1E.

### Quotes

A quote starts with `\Q`, and ends at the first occurrence of `\E` or the end of the string.
The full string in between is taken as a sequence of code points.

Each code point is compiled as described in "Single code points" for `def literal(cp: Int)`, and the compiled patterns are concatenated in a sequence.
This is implemented in `def literal(s: String)`.

### Predefined character classes

Predefined character classes represent a set of code points that matches a single code point in the input string.
The set typically depends on the value of `UNICODE_CHARACTER_CLASS`.

Since virtually none of them has a corresponding predefined character class in JS RegExps, they are all compiled as custom `[...]` character classes, according to their definition.

### Atomic groups

Atomic groups are not well documented in the JavaDoc, but they are well covered in outside documentation such as [on Regular-Expressions.info](https://www.regular-expressions.info/atomic.html).
They have the form `(?>𝑋)`.
An atomic group matches whatever `𝑋` matches, but once it has successfully matched a particular substring, it is considered as an atomic unit.
If backtracking is needed later on because the rest of the pattern failed to match, the atomic group is backtracked as a whole.

JS does not support atomic groups.
However, there exists a trick to implement atomic groups on top of look-ahead groups and back references, including with the correct performance characterics.
It is well documented in the article [Mimicking Atomic Groups](https://blog.stevenlevithan.com/archives/mimic-atomic-groups).
In a nutshell, we compile `(?>𝑋)` to `(?:(?=(𝑋))\𝑁)` where `𝑁` is the allocated group number for the capturing group `(𝑋)`.

This introduces a discrepancy between the original group numbers and the compiled group numbers for any subsequent capturing group.
This is why we maintain `groupNumberMap`.
Note that the discrepancy applies within `𝑋` as well, so we record it before compiling the subexpression `𝑋`.

### Possessive quantifiers

[Possessive quantifiers](https://www.regular-expressions.info/possessive.html) can be interpreted as sugar for atomic groups over greedy quantifiers.
For example, `𝑋*+` is equivalent to `(?>𝑋*)`.

Since JS does not support possessive quantifiers any more than atomic groups, we compile them as that desugaring, followed by the compilation scheme of atomic groups.

However, there is an additional problem.
For atomic groups, we know before parsing `𝑋` that we need to record a discrepancy in the group numbering.
For possessive quantifiers, we only know that *after* having parsed `𝑋`, but it should apply also *within* `𝑋`.
We do that with postprocessing.
Before compiling any token `𝑋`, we record the current `compiledGroupCount`, and when compiling a possessive quantifier, we increment the compiled group number of those greater than the recorded count.
We do this

- in the values of `groupNumberMap`, and
- in the back references found in the compiled pattern for `𝑋`.

The latter is pretty ugly, but is robust nevertheless.

### Custom character classes

Unlike JavaScript, Java regexes support intersections and unions of character classes.
We compile them away using the following equivalences:

* Positive intersection: `[𝐴&&𝐵]` is equivalent to `(?=[𝐴])[𝐵]`
* Negative intersection: `[^𝐴&&𝐵]` is equivalent to `[^𝐴]|[^𝐵]`
* Positive union: `[𝐴𝐵]` is equivalent to `[𝐴]|[𝐵]`
* Negative union: `[^𝐴𝐵]` is equivalent to `(?![𝐴])[^𝐵]`

For example, using the rule on positive intersection, we can compile the example from the JavaDoc `[a-z&&[^m-p]]` to `(?=[a-z])[^m-p]`.

An alternative design would have been to resolve all the operations at compile-time to get to flat code point sets.
This would require to expand `\p{}` and `\P{}` Unicode property names into equivalent sets, which would need a significant chunk of the Unicode database to be available.
That strategy would have a huge cost in terms of code size, and likely in terms of execution time as well (for compilation and/or matching).

### Handling surrogate pairs without support for the 'u' flag

So far, we have assumed that the underlying RegExp supports the 'u' flag, which we test with `supportsUnicode`.
In this section, we cover how the compilation is affected when it is not supported.
This can only happen when we target ES 5.1.

The ECMAScript specification is very precise about how patterns and strings are interpreted when the 'u' flag is enabled.
It boils down to:

* First, the pattern and the input, which are strings of 16-bit UTF-16 code units, are decoded into a *list of code points*, using the WTF-16 encoding.
  This means that surrogate pairs become single supplementary code points, while lone surrogates (and other code units) become themselves.
* Then, all the regular expressions operators work on these lists of code points, never taking individual code units into account.

The documentation for Java regexes does not really say anything about what it considers "characters" to be.
However, experimentation and tests show that they behave exactly like ECMAScript with the 'u' flag.

Without support for the 'u' flag, the JavaScript RegExp engine will parse the pattern and process the input with individual Chars rather than code points.
In other words, it will consider surrogate pairs as two separate (and therefore separable) code units.
If we do nothing against it, it can jeopardize the semantics of regexes in several ways:

* a `.` will match only the high surrogate of a surrogate pair instead of the whole codepoint,
* same issue with any negative character class like `[^a]`,
* an unpaired high surrogate in the pattern may match the high surrogate of a surrogate pair in the input, although it must not,
* a surrogate pair in a character class will be interpreted as *either* the high surrogate or the low surrogate, instead of both together,
* etc.

Even patterns that contain only ASCII characters (escaped or not) and use no flags can behave incorrectly on inputs that contain surrogate characters (paired or unpaired).
A possible design would have been to restrict the *inputs* to strings without surrogate code units when targeting ES 5.1.
However, that could lead to patterns that fail at matching-time, rather than at compile-time (during `Pattern.compile()`), unlike all the other features that are conditioned on the ES version.

Therefore, we go to great lengths to implement the right semantics despite the lack of support for 'u'.

#### Overall idea of the solution

When `supportsUnicode` is false, we apply the following changes to the compilation scheme.
In general, we make sure that:

* something that can match a high surrogate does not match one followed by a low surrogate,
* something that can match a supplementary code point or a high surrogate never selects the high surrogate if it could match the whole code point.

We do nothing special for low surrogates, as all possible patterns go from left to right (we don't have look-behinds in this context) and we otherwise make sure that all code points from the input are either fully matched or not at all.
Therefore, the "cursor" of the engine can never stop in the middle of a code point, and so low surrogates are only visible if they were unpaired to begin with.
The only exception to this is when the cursor is at the beginning of the pattern, when using `find`.
In that case we cannot a priori prevent the JS engine from trying to find a match starting in the middle of a code point.
To address that, we have special a posteriori handling in `Pattern.execFind()`.

#### Concretely

A single code point that is a high surrogate `𝑥` is compiled to `(?:𝑥(?![ℒ]))`, where `ℒ` is `\uDC00-\uDFFF`, the range of all low surrogates.
The negative look-ahead group prevents a match from separating the high surrogate from a following low surrogate.

A dot-all (in `codePointNotAmong("")`) is compiled to `(?:[^ℋ]|[ℋ](?:[ℒ]|(?![ℒ])))`, where `ℋ` is `\uD800-\uDBFF`, the range of all high surrogates.
This means either

* any code unit that is not a high surrogate, or
* a high surrogate and a following low surrogate (taking a full code point is allowed), or
* a high surrogate that is not followed by a low surrogate (separating a surrogate pair is not allowed).

We restrict the internal contract of `codePointNotAmong(𝑠)` to only take BMP code points that are not high surrogates, and compile it to the same as the dot-all but with the characters in `𝑠` excluded like the high surrogates: `(?:[^𝑠ℋ]|[ℋ](?:[ℒ]|(?![ℒ])))`.

Since `UNICODE_CHARACTER_CLASS` is not supported, all but one call site of `codePointNotAmong` already respect that stricter contract.
The only one that does not is the call `codePointNotAmong(thisSegment)` inside `CharacterClassBuilder.conjunctResult()`.
To make that one compliant, we make sure not to add illegal code points in `thisSegment`.
To do that, we exploit the equivalences `[𝐴𝐵] = [𝐴]|[𝐵]` and `[^𝐴𝐵] = (?![𝐴])[𝐵]` where `𝐴` is an illegal code point to isolate it in a separate alternative, that we can compile as a single code point above.
For example, the character class `[k\uD834f]`, containing a high surrogate code point, is equivalent to `[\uD834]|[kf]`, which can be compiled as `(?:\uD834(?![ℒ]))|[kf])`.
That logic is implemented in `CharacterClassBuilder.addSingleCodePoint()`.

Code point ranges that contain illegal code points are decomposed into the union of 4 (possibly empty) ranges:

* one with only BMP code points below high surrogates, compiled as is
* one with high surrogates `𝑥-𝑦`, compiled to `(?:[𝑥-𝑦](?![ℒ]))`
* one with BMP code points above high surrogates, compiled as is
* one with supplementary code points `𝑥-𝑦`, where `𝑥` is the surrogate pair `𝑝𝑞` and `𝑦` is the pair `𝑠𝑡`, which is further decomposed into:
  * the range `𝑝𝑞-𝑝\uDFFF`, compiled as `(?:𝑝[𝑞-\uDFFF])`
  * the range `𝑝′\uDC00-𝑠′\uDFFF` where 𝑝′ = 𝑝+1 and 𝑠′ = 𝑠−1, compiled to `(?:[𝑝′-𝑠′][\uDC00-\uDFFF])`
  * the range `𝑠\uDC00-𝑠𝑡`, compiled to `(?:𝑠[\uDC00-𝑡])`

That logic is implemented in `CharacterClassBuilder.addCodePointRange()`.

## About code size

For historical reasons, code size is critical in this class.
Before Scala.js 1.7.0, `java.util.regex.Pattern` was just a wrapper over native `RegExp`s.
The patterns were passed through with minimal preprocessing, without caring about the proper semantics.
This created an expectation of small code size for this class.
When we fixed the semantics, we had to introduce this compiler, which is non-trivial.
In order not to regress too much on code size, we went to great lengths to minimize the code size impact of this class, in particular in the default ES 2015 configuration.

When modifying this code, make sure to preserve as small a code size as possible.
