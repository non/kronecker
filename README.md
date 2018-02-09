## Kronecker

### Preamble

```
"Die ganzen Zahlen hat der liebe Gott gemacht, alles andere ist Menschenwerk."

  -- Leopold Kronecker

(God made the integers, everything else is human work.)
```

### Overview

Kronecker is intended to be a library for enumerating all distinct
values of types.

### Example

The `Countable[A]` type class provides a `get` method, which takes an
index (given as a `spire.math.SafeLong`) and returns an `Option[A]`.
The type class instance must be able to generate any possible `A`
value (provided a large enough input number is provided).

Here's an example REPL session that demonstrates some basic
functionality:

```scala
import kronecker._

val c0 = Countable[List[Byte]]

c0.get(0)          // Some(List())
c0.get(1)          // Some(List(0))
c0.get(2)          // Some(List(1))
c0.get(1000)       // Some(List(2, -25))
c0.get(1000000)    // Some(List(14, 65, 63))
c0.get(1000000000) // Some(List(58, -103, -56, -1))

val i = Z(20).pow(64)
// 184467440737095516160000000000000000000000000000000000000000000000000000000000000000

c0.get(i)
// Some(List(23, 78, 2, -24, 62, -8, -13, -39, -90, -106, -20, 109, 55,
//          -20, 99, -66, 105, 29, -1, -2, -2, -2, -2, -2, -2, -2, -2,
//          -2, -2, -2, -2, -2, -2, -2, -1))


val c1 = Countable[(Z, Z, Z, Z, Z, Z, Z, Z, Z)]

c1.get(0)          // Some((0,0,0,0,0,0,0,0,0))
c1.get(1)          // Some((1,0,0,0,0,0,0,0,0))
c1.get(2)          // Some((0,1,0,0,0,0,0,0,0))
c1.get(1000)       // Some((1,0,-2,0,0,0,0,0,0))
c1.get(1000000)    // Some((-1,0,2,1,2,-1,0,0,-2))
c1.get(1000000000) // Some((2,1,0,4,-5,-1,-2,3,3))

val j = Z(20).pow(32)
// 429496729600000000000000000000000000000000

c1.get(j) // Some((25689,-10621,1664,-1809,-14874,22207,-1114,9471,172))
```

After the JVM is warm, the above examples run in near-instant time on
the author's machine. However it's not hard to increment the exponents
used until the library takes "too long" to return (for example,
`c1.get(i)` takes longer than you'll care to wait).

### Laws

The laws for `ev: Finite[A]` are as follows:

 * `ev.size >= 0`
 * `ev.get(i)` returns `Some(_)` for all `i` in `[0, ev.size)`
 * `ev.get(i)` returns `None` for all `i >= ev.size`
 * `ev.get(i) = ev.get(j)` if and only if `i == j`
 * for every `a: A` there is an `i` such that `ev.get(i) = Some(a)`
 * `ev.cardinality = Card(ev.size)`

The laws for `ev: Infinite[A]` are as follows:

 * `ev(i) = ev(j)` if and only if `i == j`
 * for every `a: A` there is an `i` such that `ev(i) = a`
 * `ev.get(i) = Some(ev(i))`
 * `ev.cardinality = Infinite`

Every `Countable[A]` is either `Finite[A]` or `Infinite[A]`, so the
"common" laws that apply no matter what are:

 * `ev.get(i)` returns `Some(_)` for all `i < ev.cardinality`
 * `ev.get(i)` returns `None` for all `i >= ev.cardinality`
 * `ev.get(i) = ev.get(j)` if and only if `i == j`
 * for every `a: A` there is an `i` such that `ev.get(i) = Some(a)`
 
In all these laws `i` is assumed to be a non-negative, unbounded
integer (i.e. a `spire.math.SafeLong`, aliased as `Z` in Kronecker).

### Details

Some people might take issue with the finite/infinite terminology
(especially in a library named for Kronecker). The terms stand in for
bounded/unbounded. `Finite[A]` instances have a hard upper bound on
how many unique values exist, regardless of how much work the caller
chooses to do generating them. By contrast, `Infinite[A]` instances
can (in principle) go as big as the caller desires (given adequte time
and space).

In practice, there are certain internal optimizations which lower the
theoretical bounds somewhat, but the authors are confident that most
users will be unable to observe these in practice.

### Known issues

There are numbers that are too big to compute arithmetically and which
will cause your JVM to appear to hang. Kronecker's API may encourage
you to dance dangerously close to the chasm of non-termination:
consider yourself warned.

Currently `Finite[Set[A]]` can fail if the corresponding `Finite[A]`
has a cardinality that is too large. The work-around is to create a
bogus infinite instance as follows:

```scala
def bogus[A](implicit ev: Finite[A]): Infinite[Set[A]] = {
  val ia: Infinite[A] = Countable.sz.translate(ev.get(_).get)
  Countable.iset(ia)
}
```

This issue will be shared by any other instances that are technically
finite but so large that we can't arithmetically represent their
cardinality.

### Future work

It might nice to add a *co-countable* type class (i.e. `Indexable`) to
represent being able to go from a value to its index (`A => Z`). This
would be analogous to the relationship between `Gen` and `Cogen`;
using `Indexable` we could provide `Countable[A => B]` instances.

We could generalize our support for `Countable[Either[A, B]]` to
coproducts (e.g. `Countable[A :+: B :+: ...]`) in the same way we
currently support `HList`. With this, we could add support for
deriving sealed ADTs.

It's possible that exposing the finite/infinite split to the user is
not the best design. It would be possible to paper over this with
pattern-matching (although we'd lose some assurances we currently get
about doing things properly).

There is some work-in-progress around actually using Countable[A] to
power property-based tests (e.g. ScalaCheck). This might end up being
kind of cool if it works.

In terms of space-age work, using some model of *codata* to bound how
much work we're willing to do (and to catch situations where the JVM
might appear to hang before it does so) would be pretty radical.

### Copyright and License

All code is available to you under the Apache 2 license, available at
https://opensource.org/licenses/Apache-2.0.

Copyright Erik Osheim, 2018.
