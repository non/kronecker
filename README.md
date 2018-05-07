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

The laws for `ev: Countable[A]` are as follows:

 * for every `a: A` there is an `i` such that `ev.get(i) = Some(a)`
 * `ev.get(i)` returns `Some(_)` for all `i < ev.cardinality`
 * `ev.get(i)` returns `None` for all `i >= ev.cardinality`
 * `ev.get(i) = Some(_) = ev.get(j)` if and only if `i = j`
 
The laws for `ev: Indexable[A]` are the above laws and also:

 * if `ev.get(i)` returns `Some(a)`, then `ev.index(a) = i`
 * for all `a`, `0 <= index(a) < ev.cardinality`
 * for all `a`, `get(index(a)) = Some(a)`
 * `index(a1) = index(a2)` if and only if `a1 = a2`
 
In all these laws `i` is assumed to be a non-negative, unbounded
integer (i.e. a `spire.math.SafeLong`, aliased as `Z` in Kronecker).

### Details

Some people might take issue with the finite/infinite terminology
(especially in a library named for Kronecker). The terms stand in for
bounded/unbounded. `Card.Finite` represents a finite, definite
quantity that we can compute with, whereas `Card.Infninite` represents
an unbounded cardinality (a set whose members can't be exhaustively
enumerated). The other `Card.Semifinite` members (`Plus`, `Times`,
`Pow`) represent quantities that are bounded, but which can't be
computed with in the current runtime (they are effectively unbounded).

### Known issues

There are numbers that are too big to compute arithmetically and which
will cause your JVM to appear to hang, or to crash. Kronecker's API
may encourage you to dance dangerously close to the chasm of
non-termination: consider yourself warned.

For example, here's how the size of an integer in a set affects the
index returned by `Indexable[Set[Int]]`:

```scala
val ev = kronecker.Indexable[Set[Int]]
ev.index(Set(1))            //          1 digit
ev.index(Set(10))           //          4 digits
ev.index(Set(100))          //         31 digits
ev.index(Set(1000))         //        302 digits
ev.index(Set(10000))        //      3,011 digits
ev.index(Set(100000))       //     30,103 digits (feels instant)
ev.index(Set(1000000))      //    301,030 digits (some delay)
ev.index(Set(10000000))     //  3,010,300 digits (takes ~3 seconds)
ev.index(Set(100000000))    // 30,103,000 digits (takes ~3 minutes)
ev.index(Set(Int.MaxValue)) // crashes instantly with ArithmeticException
```

The underlying issue here is that if the cardinality of `A` is *x*,
then the cardinality of a `Set[A]` is *2ˣ*.

### Future work

We're missing `Indexable` instances for `Map`, for `HList`, and for
`Coproduct`.

There is some work-in-progress around actually using `Countable[A]` to
power property-based tests (e.g. ScalaCheck). This might end up being
kind of cool if it works well.

In terms of space-age work, using some model of *codata* to bound how
much work we're willing to do (and to catch situations where the JVM
might appear to hang before it does so) would be pretty radical.

### Copyright and License

All code is available to you under the Apache 2 license, available at
https://opensource.org/licenses/Apache-2.0.

Copyright Erik Osheim, 2018.
