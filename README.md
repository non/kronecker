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

### Copyright and License

All code is available to you under the Apache 2 license, available at
https://opensource.org/licenses/Apache-2.0.

Copyright Erik Osheim, 2018.
