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

Here's an example REPL session that demonstrates some early functionality:

```scala
scala> import kronecker._
import kronecker._

scala> val c = Countable[List[Byte]]
c: kronecker.Countable[List[Byte]] = kronecker.Countable$$anon$2@5ad1bd6d

scala> c.get(0)
res0: Option[List[Byte]] = Some(List())

scala> c.get(1)
res1: Option[List[Byte]] = Some(List(0))

scala> c.get(1000)
res2: Option[List[Byte]] = Some(List(2, -25))

scala> c.get(BigInt(20).pow(20))
res4: Option[List[Byte]] = Some(List(85, -69, 116, -31, -43, 47, -2, -2, -2, -2, -1))

scala> val cc = Countable[(Z, List[Boolean])]
cc: kronecker.Countable[(kronecker.Z, List[Boolean])] = kronecker.Countable$$anon$3@7cfdd278

scala> cc.get(BigInt(5).pow(5))
res1: Option[(kronecker.Z, List[Boolean])] = Some((-17,List(false, true, true, false, true)))
```

### Copyright and License

All code is available to you under the Apache 2 license, available at
https://opensource.org/licenses/Apache-2.0.

Copyright Erik Osheim, 2018.
