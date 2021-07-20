package kronecker

import org.scalacheck.Properties
import org.typelevel.claimant.Claim

class DemoTest extends Properties("Demo") {

  property("demo") = {

    import eu.timepit.refined._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.{boolean => b}
    import eu.timepit.refined.{numeric => n}
    import spire.implicits._
    import kronecker.refined.implicits._
    val c = Countable[Refined[Int, b.And[n.Greater[W.`5`.T], n.Less[W.`20`.T]]]]

    val size = 20 - 5 - 1
    val p0 = Claim(c.cardinality == Card(size))
    val ps = (0 until size).map(i => Claim(c.get(i).get.value == 6 + i))
    ps.foldLeft(p0)(_ && _)
  }
}
