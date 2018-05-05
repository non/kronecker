package kronecker

import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary

object Testing {
  implicit val arbitraryZ: Arbitrary[Z] =
    Arbitrary(arbitrary[BigInt].map(n => Z(n.abs)))
    //Arbitrary(arbitrary[Byte].map(n => Z(n).abs))
}
