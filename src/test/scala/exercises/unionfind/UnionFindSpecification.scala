package exercises.unionfind

import org.scalacheck.{Arbitrary, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAll}

/**
 * Created by abdullah on 1/25/15.
 */
object UnionFindSpecification extends Properties("exercises.unionfind.UnionFind") {

  val smallNumbers = Arbitrary.arbitrary[Int].suchThat(x => x >= 0 && x <= 100000)

  implicit def arbQuickFind: Arbitrary[QuickFind] = Arbitrary {
    smallNumbers.map(Helpers.randQuickFind(_))
  }

  property("components") = forAll((uf: QuickFind) => uf.components <= uf.N)

  property("union connected") = forAll { (uf: QuickFind, p: Int, q: Int) =>
    (p >= 0 && p < uf.N && q >= 0 && q < uf.N) ==> {
      uf.union(p, q)
      uf.connected(p, q) && uf.connected(q, p)
    }
  }
}