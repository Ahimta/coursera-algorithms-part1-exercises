package exercises.unionfind

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAll}

/**
 * Created by abdullah on 1/25/15.
 */
object UnionFindSpecification extends Properties("exercises.unionfind.UnionFind") {

  val smallNumbers = Gen.choose(1, 1000)

  implicit def arbUnionFind: Arbitrary[UnionFind] = Arbitrary {
    smallNumbers.map(Helpers.randQuickFind)
  }

  property("components") = forAll((uf: UnionFind) => uf.components <= uf.N && uf.components > 0)

  property("connected - reflexivity") = forAll { (uf: UnionFind, p: Int) =>
    (p >= 0 && p < uf.N) ==> {
      uf.connected(p, p)
    }
  }

  property("find - identity") = forAll { (uf: UnionFind, p: Int) =>
    (p >= 0 && p < uf.N) ==> {
      uf.find(p) == uf.find(p)
    }
  }

  property("union connected") = forAll { (uf: UnionFind, p: Int, q: Int) =>
    (p >= 0 && p < uf.N && q >= 0 && q < uf.N) ==> {
      uf.union(p, q)
      uf.connected(p, q) && uf.connected(q, p)
    }
  }

  property("union connected") = forAll { (uf: UnionFind, p: Int, q: Int, r: Int) =>
    (p >= 0 && p < uf.N && q >= 0 && q < uf.N && r >= 0 && r < uf.N) ==> {
      uf.union(p, q).union(q, r)
      uf.connected(p, r) && uf.connected(r, p)
    }
  }
}