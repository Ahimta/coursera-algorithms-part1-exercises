package exercises.unionfind

import scala.annotation.tailrec

/**
 * Created by abdullah on 1/26/15.
 */
class WeightedQuickUnion(N: Int) extends UnionFind(N) {

  val szs = Array.fill(N)(0)

  override def union(p: Int, q: Int): UnionFind = {

    val pid = find(p)
    val qid = find(q)

    if (pid == qid) { this }
    else {
      _components -= 1

      if (szs(pid) < szs(qid)) {
        ids(qid) = pid
        szs(pid) += 1
      } else {
        ids(pid) = qid
        szs(qid) += 1
      }
      this
    }
  }

  override def find(p: Int): Int = {
    require(isValidId(p))

    val root = ids(p)

    if (p == root) { p }
    else { find(root) }
  }
}
