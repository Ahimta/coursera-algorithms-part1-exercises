package exercises.unionfind

import scala.annotation.tailrec

/**
 * Created by abdullah on 1/26/15.
 */
final class WeightedQuickUnion(N: Int) extends UnionFind(N) {

  val szs = Array.fill(N)(1)

  override def union(p: Int, q: Int): UnionFind = {

    val pid = find(p)
    val qid = find(q)

    if (pid != qid) {
      _components -= 1

      if (szs(pid) > szs(qid)) {
        ids(qid) = pid
        szs(pid) += szs(qid)
      } else {
        ids(pid) = qid
        szs(qid) += szs(pid)
      }
    }

    this
  }

  @tailrec
  override def find(p: Int): Int = {
    require(isValidId(p))

    val pid = ids(p)

    if (p == pid) { p }
    else { find(pid) }
  }
}
