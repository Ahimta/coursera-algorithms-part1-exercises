package exercises.unionfind

import scala.annotation.tailrec

/**
 * Created by abdullah on 1/27/15.
 */
final class WeightedQuickUnionWPC(N: Int) extends UnionFind(N) {

  val szs = Array.fill(N)(0.toByte)

  override def union(p: Int, q: Int): UnionFind = {

    val pid = find(p)
    val qid = find(q)

    if (pid != qid) {
      if      (szs(pid) > szs(qid)) { ids(qid) = pid }
      else if (szs(pid) < szs(qid)) { ids(pid) = qid }
      else {
        ids(qid) = pid
        szs(pid) = (szs(pid) + 1).toByte
      }
    }

    this
  }

  @tailrec
  override def find(p: Int): Int = {
    require(isValidId(p))

    val pid = ids(p)
    if (p == pid) { p }
    else {
      ids(p) = ids(pid)
      find(pid)
    }
  }
}
