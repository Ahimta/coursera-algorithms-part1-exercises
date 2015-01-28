package exercises.unionfind

import scala.annotation.tailrec

/**
 * Created by abdullah on 1/26/15.
 */
final class QuickUnion(N: Int) extends UnionFind(N) {

  override def union(p: Int, q: Int): UnionFind = {
    val pid = find(p)
    val qid = find(q)

    if (pid != qid) { ids(qid) = pid }

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
