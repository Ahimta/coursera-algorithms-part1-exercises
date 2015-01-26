package exercises.unionfind

/**
 * Created by abdullah on 1/26/15.
 */
class QuickUnion(N: Int) extends UnionFind(N) {

  override def union(p: Int, q: Int): UnionFind = {
    val pid = find(p)
    val qid = find(q)

    if (pid == qid) { this }
    else {
      ids(qid) = pid
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
