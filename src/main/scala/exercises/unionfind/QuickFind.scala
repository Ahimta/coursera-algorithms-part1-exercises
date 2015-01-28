package exercises.unionfind

/**
 * Created by abdullah on 1/25/15.
 */
final class QuickFind(N: Int) extends UnionFind(N) {

  override def union(p: Int, q: Int): UnionFind = {

    val pid = find(p)
    val qid = find(q)

    if (pid != qid) {
      for (i <- 0 until N) yield {
        if (ids(i) == qid) {
          ids(i) = pid
        }
      }
      _components -= 1
    }

    this
  }

  override def find(p: Int): Int = {
    require(isValidId(p))
    ids(p)
  }
}
