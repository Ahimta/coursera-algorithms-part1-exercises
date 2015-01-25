package exercises.unionfind

/**
 * Created by abdullah on 1/25/15.
 */
class QuickFind(val N: Int) extends UnionFind(N) {

  var _components = N
  var ids         = (0 until N).toArray

  override def union(p: Int, q: Int): UnionFind = {
    require(p < N && p >= 0)

    if (p == q) { this }
    else {
      ids = ids.map(x => if (x == q) p else x)
      _components -= 1
      this
    }
  }

  override def components: Int = _components

  override def find(p: Int): Int = {
    require(p < N && p >= 0)
    ids(p)
  }
}
