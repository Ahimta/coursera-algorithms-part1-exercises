package exercises.unionfind

/**
  * Created by abdullah on 1/25/15.
  */
abstract class UnionFind(val N: Int) {

  require(N > 0)

  protected var _components = N
  protected val ids         = (0 until N).toArray

  protected def isValidId(p: Int) = p >= 0 && p < N

  def union(p: Int, q: Int): UnionFind

  def find(p: Int): Int

  final def connected(p: Int, q: Int): Boolean = find(p) == find(q)

  final def components: Int = _components

  override def toString: String = s"$N - $components"
 }
