package exercises.unionfind

/**
  * Created by abdullah on 1/25/15.
  */
abstract class UnionFind(N: Int) {

   def union(p: Int, q: Int): UnionFind

   def find(p: Int): Int

   def connected(p: Int, q: Int): Boolean = find(p) == find(q)

   def components: Int
 }
