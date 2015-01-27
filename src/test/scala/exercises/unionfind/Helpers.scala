package exercises.unionfind

import scala.util.Random

/**
 * Created by abdullah on 1/25/15.
 */
object Helpers {

  private sealed abstract class Operation
  private case object Components               extends Operation
  private case class Connected(p: Int, q: Int) extends Operation
  private case class Union(p: Int, q: Int)     extends Operation
  private case class Find(p: Int)              extends Operation

  private def randomOps(N: Int): Seq[Operation] = {
    require(N >= 0)

    (0 until N).map { _ =>
      val p = Random.nextInt(N)
      val q = Random.nextInt(N)

      val op = Random.nextInt(4) match {
        case 0 => Components
        case 1 => Connected(p, q)
        case 2 => Union(p, q)
        case 3 => Find(p)
      }

      op
    }
  }

  private def executeOp(uf: UnionFind, op: Operation): Unit = op match {
    case Components      => uf.components
    case Connected(p, q) => uf.connected(p, q)
    case Union(p, q)     => uf.union(p, q)
    case Find(p)         => uf.find(p)
  }

  def randQuickFind(N: Int): UnionFind = {
    val ops = randomOps(N)
    val uf  = new WeightedQuickUnionWPC(N)

    ops.foreach(executeOp(uf, _))
    uf
  }
}
