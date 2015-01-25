package exercises.unionfind

import scala.util.Random

/**
 * Created by abdullah on 1/25/15.
 */
object Helpers {

  private sealed abstract class Operation
  private case class Connected(p: Int, q: Int) extends Operation
  private case class Union(p: Int, q: Int)     extends Operation
  private case class Find(p: Int)      extends Operation

  private def randomOps(N: Int): Seq[Operation] = {
    require(N >= 0)

    (0 until N).map { _ =>
      val p = Random.nextInt(N)
      val q = Random.nextInt(N)

      val op = Random.nextInt(3) match {
        case 0 => Connected(p, q)
        case 1 => Union(p, q)
        case 2 => Find(p)
      }

      op
    }
  }

  private def executeOp(uf: UnionFind, op: Operation): Unit = op match {
    case Connected(p, q) => uf.connected(p, q)
    case Union(p, q)     => uf.union(p, q)
    case Find(p)         => uf.find(p)
  }

  private def executeOps(uf: QuickFind, ops: Seq[Operation]): Unit = ops.foreach(executeOp(uf, _))

  def randQuickFind(N: Int): QuickFind = {
    val ops = randomOps(N)
    val uf  = new QuickFind(N)

    executeOps(uf, ops)
    uf
  }
}
