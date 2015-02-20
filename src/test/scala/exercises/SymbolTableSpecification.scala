package exercises

import org.scalacheck.{Gen, Arbitrary, Properties}
import org.scalacheck.Prop.{BooleanOperators, forAll, all}

import scala.annotation.tailrec
import scala.util.Random

/**
 * Created by Abdullah Alansari on 2/20/15.
 */
object SymbolTableSpecification extends Properties("SymbolTable") {

  private sealed abstract class Operation
  private final case class  Put(key: String, value: Int) extends Operation
  private final case class  Get(key: String)             extends Operation
  private       case object Keys                         extends Operation
  private       case object Size                         extends Operation

  private def randomOps(N: Int): Seq[Operation] = {

    (1 to N).map { _ =>

      Random.nextInt(4) match {

        case 0 => Put(key = Random.nextString(16), value = Random.nextInt())
        case 1 => Get(key = Random.nextString(16))
        case 2 => Keys
        case 3 => Size
      }
    }
  }

  private def randomSymbolTable(N: Int): SymbolTable[String,Int] = {

    val table = new SymbolTable[String, Int]()
    val ops   = randomOps(N)

    ops.foreach {
      case Put(k, v) => table.put(k, v)
      case Get(k)    => table
      case Keys      => table.keys
      case Size      => table.size
    }

    table
  }

  private def isSorted[T](xs: Array[T])(implicit ord: Ordering[T]): Boolean = {

    @tailrec
    def loop(i: Int): Boolean = {

      if (i == 0) { true }
      else if (i > 0 && ord.lteq(xs(i - 1), xs(i))) {
        loop(i - 1)
      }
      else { false }
    }

    loop(xs.length - 1)
  }

  implicit def arbSymbolTable: Arbitrary[SymbolTable[String,Int]] = Arbitrary {
    Gen.sized(randomSymbolTable)
  }

  property("put get") = forAll { (table: SymbolTable[String, Int], key: String, value: Int) =>

    table.put(key, value).get(key).get == value &&
    isSorted(Stream.from(1).map(_ => table.deleteMin()).takeWhile(!_.isEmpty).map(_.get).toArray)
  }
}
