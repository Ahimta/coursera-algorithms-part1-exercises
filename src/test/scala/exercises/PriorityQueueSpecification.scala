package exercises

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

/**
 * Created by Abdullah Alansari on 2/17/15.
 */
object PriorityQueueSpecification extends Properties("PriorityQueue") {

  property("sorted") = forAll { (xs: List[Byte]) =>

    val queue = new PriorityQueue[Byte](xs.length)

    xs.foldLeft(queue)(_.insert(_))

    xs.map(_ => queue.delete) == xs.sorted.reverse
  }
}
