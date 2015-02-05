package exercises

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

/**
 * Created by Abdullah Alansari on 2/3/15.
 */
object SearchingSpecification extends Properties("Searching") {

  property("Searching.binarySearch") = forAll { (xs: List[Int], x: Int) =>

    val arr = xs.toArray.sorted

    Searching.binarySearch(arr, x) match {
      case Some(_) => java.util.Arrays.binarySearch(arr, x) >= 0
      case None    => java.util.Arrays.binarySearch(arr, x) < 0
    }
  }
}
