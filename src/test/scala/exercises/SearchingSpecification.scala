package exercises

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

/**
 * Created by Abdullah Alansari on 2/3/15.
 */
object SearchingSpecification extends Properties("Searching") {

  property("Sorting.quickSelect") = forAll { (range: Byte, k: Byte) =>
    (range > 0 && k >= 1 && k <= range) ==> {
      Searching.quickSelect((1 to range).toArray, k, largest = true) == Some(range - k + 1)
    }
  }

  property("Sorting.quickSelect") = forAll { (range: Byte, k: Byte) =>
    (range > 0 && k >= 1 && k <= range) ==> {
      Searching.quickSelect((1 to range).toArray, k) == Some(k)
    }
  }

  property("Searching.binarySearch") = forAll { (xs: List[Int], x: Int) =>

    val arr = xs.toArray.sorted

    Searching.binarySearch(arr, x) match {
      case Some(i) => java.util.Arrays.binarySearch(arr, x) == i
      case None    => java.util.Arrays.binarySearch(arr, x) < 0
    }
  }
}
