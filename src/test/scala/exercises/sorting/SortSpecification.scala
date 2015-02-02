package exercises.sorting

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll

/**
 * Created by Abdullah Alansari on 2/2/15.
 */
object SortSpecification extends Properties("Sort") {

  property("p") = forAll { (xs: List[Int]) =>
    xs.sorted == InsertionSort.sort(xs.toArray).toList
  }
}
