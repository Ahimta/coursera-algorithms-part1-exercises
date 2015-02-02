package exercises

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

/**
 * Created by Abdullah Alansari on 2/2/15.
 */
object SortingSpecification extends Properties("Sort") {

  property("Sorting.insertionSort") = forAll { (xs: List[Int]) =>
    xs.sorted == Sorting.insertionSort(xs.toArray).toList
  }

  property("Sorting.selectionSort") = forAll { (xs: List[Int]) =>
    xs.sorted == Sorting.selectionSort(xs.toArray).toList
  }

  property("Sorting.shellSort") = forAll { (xs: List[Int]) =>
    xs.sorted == Sorting.shellSort(xs.toArray).toList
  }

  property("Sorting.shuffle") = forAll { (xs: List[Int]) =>
    Sorting.shuffle(xs.toArray).sorted.toList == xs.sorted
  }

  property("Sorting.shuffle") = forAll { (xs: List[Int]) =>
    (xs.length > 3) ==> {
      Sorting.shuffle(xs.toArray).toList != xs
    }
  }
}
