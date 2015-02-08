package exercises

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

/**
 * Created by Abdullah Alansari on 2/2/15.
 */
object SortingSpecification extends Properties("Sort") {

  property("Sorting.quickSelect") = forAll { (range: Byte, k: Byte) =>
    (range > 0 && k >= 1 && k <= range) ==> {
      Sorting.quickSelect((1 to range).toArray, k) == Some(k)
    }
  }

  property("Sorting.quickSelect") = forAll { (range: Byte, k: Byte) =>
    (range > 0 && k > range) ==> {
      Sorting.quickSelect((1 to range).toArray, k) == None
    }
  }

  property("Sorting.insertionSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.insertionSort(xs.toArray).toList
  }

  property("Sorting.selectionSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.selectionSort(xs.toArray).toList
  }

  property("Sorting.shellSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.shellSort(xs.toArray).toList
  }

  property("Sorting.quickSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.quickSort(xs.toArray).toList
  }

  property("Sorting.mergeSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.mergeSort(xs.toArray).toList
  }

  property("Sorting.bubbleSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.bubbleSort(xs.toArray).toList
  }

  property("Sorting.shuffle") = forAll { (xs: List[Byte]) =>
    Sorting.shuffle(xs.toArray).sorted.toList == xs.sorted
  }

  property("Sorting.shuffle") = forAll { (xs: List[Byte]) =>
    (xs.length > 3) ==> {
      Sorting.shuffle(xs.toArray).toList != xs
    }
  }
}
