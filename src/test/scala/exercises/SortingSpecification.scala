package exercises

import org.scalacheck.Prop.{BooleanOperators, forAll}
import org.scalacheck.Properties

/**
 * Created by Abdullah Alansari on 2/2/15.
 */
object SortingSpecification extends Properties("Sorting") {

  property("insertionSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.insertionSort(xs.toArray).toList
  }

  property("selectionSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.selectionSort(xs.toArray).toList
  }

  property("shellSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.shellSort(xs.toArray).toList
  }

  property("quickSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.quickSort(xs.toArray).toList
  }

  property("mergeSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.mergeSort(xs.toArray).toList
  }

  property("bubbleSort") = forAll { (xs: List[Byte]) =>
    xs.sorted == Sorting.bubbleSort(xs.toArray).toList
  }

  property("shuffle") = forAll { (xs: List[Byte]) =>
    Sorting.shuffle(xs.toArray).sorted.toList == xs.sorted
  }

  property("shuffle") = forAll { (xs: List[Byte]) =>
    (xs.length > 3) ==> {
      Sorting.shuffle(xs.toArray).toList != xs
    }
  }
}
