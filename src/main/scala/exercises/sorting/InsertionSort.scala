package exercises.sorting

/**
 * Created by Abdullah Alansari on 2/2/15.
 */
object InsertionSort extends Sort {

  override def sort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    var isSorted = false
    var i        = 0

    while (i < xs.length && !isSorted) {

      var shifts = 0
      var j      = (i + 1)

      while (j < xs.length) {

        var currentIndex = j
        var prevIndex    = (j - 1)

        while (currentIndex > 0 && ord.lt(xs(currentIndex), xs(prevIndex))) {
          swap(xs, currentIndex, prevIndex)

          currentIndex -= 1
          prevIndex    -= 1
          shifts       += 1
        }

        isSorted = (shifts == 0)
        j += 1
      }

      i += 1
    }

    xs
  }
}
