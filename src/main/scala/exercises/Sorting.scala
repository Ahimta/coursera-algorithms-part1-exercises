package exercises

import scala.util.Random

/**
 * Created by Abdullah Alansari on 2/3/15.
 */
object Sorting {

  private def swap[T](xs: Array[T], i: Int, j: Int): Unit = {
    val tmp = xs(i)
    xs(i) = xs(j)
    xs(j) = tmp
  }

  private def insertionSort[T](xs: Array[T], step: Int)(implicit ord: Ordering[T]): Array[T] = {

    var isSorted = false
    var i        = 0

    while (i < xs.length && !isSorted) {

      var shifts = 0
      var j      = i + step

      while (j < xs.length) {

        var currentIndex = j
        var prevIndex    = j - step

        while (prevIndex >= 0 && ord.lt(xs(currentIndex), xs(prevIndex))) {
          swap(xs, currentIndex, prevIndex)

          currentIndex -= step
          prevIndex    -= step
          shifts       += 1
        }

        isSorted = shifts == 0
        j += step
      }

      i += 1
    }

    xs
  }

  def shuffle[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    for (i <- 1 until xs.length) yield {
      swap(xs, i, Random.nextInt(i))
    }

    xs
  }

  def selectionSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    var i = 0

    while (i < xs.length) {

      var currentMinIndex = i
      var currentMin      = xs(i)
      var j               = i + 1

      while (j < xs.length) {

        val currentElt = xs(j)

        if (ord.lt(currentElt, currentMin)) {
          currentMinIndex = j
          currentMin      = currentElt
        }

        j += 1
      }

      swap(xs, i, currentMinIndex)

      i += 1
    }

    xs
  }

  def insertionSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = insertionSort(xs, 1)

  def shellSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    if (xs.length > 1) {

      var k = xs.length

      do {
        k /= 2
        insertionSort(xs, k)
      } while (k > 1)
    }

    xs
  }
}
