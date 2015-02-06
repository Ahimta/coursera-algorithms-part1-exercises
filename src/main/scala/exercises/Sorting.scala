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

  private def partition[T](xs: Array[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): (Int, Int) = {

    val pivot = xs(lo)

    var eqlt = lo
    var eqgt = hi
    var lt   = lo
    var gt   = hi

    while (lt <= gt) {
      if      (ord.equiv(xs(lt), pivot)) {
        swap(xs, eqlt, lt)
        eqlt += 1
        lt   += 1
      }
      else if (ord.equiv(xs(gt), pivot)) {
        swap(xs, eqgt, gt)
        eqgt -= 1
        gt   -= 1
      }
      else if (ord.lt(xs(gt), pivot)) {
        swap(xs, lt, gt)
        lt += 1
      }
      else if (ord.gt(xs(lt), pivot)) {
        swap(xs, lt, gt)
        gt -= 1
      }
      else {
        lt += 1
        gt -= 1
      }
    }

    while (eqgt < hi) {
      eqgt += 1
      gt   += 1
      swap(xs, eqgt, gt)
    }

    while (eqlt > lo) {
      eqlt -= 1
      lt   -= 1
      swap(xs, eqlt, lt)
    }

    (lt, gt)
  }

  private def quickSort[T](xs: Array[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): Array[T] = {
    if (lo < hi) {
      val (min, max) = partition(xs, lo, hi)
      quickSort(xs, lo, min - 1)
      quickSort(xs, max + 1, hi)
    }

    xs
  }

  private def hsort[T](xs: Array[T], step: Int)(implicit ord: Ordering[T]): Array[T] = {

    var i = step

    while (i < xs.length) {

      var currentIndex = i
      var prevIndex    = i - step

      while (prevIndex >= 0 && ord.lt(xs(currentIndex), xs(prevIndex))) {

        swap(xs, prevIndex, currentIndex)
        currentIndex -= step
        prevIndex    -= step
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

  def insertionSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = hsort(xs, 1)

  def shellSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    if (xs.length > 1) {

      var k = xs.length

      do {
        k = k / 3 + 1
        hsort(xs, k)
      } while (k > 1)
    }

    xs
  }

  def partition[T](xs: Array[T])(implicit ord: Ordering[T]): (Int, Int) = partition(xs, 0, (xs.length - 1))

  def quickSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] =
    if (xs.isEmpty || xs.length == 1) { xs }
    else {
      shuffle(xs)
      quickSort(xs, 0, (xs.length - 1))
    }
}
