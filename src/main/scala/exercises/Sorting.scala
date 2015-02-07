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

  private def merge[T](xs: Array[T], ys: Array[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): Array[T] = {

    for (i <- (lo to hi)) yield {
      ys(i) = xs(i)
    }

    val midIndex = (hi - lo) / 2 + lo

    var mergedIndex = lo
    var rightIndex  = midIndex + 1
    var leftIndex   = lo

    while (leftIndex <= midIndex || rightIndex <= hi) {

      if      (rightIndex > hi || (leftIndex <= midIndex && ord.lteq(ys(leftIndex), ys(rightIndex)))) {
        xs(mergedIndex) = ys(leftIndex)
        leftIndex += 1
      }
      else if (leftIndex > midIndex || (rightIndex <= hi && ord.lteq(ys(rightIndex), ys(leftIndex)))) {
        xs(mergedIndex) = ys(rightIndex)
        rightIndex += 1
      }

      mergedIndex += 1
    }

    xs
  }

  private def quickSort[T](xs: Array[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): Array[T] = {
    if (lo < hi) {
      val (min, max) = partition(xs, lo, hi)
      quickSort(xs, lo, min - 1)
      quickSort(xs, max + 1, hi)
    }

    xs
  }

  private def mergeSort[T](xs: Array[T], ys: Array[T], lo: Int, hi: Int)(implicit ord: Ordering[T]): Array[T] = {

    if (lo < hi) {
      val mid = (hi - lo) / 2 + lo

      mergeSort(xs, ys, lo, mid)
      mergeSort(xs, ys, (mid + 1), hi)

      merge(xs, ys, lo, hi)
    }

    xs
  }

  private def hsort[T](xs: Array[T], step: Int)(implicit ord: Ordering[T]): Array[T] = {

    for (i <- (step until xs.length)) yield {

      var currentIndex = i
      var prevIndex    = i - step

      while (prevIndex >= 0 && ord.lt(xs(currentIndex), xs(prevIndex))) {

        swap(xs, prevIndex, currentIndex)
        currentIndex -= step
        prevIndex    -= step
      }
    }

    xs
  }

  def shuffle[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    for (i <- 1 until xs.length) yield {
      swap(xs, i, Random.nextInt(i))
    }

    xs
  }

  def partition[T](xs: Array[T])(implicit ord: Ordering[T]): (Int, Int) = partition(xs, 0, (xs.length - 1))

  def merge[T](xs: Array[T], ys: Array[T])(implicit ord: Ordering[T]): Array[T] = merge(xs, ys, 0, (xs.length - 1))

  def selectionSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    for (i <- (0 until xs.length)) yield {

      var minIndex = i
      var min      = xs(i)

      for (j <- ((i + 1) until xs.length)) yield {

        val current = xs(j)

        if (ord.lt(current, min)) {
          minIndex = j
          min      = current
        }
      }

      swap(xs, i, minIndex)
    }

    xs
  }

  def insertionSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = hsort(xs, 1)

  def shellSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] = {

    if (xs.length > 1) {

      val increments = Stream.iterate(1)(3 * _ + 1).takeWhile(_ < xs.length).reverse
      increments.foreach(hsort(xs, _))
    }

    xs
  }

  def quickSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] =
    if (xs.isEmpty || xs.length == 1) { xs }
    else {
      shuffle(xs)
      quickSort(xs, 0, (xs.length - 1))
    }

  def mergeSort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T] =
    if (xs.isEmpty || xs.length == 1) { xs }
    else {
      val ys = xs.clone()
      mergeSort(xs, ys, 0, (xs.length - 1))
    }
}
