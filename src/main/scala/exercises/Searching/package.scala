package exercises

import scala.annotation.tailrec

/**
 * Created by Abdullah Alansari on 2/3/15.
 */
package object Searching {

  def binarySearch[T](xs: Array[T], x: T)(implicit ord: Ordering[T]): Option[Int] = {

    var found: Option[Int] = None

    var max = xs.length - 1
    var min = 0

    while (max >= min && found.isEmpty) {
      val mid     = (max - min) / 2 + min
      val current = xs(mid)

      if      (ord.lt(x, current)) { max = mid - 1 }
      else if (ord.gt(x, current)) { min = mid + 1 }
      else                         { found = Some(mid)  }
    }

    found
  }

  def quickSelect[T](xs: Array[T], k: Int, largest: Boolean = false)(implicit ord: Ordering[T]): Option[T] = {

    require(k >= 1 && k <= xs.length)

    val kth = if (largest) (xs.length - k + 1) else k

    @tailrec
    def helper(lo: Int, hi: Int): Option[T] = {
      if (lo < hi) {
        val (min, max) = Sorting.partition(xs, lo, hi)

        if      ((kth - 1) < min) { helper(lo, (min - 1)) }
        else if ((kth - 1) > max) { helper((max + 1), hi) }
        else                      { Some(xs(min)) }

      }
      else if (lo == hi && (kth - 1) == lo) { Some(xs(lo)) }
      else { None }
    }

    helper(0, (xs.length - 1))
  }

  def median[T](xs: Array[T])(implicit ord: Ordering[T]): Option[T] =
    if (xs.length % 2 == 0) { None }
    else { quickSelect(xs, (xs.length / 2) + 1) }
}
