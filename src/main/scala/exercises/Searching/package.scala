package exercises

/**
 * Created by Abdullah Alansari on 2/3/15.
 */
package object Searching {

  def binarySearch[T](xs: Array[T], x: T)(implicit ord: Ordering[T]): Option[T] = {

    var found: Option[T] = None

    var max = xs.length - 1
    var min = 0

    while (max >= min && found.isEmpty) {
      val mid     = (max - min) / 2 + min
      val current = xs(mid)

      if      (ord.lt(x, current)) { max = mid - 1 }
      else if (ord.gt(x, current)) { min = mid + 1 }
      else                         { found = Some(current)  }
    }

    found
  }
}
