package exercises.sorting

/**
 * Created by Abdullah Alansari on 2/2/15.
 */
trait Sort {

  protected def swap[T](xs: Array[T], i: Int, j: Int): Unit = {
    val tmp = xs(i)
    xs(i) = xs(j)
    xs(j) = tmp
  }

  def sort[T](xs: Array[T])(implicit ord: Ordering[T]): Array[T]
}
