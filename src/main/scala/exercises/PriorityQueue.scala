package exercises

import scala.annotation.tailrec
import scala.reflect.ClassTag

/**
 * Created by Abdullah Alansari on 2/17/15.
 */
final class PriorityQueue[T](val N: Int)(implicit ord: Ordering[T], tag: ClassTag[T]) {

  require(N >= 0)

  private var _size = 0
  private val xs    = new Array[T](N + 1)

  private def swap(i: Int, j: Int): Unit = {
    val tmp = xs(i)
    xs(i) = xs(j)
    xs(j) = tmp
  }

  @tailrec
  private def swim(i: Int): Unit = {

    require(i >= 1 && i <= size)

    if (i / 2 >= 1 && ord.gt(xs(i), xs(i / 2))) {

      swap((i / 2), i)
      swim(i / 2)
    }
  }

  @tailrec
  private def sink(i: Int): Unit = {

    if (i * 2 == size && ord.gt(xs(i * 2), xs(i))) { swap(i, i * 2) }
    else if (i * 2 < size) {

      val x1 = xs(i * 2)
      val x2 = xs(i * 2 + 1)

      if      (ord.gteq(x1, x2) && ord.gt(x1, xs(i))) {
        swap(i, (i * 2))
        sink(i * 2)
      }
      else if (ord.gteq(x2, x1) && ord.gt(x2, xs(i))) {
        swap(i, (i * 2 + 1))
        sink(i * 2 + 1)
      }
    }
  }

  def isEmpty: Boolean = (_size == 0)

  def isFull: Boolean  = (_size == N)

  def size: Int = _size

  def insert(x: T): PriorityQueue[T] =
    if (isFull) { throw new IllegalStateException }
    else {
      _size += 1
      xs(size) = x
      swim(size)
      this
    }

  def delete: T =
    if (isEmpty) { throw new NoSuchElementException }
    else {
      val x = xs(1)
      swap(1, size)
      _size -= 1
      sink(1)
      x
    }

  def peek: T =
    if (isEmpty) { throw new NoSuchElementException }
    else         { xs(1) }
}
