package exercises

import scala.reflect.ClassTag

/**
 * Created by Abdullah Alansari on 2/17/15.
 */

object PriorityQueue {

  import scala.annotation.tailrec

  def swap[T](xs: Array[T], i: Int, j: Int): Array[T] = {

    require(i >= 0 && i < xs.length && j >= 0 && j < xs.length)

    val tmp = xs(i)

    xs(i) = xs(j)
    xs(j) = tmp

    xs
  }

  @tailrec
  def sink[T](xs: Array[T], rank: Int, lo: Int, hi: Int)(implicit ord: Ordering[T]): Array[T] = {

    require(lo >= 1 && hi <= xs.length)
    require(rank >= lo && rank <= hi)
    require(hi >= lo)

    val childRank1 = rank * 2
    val childRank2 = childRank1 + 1

    val currentIndex = rank - 1
    val childIndex1  = childRank1 - 1
    val childIndex2  = childRank2 - 1

    if (childRank1 == hi && ord.gt(xs(childIndex1), xs(currentIndex))) { swap(xs, currentIndex, childIndex1) }
    else if (childRank1 < hi) {

      val childValue1 = xs(childIndex1)
      val childValue2 = xs(childIndex2)

      if      (ord.gteq(childValue1, childValue2) && ord.gt(childValue1, xs(currentIndex))) {
        swap(xs, currentIndex, childIndex1)
        sink(xs, childRank1, lo, hi)
      }
      else if (ord.gteq(childValue2, childValue1) && ord.gt(childValue2, xs(currentIndex))) {
        swap(xs, currentIndex, childIndex2)
        sink(xs, childRank2, lo, hi)
      }
      else {
        xs
      }
    }
    else {
      xs
    }
  }

  @tailrec
  def swim[T](xs: Array[T], rank: Int, lo: Int, hi: Int)(implicit ord: Ordering[T]): Array[T] = {

    require(lo >= lo && hi <= xs.length)
    require(rank >= lo && rank <= hi)
    require(hi >= lo)

    val parentRank = rank / 2

    val currentIndex = rank - 1
    val parentIndex  = parentRank - 1

    if (parentRank >= lo && ord.gt(xs(currentIndex), xs(parentIndex))) {

      swap(xs, parentIndex, currentIndex)
      swim(xs, parentRank, lo, hi)
    }
    else {
      xs
    }
  }
}

final class PriorityQueue[T](val N: Int)(implicit ord: Ordering[T], tag: ClassTag[T]) {

  require(N >= 0)

  private var _size = 0
  private val xs    = new Array[T](N)

  private def swim(rank: Int): Array[T] = PriorityQueue.swim(xs, rank, 1, size)

  private def sink(rank: Int): Array[T] = PriorityQueue.sink(xs, rank, 1, size)

  def isEmpty: Boolean = _size == 0

  def isFull: Boolean  = _size == N

  def size: Int = _size

  def insert(x: T): PriorityQueue[T] =
    if (isFull) { throw new IllegalStateException }
    else {
      xs(size) = x
      _size += 1
      if (size > 1) { swim(size) }
      this
    }

  def delete: T =
    if (isEmpty) { throw new NoSuchElementException }
    else {
      val x = xs(0)
      _size -= 1
      PriorityQueue.swap(xs, 0, size)
      if (size > 1) { sink(1) }
      x
    }

  def peek: T =
    if (isEmpty) { throw new NoSuchElementException }
    else         { xs(0) }
}
