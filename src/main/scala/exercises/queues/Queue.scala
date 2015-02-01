package exercises.queues

import java.util

/**
 * Created by Abdullah Alansari on 2/1/15.
 */
abstract class Queue[T] {

  protected var _length = 0
  protected val queue: util.List[T]

  final def length: Int = _length

  final def isEmpty: Boolean = {
    assert(_length >= 0)
    _length == 0
  }

  final def enqueue(x: T): Queue[T] = {
    require(x != null)
    _length += 1
    queue.add(x)
    this
  }

  final def dequeue: T =
    if (isEmpty) { throw new NoSuchElementException() }
    else {
      _length -= 1
      queue.remove(0)
    }

  final def peek: T =
    if (isEmpty) { throw new NoSuchElementException() }
    else         { queue.get(0) }
}
