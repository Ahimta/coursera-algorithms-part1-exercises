package exercises.stacks

import java.util

/**
 * Created by Abdullah Alansari on 2/1/15.
 */
class ScalaLinkedList[T] extends Stack[T] {

  override val stack = new util.LinkedList[T]()

  override def peek: T =
    if (isEmpty) { throw new NoSuchElementException() }
    else         { stack.get(0) }

  override def push(x: T): Stack[T] = {
    require(x != null)
    _length += 1
    stack.add(x)
    this
  }

  override def pop: T =
    if (isEmpty) { throw new NoSuchElementException() }
    else {
      _length -= 1
      stack.remove(0)
    }
}
