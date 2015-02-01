package exercises.stacks

import java.util

/**
 * Created by Abdullah Alansari on 2/1/15.
 */
class StackArray[T] extends Stack[T] {

  override val stack = new util.ArrayList[T]()

  override def peek: T =
    if (isEmpty) { throw new NoSuchElementException() }
    else         { stack.get(length - 1) }

  override def push(x: T): Stack[T] = {
    require(x != null)
    stack.add(x)
    _length += 1
    this
  }

  override def pop: T =
    if (isEmpty) { throw new NoSuchElementException() }
    else {
      _length -= 1
      stack.remove(length)
    }
}
