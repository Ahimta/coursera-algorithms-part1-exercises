package exercises.stacks

/**
 * Created by Abdullah Alansari on 2/1/15.
 */
abstract class Stack[T] {

  protected var _length = 0
  protected val stack: List[T]

  final def length: Int = _length

  final def isEmpty: Boolean = {
    assert(_length >= 0)
    _length == 0
  }

  def peek: T

  def push(x: T): Stack[T]

  def pop: T
}
