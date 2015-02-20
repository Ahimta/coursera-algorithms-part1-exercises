package exercises

import scala.annotation.tailrec

/**
 * Created by Abdullah Alansari on 2/20/15.
 */
final class SymbolTable[K, V](implicit ord: Ordering[K]) {

  import SymbolTable._

  private var root: Option[Node[K, V]] = None
  private var _size = 0

  def put(key: K, value: V): SymbolTable[K, V] = {

    @tailrec
    def loop(node: Node[K, V]): Unit = {

      if (ord.lt(key, node.key)) {

        node.left match {

          case Some(n) => loop(n)
          case None =>
            node.left = Some(Node(key = key, value = value))
            _size += 1
        }
      }
      else if (ord.gt(key, node.key)) {

        node.right match {

          case Some(n) => loop(n)
          case None =>
            node.right = Some(Node(key = key, value = value))
            _size += 1
        }
      }
      else {
        node.value = value
      }
    }

    root match {
      case None       =>
        root  = Some(Node(key = key, value = value))
        _size = 1
      case Some(node) => loop(node)
    }

    this
  }

  def get(key: K): Option[V] = {

    def loop(node: Node[K,V]): Option[V] = {

      if (ord.lt(key, node.key)) {

        node.left match {

          case Some(n) => loop(n)
          case None    => None
        }
      }
      else if (ord.gt(key, node.key)) {

        node.right match {

          case Some(n) => loop(n)
          case None    => None
        }
      }
      else {
        Some(node.value)
      }
    }

    root match {
      case None       => None
      case Some(node) => loop(node)
    }
  }

  def keys: Seq[K] = {

    val queue = collection.mutable.Queue.empty[K]

    def loop(node: Option[Node[K,V]]): Unit = node match {

      case None =>
      case Some(Node(left, key, _, right)) =>

        queue += key
        loop(left)
        loop(right)
    }

    loop(root)
    queue
  }

  def max: Option[K] = {

    @tailrec
    def loop(node: Node[K,V]): K = node.right match {

      case Some(n) => loop(n)
      case None    => node.key
    }

    root.map(loop)
  }

  def min: Option[K] = {

    @tailrec
    def loop(node: Node[K,V]): K = node.left match {

      case Some(n) => loop(n)
      case None    => node.key
    }

    root.map(loop)
  }

  def size: Int = _size

  def isEmpty: Boolean = _size == 0
}

object SymbolTable {

  private case class Node[K,V](var left: Option[Node[K,V]] = None,
                               val key: K,
                               var value: V,
                               var right: Option[Node[K,V]] = None)
}