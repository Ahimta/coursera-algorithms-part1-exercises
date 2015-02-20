package exercises

import scala.annotation.tailrec

/**
 * Created by Abdullah Alansari on 2/20/15.
 */
final class SymbolTable[K, V](implicit ord: Ordering[K]) {

  import SymbolTable._

  private var root: Option[Node[K, V]] = None

  private def size(node: Option[Node[K,V]]): Int = node match {

    case Some(Node(_, _, _, count, _)) => count
    case None                          => 0
  }

  def put(key: K, value: V): SymbolTable[K, V] = {

    def loop(node: Node[K, V]): Unit = {

      if (ord.lt(key, node.key)) {

        node.left match {

          case Some(n) => loop(n)
          case None =>
            node.left = Some(Node(key = key, value = value))
        }
      }

      else if (ord.gt(key, node.key)) {

        node.right match {

          case Some(n) => loop(n)
          case None =>
            node.right = Some(Node(key = key, value = value))
        }
      }

      else { node.value = value }

      node.size = 1 + size(node.left) + size(node.right)
    }

    root match {
      case Some(node) => loop(node)
      case None       =>
        root  = Some(Node(key = key, value = value))
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
      case Some(node) => loop(node)
      case None       => None
    }
  }

  def keys: Seq[K] = {

    val queue = collection.mutable.Queue.empty[K]

    def loop(node: Option[Node[K,V]]): Unit = node match {

      case None =>
      case Some(Node(left, key, _, _, right)) =>

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

  def deleteMax(): Option[K] = {

    def loop(node: Node[K,V]): K = node.right match {

      case None =>

        node match {

          case Node(left, key, _, _, _) =>
            root = left
            root.foreach(n => n.size = 1 + size(n.left) + size(n.right))
            key
        }

      case Some(n@Node(_, _, _, _, Some(_))) =>
        val max = loop(n)
        node.size = 1 + size(node.left) + size(node.right)
        max

      case Some(Node(left, key, _, _, None))    =>
        node.right = left
        node.size  = 1 + size(node.left) + size(node.right)
        key
    }

    root.map(loop)
  }
  
  def deleteMin(): Option[K] = {

    def loop(node: Node[K,V]): K = node.left match {

      case None =>

        node match {

          case Node(_, key, _, _, right) =>

            root = right
            root.foreach(n => n.size = 1 + size(n.left) + size(n.right))
            key
        }

      case Some(n@Node(Some(_), _, _, _, _)) =>
        val min = loop(n)
        node.size = 1 + size(node.left) + size(node.right)
        min

      case Some(Node(None, key, _, _, right))    =>

        node.left = right
        node.size = 1 + size(node.left) + size(node.right)
        key
    }

    root.map(loop)
  }

  def floor(key: K): Option[K] = {

    def loop(node: Option[Node[K,V]]): Option[K] = node match {

      case None => None
      case Some(Node(left, currentKey, _, _, right)) =>

        if (ord.lt(key, currentKey)) { loop(left) }

        else if (ord.gt(key, currentKey)) {

          loop(right) match {

            case Some(k) => Some(k)
            case None    => Some(currentKey)
          }
        }

        else { Some(currentKey) }
    }

    loop(root)
  }

  def ceil(key: K): Option[K] = {

    def loop(node: Option[Node[K,V]]): Option[K] = node match {

      case None => None
      case Some(Node(left, currentKey, _, _, right)) =>

        if (ord.lt(key, currentKey)) {

          loop(left) match {

            case Some(k) => Some(k)
            case None    => Some(currentKey)
          }
        }

        else if (ord.gt(key, currentKey)) { loop(right) }

        else { Some(currentKey) }
    }

    loop(root)
  }

  def size: Int = size(root)

  def isEmpty: Boolean = size == 0

  override def toString: String = root.toString
}

object SymbolTable {

  private case class Node[K,V](var left: Option[Node[K,V]] = None,
                                    key: K,
                               var value: V,
                               var size: Int = 1,
                               var right: Option[Node[K,V]] = None)

  def main(args: Array[String]): Unit = { }
}