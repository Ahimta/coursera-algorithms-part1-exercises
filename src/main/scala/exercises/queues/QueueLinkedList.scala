package exercises.queues

import java.util

/**
 * Created by Abdullah Alansari on 2/1/15.
 */
class QueueLinkedList[T] extends Queue[T] {

  override protected val queue: util.List[T] = new util.LinkedList[T]()
}
