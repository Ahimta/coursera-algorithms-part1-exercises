package exercises.queues

import java.util

/**
 * Created by Abdullah Alansari on 2/1/15.
 */
class QueueArray[T] extends Queue[T] {

  override protected val queue: util.List[T] = new util.ArrayList[T]()
}
