
class Stack[T](val list: List[T] = List()) {

  def push(element: T): Stack[T] = new Stack(element :: list)

  def pop(): T = list.head

}

class Queue[T](val list: List[T] = List()) {
  def enqueue(element: T): Queue[T] = new Queue(list :+ element)

  def dequeue(): Option[T] = list match {
    case _ :: _ => Some(list.last)
    case Nil => None
  }
}