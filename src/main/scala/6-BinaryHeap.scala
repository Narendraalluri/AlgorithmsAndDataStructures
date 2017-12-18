

case class BinaryHeap[T](var array: List[T])

object BinaryHeap {

  def swap[T](array: Array[T], leftIndex: Int, righIndex: Int): Unit = {
    val temp = array(leftIndex)
    array(leftIndex) = array(righIndex)
    array(righIndex) = temp
  }

  def bubbleUp[T](array: Array[T], index: Int)(implicit ordering: Ordering[T]): Array[T] = {
    if(index < 2) {
      return array
    }
    if(ordering.gt(array(index/2), array(index))) {
      swap(array, index/2, index)
      return bubbleUp(array, index/2)
    }
    array
  }

  def bubbleDown[T](array: Array[T], index: Int)(implicit ordering: Ordering[T]): Array[T] = {

    if(index * 2 < array.size && ordering.lt(array(index * 2), array(index))) {
      swap(array, index * 2, index)
      bubbleDown(array, index * 2)
    }
    if ((index * 2) + 1 < array.size && ordering.lt(array((index * 2) + 1), array(index))) {
      swap(array, (index * 2) + 1, index)
      bubbleDown(array, (index * 2) + 1)
    }
    array
  }

  def apply[T](array: Array[T])(implicit ordering: Ordering[T]): Array[T] = {
    1 to array.size - 1 foreach {
      index => {
        bubbleUp(array, index)
      }
    }

    array
  }


  def removeMin[T](array: Array[T])(implicit ordering: Ordering[T]): Array[T] = {
    if (array.size <= 2) {
      return array
    }
    val min = array(1)
    val last = array.last
    val newArray = array.dropRight(1)
    newArray(1) = last
    bubbleDown(newArray, 1)
  }

  def main(args: Array[String]): Unit = {

    val heap = BinaryHeap(Array(0, 9, 4, 2, 8, 3))

    print(heap(1), " -> ")
    heap foreach(print)
    println()

    var newArray = heap
    0 to heap.size - 3 foreach((index) => {
      newArray = BinaryHeap.removeMin(newArray)
      print(newArray(1), " -> ")
      newArray foreach(print)
      println()
    })


  }
}