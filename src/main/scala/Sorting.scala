import scala.annotation.tailrec

trait Sort {
  def sort[T](list: List[T])(implicit ordering:Ordering[T]): List[T] ;
}

object InsertionSort extends Sort {

  def insert[T](list: List[T], element: T)(implicit  ordering: Ordering[T]): List[T] = {

    @tailrec def insertHelper[T](list: List[T], element: T, result: List[T])(implicit ordering: Ordering[T]): List[T] = {
      if (list.isEmpty)
        return List(element)
      if (!ordering.gt(list.head, element)) {
        return element :: list
      }
      return  insertHelper(list.tail, element, list.head :: result)
    }
    insertHelper(list, element, List[T]())
  }

  override def sort[T](list: List[T])(implicit ordering: Ordering[T]): List[T] = {
    list.foldLeft(List[T]())((memo, element) => insert(memo, element))
  }
}

object SelectionSort extends Sort {

  def minimum[T](list: List[T])(implicit ordering:Ordering[T]): (T, List[T]) =
    list.tail.foldLeft((list.head, List[T]()))(
      (result, element) => if (ordering.gt(result._1, element) )  (element, result._1 :: result._2) else (result._1, element :: result._2 )
    )

  override def sort[T](list: List[T])(implicit ordering:Ordering[T]): List[T] = {

    @tailrec def sortHelper(list: List[T], result: List[T]): List[T] = {
      if (list.isEmpty) {
        return result
      }
      val minimum = SelectionSort.minimum(list)
      sortHelper(minimum._2, minimum._1 :: result)
    }

    sortHelper(list, List())
  }

}


