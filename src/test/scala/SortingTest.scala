import org.scalacheck.{Properties}
import org.scalacheck.Prop.forAll


object SortSpecification extends Properties("Sort") {

  def isSorted[T](list: List[T])(implicit ordering:Ordering[T]) : Boolean = {
    if (list.size < 2)
      return true
    return (ordering.gteq(list.head, list.tail.head)) && isSorted(list.tail);
  }

  property("SelectionSort") = forAll {
    (list: List[Int]) => isSorted(SelectionSort.sort(list))
  }

  property("InsertionSort") = forAll {
    (list: List[Int]) => isSorted(InsertionSort.sort(list))
  }

}