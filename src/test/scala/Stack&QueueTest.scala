import org.scalacheck.{Properties}
import org.scalacheck.Prop.forAll

object StackAndQueueSpecification extends Properties("Stack&Queue") {

  property("StackPushPop") = forAll {
    (list: List[Int], element: Int) => new Stack(list).push(element).pop() == element
  }

  property("Enqueue") = forAll {
    (list: List[Int], element: Int) => new Queue(list).enqueue(element).list.last == element
  }

  property("Dequeue") = forAll {
    (list: List[Int]) => {
      val result = new Queue(list).dequeue()
      list match {
        case _ :: _ => result == Some(list.last)
        case Nil => result == None
      }
    }
  }
}