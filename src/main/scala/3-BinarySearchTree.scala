import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

case class BST[T](var value:T, var left: Option[BST[T]], var right: Option[BST[T]]) {
  override def toString: String = {
    "(" + value + "," + left + "," + right + ")"
  }
}

object BST {

  def height[T](bst: Option[BST[T]]): Int = {
    bst match {
      case None => 0
      case Some(BST(_, l, r)) =>  1 + Math.max(height(l), height(r))
    }
  }

  def heightTailRec[T](bst: BST[T]): Int = {
    @tailrec def heightHelper[T](tree: List[(BST[T], Int)], acc: Int): Int = {
      tree match {
        case Nil => acc
        case (BST(v, None, None), level) :: ls => heightHelper(ls, Math.max(level, acc))
        case (BST(v, Some(l), None), level) :: ls => heightHelper((l, level + 1)  :: ls, 0 )
        case (BST(v, None, Some(r)), level) :: ls => heightHelper( (r, level + 1) :: ls, 0 )
        case (BST(v, Some(l), Some(r)), level) :: ls => heightHelper((l, level + 1)  :: (r, level + 1) :: ls, 0)
      }
    }
    heightHelper(List((bst, 1)), 0)
  }

  def inOrder[T](bst:Option[BST[T]]): List[T] = {
    if (bst.isEmpty) {
      return List()
    }
    inOrder(bst.get.left) ::: (bst.get.value :: Nil) ::: inOrder(bst.get.right)
  }

  def inOrderBFS[T](bst: BST[T])(implicit c: ClassTag[T]): Array[Array[Either[T, Char]]] = {
    @tailrec def inOrderBFSHelper[T](tree: List[(BST[T], Int, Int, Char, Int)], acc: Array[Array[Either[T, Char]]]): Array[Array[Either[T, Char]]] = {
      val nextRow = (level: Int) => level match {
        case 1 => 1
        case 2 => 2
        case _ => 3 * math.pow(2, level - 3).toInt
      }
      tree match {
        case Nil => acc
        case (BST(v, None, None), row, col, lcr, level) :: ls => {
          def printArrow(lcr: Char, row: Int, col:Int, x: Int): Unit = {
            lcr match {
              case 'l' => acc(row - x - 1)(col + x) = Right('/')
              case 'r' => acc(row - x - 1 )(col - x - 1) = Right('\\')
            }
          }
          lcr match {
            case 'l' => acc(row)(col - 1) = Left(v)
            case 'r' => acc(row)(col + 1) = Left(v)
            case 'c' => acc(row)(col) = Left(v)
          }

          if (row != 0) {
            if ( level == 1) {
              lcr match {
                case 'l' => acc(row -  1)(col + 1) = Right('/')
                case 'r' => acc(row - 1 )(col - 1) = Right('\\')
              }
            } else {
              val repeatArrows = 3 * Math.pow(2, level -2 ).toInt - 2 ;

              0 to repeatArrows foreach { x => printArrow(lcr, row , col, x) }
            }




          }
          inOrderBFSHelper(ls, acc)
        }
        case (BST(v, Some(l), None), row, col, lcr, level) :: ls => inOrderBFSHelper((l, row + nextRow(level), col - nextRow(level), 'l', level - 1) :: (BST(v, None, None), row, col, lcr, level) :: ls, acc)
        case (BST(v, None, Some(r)), row, col, lcr, level) :: ls => inOrderBFSHelper((BST(v, None, None), row, col, lcr, level) :: (r, row + nextRow(level), col + nextRow(level), 'r', level - 1) :: ls, acc)
        case (BST(v, Some(l), Some(r)), row, col, lcr, level) :: ls => inOrderBFSHelper((l, row + nextRow(level), col - nextRow(level), 'l', level -1 ) :: (BST(v, None, None), row, col, lcr, level) :: (r, row + nextRow(level), col + nextRow(level), 'r', level - 1) :: ls, acc)
      }
    }
    val height = BST.heightTailRec(bst);
    val matrix = Array.ofDim[Either[T, Char]](3 * Math.pow(2, height -2 ).toInt, 3*Math.pow(2, height-1).toInt )
    inOrderBFSHelper(List((bst, 0, 3*Math.pow(2, height-2).toInt - 1, 'c', height)), matrix)
  }

  def printTree[T](matrix: Array[Array[Either[T, Char]]]): Unit = {
    matrix.foreach( row => {
      row.foreach {
        case null => print (" ")
        case Left(l) => print(l)
        case Right(r) => print(r)
      }
      println()
    })
  }

  def inOrderTailRec[T](bst: BST[T]): List[T] = {
    @tailrec def inOrderHelper[T](tree: List[BST[T]], acc: List[T]): List[T] = {
      tree match {
        case Nil => acc
        case BST(v, None, None) :: ls => inOrderHelper(ls, acc :+ v)
        case BST(v, Some(l), None) :: ls => inOrderHelper(l :: BST(v, None, None) :: ls, acc)
        case BST(v, None, Some(r)) :: ls => inOrderHelper(BST(v, None, None) :: r :: ls, acc)
        case BST(v, Some(l), Some(r)) :: ls => inOrderHelper(l :: BST(v, None, None) :: r :: ls, acc)
      }
    }
    inOrderHelper(List(bst), List())
  }

  def insert[T](bst: Option[BST[T]], element:T)(implicit ordering: Ordering[T]): BST[T] = {
    bst match {
      case Some(BST(v, l, r)) => {
        if (ordering.gteq(element, v)) {
          BST(v, l, Some(insert(r, element)))
        } else {
          BST(v, Some(insert(l, element)), r)
        }
      }
      case None => BST(element, None, None)
    }
  }

  def insertTailRec[T](bst: BST[T], element:T)(implicit ordering: Ordering[T]): BST[T] = {
    @tailrec def findSuitableParent[T](bst: BST[T], element:T, parent: BST[T])(implicit ordering: Ordering[T]): BST[T] = {
        bst match {
          case a@BST(v, Some(l), _) if ordering.gteq(v, element) =>  findSuitableParent(l, element, a)
          case a@BST(v, None, _) if ordering.gteq(v, element) =>  a
          case a@BST(v, _, Some(r)) if ordering.lt(v, element) =>  findSuitableParent(r, element, a)
          case a@BST(v, _, None) if ordering.lt(v, element) =>  a
      }
    }

    val suitableParent = findSuitableParent(bst, element, bst)
    suitableParent match {
      case a@BST(v, None, _) if ordering.gteq(v, element) =>  a.left = Some(BST(element, None, None))
      case a@BST(v, _, None) if ordering.lt(v, element) =>  a.right = Some(BST(element, None, None))
    }
    bst
  }


  @tailrec def search[T](bst: Option[BST[T]], element:T)(implicit ordering: Ordering[T]): Boolean = {
    if (bst.isEmpty) {
      return false
    }
    if (bst.get.value == element) {
      true
    } else if(ordering.gt(bst.get.value, element)) {
      search(bst.get.left, element)
    } else {
      search(bst.get.right, element)
    }

  }
}

object Test extends App {
  val inputList = List(2,6,3,6,1,8, 0, 2, 4, 9, 323)
  val foldF = (memo: BST[Int], element: Int) => BST.insertTailRec(memo, element)
  val bst = inputList.tail.foldLeft(new BST(inputList.head, None, None))(foldF)
  println( BST.printTree(BST.inOrderBFS(bst)))
  println(BST.height(Some(bst)))
  println(BST.heightTailRec(bst))
  val outputList = BST.inOrder(Some(bst))
  val outputListRec = BST.inOrderTailRec(bst)
  println(outputList)
  println(outputListRec)
}