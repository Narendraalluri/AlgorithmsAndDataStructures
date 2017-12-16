import scala.annotation.tailrec
import scala.reflect.ClassTag

object BSTPrettyPrinter {

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
              case 'l' => acc(row - x - 1)(col + x + 1 ) = Right('/')
              case 'r' => acc(row - x - 1 )(col - x - 1) = Right('\\')
            }
          }
          lcr match {
            case 'l' => acc(row)(col  ) = Left(v)
            case 'r' => acc(row)(col ) = Left(v)
            case 'c' => acc(row)(col) = Left(v)
          }

          if (row != 0) {
            if ( level == 1) {
              lcr match {
                case 'l' => acc(row - 1)(col + 1) = Right('/')
                case 'r' => acc(row - 1)(col - 1) = Right('\\')
              }
            } else {
              val repeatArrows = 3 * Math.pow(2, level - 2).toInt - 2 ;

              0 to repeatArrows foreach { x => printArrow(lcr, row , col, x) }
            }
          }
          inOrderBFSHelper(ls, acc)
        }
        case (BST(v, Some(l), None), row, col, lcr, level) :: ls => {
          inOrderBFSHelper((l, row + nextRow(level), col - nextRow(level), 'l', level - 1) :: (BST(v, None, None), row, col, lcr, level) :: ls, acc)
        }
        case (BST(v, None, Some(r)), row, col, lcr, level) :: ls => {
          inOrderBFSHelper((BST(v, None, None), row, col, lcr, level) :: (r, row + nextRow(level), col + nextRow(level), 'r', level - 1) :: ls, acc)
        }
        case (BST(v, Some(l), Some(r)), row, col, lcr, level) :: ls => {
          inOrderBFSHelper((l, row + nextRow(level), col - nextRow(level) , 'l', level -1 ) :: (BST(v, None, None), row, col, lcr, level) :: (r, row + nextRow(level), col + nextRow(level)  , 'r', level - 1) :: ls, acc)
        }
      }
    }
    val height = BST.heightTailRec(bst);
    val matrixWidth = 3 * Math.pow(2, height - 2).toInt
    val matrixHeight = 3 * Math.pow(2, height - 1).toInt - 1
    val matrix = Array.ofDim[Either[T, Char]](matrixWidth, matrixHeight)
    val initalWidth = 3*Math.pow(2, height-2).toInt - 1
    println(initalWidth)
    inOrderBFSHelper(List((bst, 0, matrixHeight/2-1, 'c', height)), matrix)
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

  def main(args: Array[String]): Unit = {
    val inputList = List(2,6,3,6,1,7, 0, 2, 4, 8, 9)
    val foldF = (memo: BST[Int], element: Int) => BST.insertTailRec(memo, element)
    val bst = inputList.tail.foldLeft(new BST(inputList.head, None, None))(foldF)
    println(bst)
    println( BSTPrettyPrinter.printTree(BSTPrettyPrinter.inOrderBFS(bst)))
  }
}