import AVLTree.balance

import scala.annotation.tailrec
import scala.reflect.ClassTag

case class AVLTree[T](value: T, var left: Option[AVLTree[T]], var right: Option[AVLTree[T]], var height: Int = 0)

object AVLTree {


  def height[T](tree: Option[AVLTree[T]]): Int = {
    tree match {
      case None => 0
      case Some(AVLTree(_, l, r, _)) => 1 + math.max(height(l), height(r))
    }
  }

  def balance[T](tree: AVLTree[T]): Int = {
    height(tree.left) - height(tree.right)
  }



  def rightRotate[T](tree: AVLTree[T]): AVLTree[T] = {
    val leftTree = tree.left.get;
    tree.left = leftTree.right
    leftTree.right = Some(tree)

    leftTree.height = height(Some(leftTree))
    tree.height = height(Some(tree))

    leftTree
  }

  def leftRotate[T](tree: AVLTree[T]): AVLTree[T] = {
    val rightTree = tree.right.get;
    tree.right = rightTree.left
    rightTree.left = Some(tree)

    rightTree.height = height(Some(rightTree))
    tree.height = height(Some(tree))

    rightTree
  }

  def insertBST[T](bst: Option[AVLTree[T]], element: T)(implicit ordering: Ordering[T]): AVLTree[T] = {
    val inserted = bst match {
      case None => AVLTree(element, None, None)
      case Some(a@AVLTree(v, l ,r, _)) if(ordering.gteq(v, element)) => {a.left = Some(insertBST(a.left, element)); a}
      case Some(a@AVLTree(v, l ,r, _)) if(ordering.lt(v, element)) => {a.right = Some(insertBST(a.right, element)); a}
    }
    inserted.height = height(Some(inserted))
    val balanceVal = balance(inserted)
    val balancedTree = balanceVal match {
      case b if b > 1 && ordering.lt(element, inserted.left.get.value) => return rightRotate(inserted) //right rotate
      case b if b > 1 && ordering.gt(element, inserted.left.get.value) => { inserted.left = Some(leftRotate(inserted.left.get)); return rightRotate(inserted) } //left rotate on node.left and right rotate on node
      case b if b < -1 && ordering.gt(element, inserted.right.get.value) => return leftRotate(inserted) //left rotate
      case b if b < -1 && ordering.lt(element, inserted.right.get.value) => { inserted.right = Some(rightRotate(inserted.right.get)); return leftRotate(inserted) } //right rotate on node.right and left rotate on node
      case _ => return inserted
    }

    balancedTree
  }

  def insert[T](bst: AVLTree[T], element: T)(implicit ordering: Ordering[T], c: ClassTag[T]): AVLTree[T] = {
    val inserted = insertBST(Some(bst), element)

    AVLPrettyPrinter.prettyPrint(inserted)
    inserted
  }

  def main(args: Array[String]): Unit = {

      val inputList = List(2,6,3,6,1,8, 43, 2, 4, 9, 323)
      val foldF = (memo: AVLTree[Int], element: Int) => AVLTree.insert(memo, element)
      val avl = inputList.tail.foldLeft(AVLTree(inputList.head, None, None))(foldF)

  }
}