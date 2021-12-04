// A binary tree.
case class BinTree[A](value: A, left: Option[BinTree[A]], right: Option[BinTree[A]])

sealed trait Dir
object Dir {
  case object Left  extends Dir
  case object Right extends Dir
}
case class Backup[A](dir: Dir, focusValue: A, oppositeNode: Option[BinTree[A]])
case class Zipper[A](focus: BinTree[A], backups: List[Backup[A]] = List.empty)

object Zipper {
  // A zipper for a binary tree.

  // Get a zipper focussed on the root node.
  def fromTree[A](bt: BinTree[A]): Zipper[A] = Zipper(bt)

  // Get the complete tree from a zipper.
  def toTree[A](zipper: Zipper[A]): BinTree[A] =
    zipper.backups.foldLeft(zipper.focus) { (bt, backup) =>
      backup.dir match {
        case Dir.Left  => BinTree(backup.focusValue, Some(bt), backup.oppositeNode)
        case Dir.Right => BinTree(backup.focusValue, backup.oppositeNode, Some(bt))
      }
    }

  // Get the value of the focus node.
  def value[A](zipper: Zipper[A]): A = zipper.focus.value

  // Get the left child of the focus node, if any.
  def left[A](zipper: Zipper[A]): Option[Zipper[A]] =
    zipper.focus.left.map { newFocus =>
      Zipper(
        newFocus,
        Backup(Dir.Left, zipper.focus.value, zipper.focus.right) :: zipper.backups
      )
    }

  // Get the right child of the focus node, if any.
  def right[A](zipper: Zipper[A]): Option[Zipper[A]] =
    zipper.focus.right.map { newFocus =>
      Zipper(
        newFocus,
        Backup(Dir.Right, zipper.focus.value, zipper.focus.left) :: zipper.backups
      )
    }

  // Get the parent of the focus node, if any.
  def up[A](zipper: Zipper[A]): Option[Zipper[A]] =
    zipper.backups.headOption.map { backup =>
      backup.dir match {
        case Dir.Left  =>
          Zipper(
            BinTree(backup.focusValue, Some(zipper.focus), backup.oppositeNode),
            zipper.backups.tail
          )
        case Dir.Right =>
          Zipper(
            BinTree(backup.focusValue, backup.oppositeNode, Some(zipper.focus)),
            zipper.backups.tail
          )
      }
    }

  // Set the value of the focus node.
  def setValue[A](v: A, zipper: Zipper[A]): Zipper[A] =
    zipper.copy(focus = zipper.focus.copy(value = v))

  // Replace a left child tree.
  def setLeft[A](l: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] =
    zipper.copy(focus = zipper.focus.copy(left = l))

  // Replace a right child tree.
  def setRight[A](r: Option[BinTree[A]], zipper: Zipper[A]): Zipper[A] =
    zipper.copy(focus = zipper.focus.copy(right = r))

}
