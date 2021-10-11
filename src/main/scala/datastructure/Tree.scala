package datastructure

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // practice 3.25
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(a) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

//  def max[A](tree: Tree[A]): A = tree match {
//    case Leaf(a) => a
//    case Branch(left, right) => max(left). max(right)
//  }

  // practice 3.26
  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(a) => a
    case Branch(left, right) => max(left).max(max(right))
  }

  // practice 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  // practice 3.28
  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(a) => Leaf(f(a))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // practice 3.29
  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(a) => f(a)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 1)((x, y) => 1 + x + y)

  def maxViaFold(tree: Tree[Int]): Int = fold(tree)((leaf) => leaf)((l, r) =>l.max(r))

  def depthViaFold[A](tree: Tree[A]): Int = fold(tree)(_ => 0)((l, r) => 1 + l + r)

  def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] = fold(tree)(a => Leaf(f(a)))((l: Tree[B], r: Tree[B]) => Branch(l,r))
}
