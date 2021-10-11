package datastructure

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }
  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))

  // practice 3.2
  def tail[A](list: List[A]): List[A] = list match {
    case Cons(x, xs) => xs
    case Nil => Nil
  }

  // practice 3.3
  def setHead[A](list: List[A], newHead: A): List[A] = list match {
    case Cons(_, xs) => Cons(newHead, xs)
    case Nil => Nil
  }

  // practice 3.4
  def drop[A](list: List[A], n: Int): List[A] = {
    if (n < 1) list
    else {
      list match {
        case Cons(_, xs) => drop(xs, n - 1)
        case Nil => Nil
      }
    }
  }

  // practice 3.5
//  def dropWhile[A](l: datastructure.List[A], f: A => Boolean): datastructure.List[A] = l match {
//    case datastructure.Cons(x, xs) => if (f(x)) datastructure.Cons(x, dropWhile(xs, f)) else dropWhile(xs, f);
//    case datastructure.Nil => datastructure.Nil
//  }
  // refs: https://alvinalexander.com/scala/how-to-use-if-then-expressions-guards-in-case-statements-scala/
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if (f(x)) => dropWhile(xs, f)
    case _ => l
  }

  // practice 3.6
  def init[A](l: List[A]): List[A] = l match {
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
    case Nil => Nil
  }

  // practice 3.7
  def foldRight[A, B](l: List[A], z: B)(f: (A,B) => B): B = {
//    println(l)
//    println(z)
    l match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  def sum2(ns: List[Int]) = foldRight(ns, 0)(_ + _)

  def product2(ns: List[Double]) = foldRight(ns, 1.0)(_ * _)

  // practice 3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => 1 + acc)

  // practice 3.10
  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // practice 3.11
  def sumViaFoldLeft(ns: List[Int]) = foldLeft(ns, 0)(_ + _)
  def productViaFoldLeft(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)
  def lengthViaFoldLeft(ns: List[Double]) = foldLeft(ns, 0)((acc, x) => acc + 1)

  // practice 3.12
  def reverseList[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((accList, x) => Cons(x, accList))

  // practice 3.13 hard 어려움, 못품
  def foldLeftViaFoldRight[A, B](l: List[A], acc: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a: A, g: (B => B)) => (b: B) => g(f(b, a)))(acc)

  // practice 3.14
  def appendViaFoldRight[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))
//  def appendViaFoldLeft[A](a1: datastructure.List[A], a2: datastructure.List[A]): datastructure.List[A] = foldLeft(a1, a2)((acc, a) => datastructure.Cons(a, acc))

  // practice 3.15
  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  // practice 3.16
  def incrementEach(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((x, acc) => Cons(x + 1, acc))

  // practice 3.17
  def doubleToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((x, acc) => Cons(x.toString, acc))

  // practice 3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((a, acc) => Cons(f(a), acc))

  // practice 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, acc) => if f(a) then Cons(a, acc) else acc)

  // practice 3.20
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B])((a, acc) => append(f(a), acc))

  def flatMapViaConcat[A,B](as: List[A])(f: A => List[B]): List[B] =
    concat(map(as)(f))

  // practice 3.21
  def filterViaFlatMap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)((x) => if f(x) then List(x) else Nil: List[A])

  // practice 3.22
  def sumWith(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumWith(xs, ys))
  }

  // practice 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (x: A, y: B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  // practice 3.24
  @annotation.tailrec
  def startsWith[A](sup: List[A], prefix: List[A]): Boolean = (sup, prefix) match {
    case (_, Nil) => true
    case (Cons(a, as), Cons(b, bs)) if a == b => startsWith(as, bs)
    case _ => false
  }

  @annotation.tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sup == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(x, xs) => hasSubSequence(xs, sub)
  }
}