import datastructure.Cons
import datastructure.List.{appendViaFoldRight, drop, dropWhile, filterViaFlatMap, flatMap, flatMapViaConcat, foldRight, init, length, product2, reverseList, setHead, sum, sumWith, tail}

import datastructure.{ Tree, Branch, Leaf }
import datastructure.Tree.{
  size,
  max,
  depth,
  map,
  fold,
  sizeViaFold
}

object Chapter3 {
  def main(args: Array[String]): Unit = {
    // pracetice 3.1
    val x = datastructure.List(1,2,3,4,5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case datastructure.Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    println(x)

    // practice 3.2
    val y = tail(datastructure.List(datastructure.Nil,2,3))
    println(y)

    // practice 3.3
    val z = setHead(datastructure.List(1,2,3), 10)
    println(z)

    // practice 3.4
    val a = drop(datastructure.List(1,2,3,4,5), 10)
    println(a)

    // practice 3.5
    val b = dropWhile(datastructure.List(1,2,3,4,5,6), _ % 2 == 0)
    println(b)

    // practice 3.6
    val c = init(datastructure.List(1,2,3,4,5))
    println(c)

    // practice 3.7
    println("product2 test:")
    println(product2(datastructure.List(1.0, 2.0, 3.0, 0.0, 9.0, 10.0)))

    // practice 3.8
    println(foldRight(datastructure.List(1,2,3), datastructure.Nil: datastructure.List[Int])(datastructure.Cons(_, _)))

    // practice 3.9
    println(length(datastructure.List(1,2,3,4,5)))

    // practice 3.12
    println(reverseList(datastructure.List(1,2,3,4,5)))

    // practice 3.14
    println(appendViaFoldRight(datastructure.List(1,2,3), datastructure.List(4,5)))
//    println(appendViaFoldLeft(datastructure.List(1,2,3), datastructure.List(3,4)))

    //practice 3.20
    println(flatMap(datastructure.List(1,2,3))(i => datastructure.List(i, i)))
    println(flatMapViaConcat(datastructure.List(1,2,3))(i => datastructure.List(i, i)))

    // practice 3.21
    println(filterViaFlatMap(datastructure.List(1,2,3,4,5))(_ % 2 == 0))

    // practice 3.22
    println(sumWith(datastructure.List(1,2,3), datastructure.List(1,2,3)))

    // practice 3.25
    println(size(Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))))

    // practice 3.26
    println(max(Branch(Leaf(10), Branch(Leaf(12), Leaf(13)))))

    // practice 3.27
    println(depth(Branch(Leaf(10), Branch(Branch(Leaf(1), Leaf(3)), Leaf(13)))))

    // practice 3.28
    println(map(Branch(Leaf(10), Branch(Branch(Leaf(1), Leaf(3)), Leaf(13))))(_ * 10))

    // practice 3.29
    println(sizeViaFold(Branch(Leaf(10), Branch(Branch(Leaf(1), Leaf(3)), Leaf(13)))))
  }
}
