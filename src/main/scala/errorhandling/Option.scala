package errorhandling

// Practice 4.1
sealed trait Option[+A] {
  // map
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // flatMap
  def flatMap[B](f: A => Option[B]): Option[B] = map(f).getOrElse(None)

  // getOrElse
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // orElse
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
    if (this == None) {
      ob
    } else {
      this
    }
  }
  def orElse_1[B >: A](ob: => Option[B]): Option[B] =
    this.map(Some(_)).getOrElse(ob)
  def orElse_2[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }
  def orElse_3[B >: A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case Some(a) => Some(a)
  }

  // filter
  def filter(f: A => Boolean): Option[A] = flatMap(a => if (f(a)) Some(a) else None)
  def filter_2(f: A => Boolean): Option[A] = this match {
    case Some(a) if (f(a)) => Some(a)
    case _ => None
  }
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

def mean(xs: Seq[Double]): Option[Double] =
  if (xs.isEmpty) None
  else Some(xs.sum / xs.length)

// Practice 4.2
def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

// Practice 4.3

def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = (a, b) match {
  case (Some(a), Some(b)) => Some(f(a, b))
  case _ => None
}

def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(aa => b.flatMap(bb => Some(f(aa, bb))))
def map2_2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
  a.flatMap(aa => b.map(bb => f(aa, bb)))

// Practice 4.4
// scala List
//def sequence[A](a: List[Option[A]]): Option[List[A]] = ???
def sequence[A](a: List[Option[A]]): Option[List[A]] =
  a match {
    case Nil => Some(Nil)
    case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
  }

// Practice 4.5
def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
  a match {
    case Nil => Some(Nil)
    case h::t => map2(f(h), traverse(t)(f))(_ :: _)
  }
