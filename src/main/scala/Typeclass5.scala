import simulacrum.typeclass

sealed trait Maybe[+A]
case class Just[A](a: A) extends Maybe[A]
case object Empty extends Maybe[Nothing]

object Maybe {
  def apply[A](a: A): Maybe[A] =
    if (a == null) Empty else Just(a)
}

object Typeclass5 {

  @typeclass trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
    def as[A, B](fa: F[A], b: B): F[B] = map(fa)(_ => b)
    def tuple[A, B](fa: F[A], b: B): F[(A, B)] = map(fa)(a => (a, b))
    def lift[A, B](f: A => B): F[A] => F[B] = map(_)(f)
  }

  object Instances {
    implicit val maybeFunctor = new Functor[Maybe] {
      override def map[A, B](fa: Maybe[A])(f: A => B): Maybe[B] =
        fa match {
          case Empty => Empty
          case Just(x) => Just(f(x))
        }
    }
  }

  import Instances._
  import Functor.ops._

  def main(args: Array[String]): Unit = {
    assert(Maybe(1).map(_ + 1) == Just(2))
    assert(Maybe[String](null).map(str => str.length) == Empty)

    val fn: Maybe[Int] => Maybe[Int] = Functor[Maybe].lift((i:Int) => i + 1)
    val list: List[Maybe[Int]] = List(Just(1), Empty, Just(3))

    assert(list.map(fn) == List(Just(2), Empty, Just(4)))
  }
}
