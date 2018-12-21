object Typeclass2 {

  trait Eq[A] {
    def eq(a: A, b: A): Boolean
  }

  class Foo(val a: Int, val b: String)

  object Syntax {
    implicit class EqOps[A: Eq](a: A)(implicit F: Eq[A]) {
      def === (b: A): Boolean = F.eq(a, b)
    }
  }

  object Instances {
    implicit val eqInt = new Eq[Int] {
      override def eq(a: Int, b: Int): Boolean = a == b
    }

    implicit val eqFoo = new Eq[Foo] {
      def eq(a: Foo,b: Foo): Boolean = a.a == b.a && a.b == b.b
    }
  }

  import Instances._
  import Syntax._

  def main(args: Array[String]): Unit = {
    assert(allEqual(List(1, 1, 1)))
    assert(!allEqual(List(1, 2, 3)))
    assert(allEqual(List(new Foo(1, "bar"), new Foo(1, "bar"))))
  }

  def allEqual[A: Eq](xs: List[A])(implicit F: Eq[A]): Boolean = xs match {
    case Nil => true
    case head :: tail => tail.forall(a => F.eq(a, head))
  }

  def allEqualSyntax[A: Eq](xs: List[A]): Boolean =
    xs match {
      case Nil => true
      case head :: tail => tail.forall(a => a === head)
    }
}