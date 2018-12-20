import simulacrum.{op, typeclass}

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Typeclass4 {
  @typeclass trait Monoid[A] {
    def empty: A
    @op("|+|") def combine(a: A, b: A): A
  }

  object Instances {
    import Monoid.ops._
    implicit val intMonoid = new Monoid[Int] {
      override def empty: Int = 0
      override def combine(a: Int, b: Int): Int = a + b
    }

    implicit val stringMonoid = new Monoid[String] {
      override def empty: String = ""
      override def combine(a: String, b: String): String = a + b
    }

    implicit def mapMonoid[A, B](implicit M:Monoid[B]): Monoid[Map[A, B]] = new Monoid[Map[A, B]] {
      override def empty: Map[A, B] = Map.empty[A, B]
      override def combine(a: Map[A, B], b: Map[A, B]): Map[A, B] =
        a.map { case (k, v) => (k, v |+| b.getOrElse(k, M.empty)) }
    }
  }

  import Instances._
  def main(args: Array[String]): Unit = {
    val range = (1 to 1000000)
    assert(parCombineAll(range) == range.sum)
  }
  import Monoid.ops._
  def combineAll[A](monoids:Seq[A])(implicit M:Monoid[A]): A =
    monoids.fold(M.empty)(_ |+| _)

  def parCombineAll[A : Monoid](monoids:Seq[A]): A = {
    val futures: Iterator[Future[A]] =
      monoids.grouped(Runtime.getRuntime.availableProcessors())
        .map(group => Future(combineAll(group)))
    val futureResult: Future[A] = Future.sequence(futures).map(agg => combineAll(agg.toSeq))
    Await.result(futureResult, Duration.Inf)
  }
}