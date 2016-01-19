package pfennig

import cats.Monad
import scala.language.higherKinds

sealed trait Distribution[A] {
  def condition(likelihood: Likelihood[A]): Distribution[A] =
    Conditional(this, likelihood)

  def flatMap[B](f: A => Distribution[B]): Distribution[B] =
    FlatMap(this, f)

  def map[B](f: A => B): Distribution[B] =
    FlatMap(this, (a: A) => Pure(f(a)))
}
final case class Pure[A](get: A) extends Distribution[A]
final case class FlatMap[A,B](d: Distribution[A], f: A => Distribution[B]) extends Distribution[B]
final case class Primitive[F[_],A](fa: F[A], sampleable: Sampleable[F]) extends Distribution[A]
final case class Conditional[A](distribution: Distribution[A], likelihood: Likelihood[A]) extends Distribution[A]
object Distribution {
  implicit object distributionInstances extends Monad[Distribution] with Sampleable[Distribution] {
    def flatMap[A,B](fa: Distribution[A])(f: A => Distribution[B]): Distribution[B] =
      fa.flatMap(f)

    def pure[A](a: A): Distribution[A] =
      Pure(a)

    def sample[A](d: Distribution[A])(randomness: Randomness): A = {
      import Sampleable.ops._

      d match {
        case Pure(a) => a
        case FlatMap(d, f) =>
          sample(f(sample(d)(randomness)))(randomness)
        case p: Primitive[f,a] =>
          val fa: f[a] = p.fa
          implicit val sampleable: Sampleable[f] = p.sampleable
          fa.sample(randomness)
        case Conditional(da, l) =>
          throw new Exception("Cannot sample from distributions that have been conditioned.")
      }
    }
  }

  def bernoulli(weight: Probability): Distribution[Boolean] =
    Primitive(Random.bernoulli(weight), Random.randomInstances)
}
