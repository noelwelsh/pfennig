package pfennig

import cats.Monad
import scala.annotation.tailrec

/** Simple implementation of a probability monad that enumerates all events. Intended to demonstrate semantics. */
final case class Enumeration[A](events: List[(A, Probability)]) {
  def condition(likelihood: Likelihood[A]): Enumeration[A] =
    Enumeration(events map { case(a, p) => a -> (p * likelihood(a)) })

  def sample(randomness: Randomness): A = {
    val weight = randomness.nextDouble()
    @tailrec
    def pick(total: Probability, events: List[(A, Probability)]): A =
      events match {
        case (a, p) :: rest =>
          if(total < weight && weight < (total + p))
            a
          else
            pick(total + p, rest)
        case Nil =>
          throw new Exception("Could not sample---ran out of events!")
      }
    pick(0.0, this.compact.normalize.events)
  }

  def normalize: Enumeration[A] = {
    val totalWeight = (events map { case (a, p) => p }).sum
    Enumeration(events map { case (a,p) => a -> (p / totalWeight) })
  }

  def compact: Enumeration[A] = {
    val distinct = (events map { case (a, p) => a }).distinct
    def prob(a: A): Probability =
      (events filter { case (x, p) => x == a } map { case (a, p) => p }).sum

    Enumeration(distinct map { a => a -> prob(a) })
  }

  def flatMap[B](f: A => Enumeration[B]): Enumeration[B] =
    Enumeration(
      events flatMap { case (a, p) =>
        f(a).events map { case (b, q) => b -> (p * q) }
      }
    )

  def map[B](f: A => B): Enumeration[B] =
    Enumeration(events map { case (a, p) => f(a) -> p })
}
object Enumeration {
  def bernoulli(weight: Probability): Enumeration[Boolean] =
    Enumeration(List( true -> weight, false -> (1 - weight) ))

  def uniform[A](atoms: List[A]): Enumeration[A] =
    Enumeration(atoms map { a => a -> 1.0 }).normalize
}
