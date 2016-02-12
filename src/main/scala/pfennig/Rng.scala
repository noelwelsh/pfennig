package pfennig

import scala.annotation.tailrec

/** An implementation of random number generators that implements Sampleable */
sealed trait Rng[A] {
  import Rng._

  def sample(randomness: Randomness): A
}
object Rng {
  final case class Discrete[A](events: List[(A, Probability)]) extends Rng[A] {
    def sample(randomness: Randomness): A = {
      val mass = (events map { case(a,p) => p }).sum
      val choice = randomness.nextDouble() * mass
      @tailrec
      def pick(total: Double, events: List[(A,Double)]): A =
        events match {
          case (a,p) :: rest =>
            if(choice < (total + p))
              a
            else
              pick(total + p, rest)
          case Nil =>
            throw new Exception("Ran out of probability mass while sampling. Hmm..")
        }
      pick(0.0, events)
    }
  }
  final case class Uniform(left: Double, right: Double) extends Rng[Double] {
    def sample(randomness: Randomness): Double = {
      val choice = randomness.nextDouble()
      val width = right - left
      (choice * width) + left
    }
  }
  final case class Normal(mean: Double, stdDev: Double) extends Rng[Double] {
    def sample(randomness: Randomness): Double = {
      val choice = randomness.nextGaussian()
      (stdDev * choice) + mean
    }
  }

  /** Create a distribution over the given elements with the given probability */
  def discrete[A](events: List[(A, Probability)]): Rng[A] =
    Discrete(events)

  /** Construct a distribution that is true with probability weight and false
    * otherwise. The weight should be between 0 and 1. */
  def bernoulli[A](weight: Probability): Rng[Boolean] =
    discrete(List(true -> weight, false -> (1 - weight)))

  /** Construct a uniform distribution over the given atoms. */
  def uniform[A](atoms: List[A]): Rng[A] = {
    val n = atoms.length
    val weight = (1.0 / n)
    discrete(atoms.map(a => a -> weight))
  }

  /** Construct a uniform distribution over the interval (left, right) */
  def uniform(left: Double, right: Double): Rng[Double] =
    Uniform(left, right)

  /** Construct a normal distribution with given mean and standard deviation. */
  def normal(mean: Double, stdDev: Double): Rng[Double] =
    Normal(mean, stdDev)

  implicit object rngInstances extends Sampleable[Rng] {
    def sample[A](r: Rng[A])(randomness: Randomness): A =
      r.sample(randomness)
  }
}
