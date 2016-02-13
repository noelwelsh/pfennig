package pfennig

import cats.Comonad
import scala.annotation.tailrec

/** A generator of randomly chosen values */
sealed trait Random[A] {
  def sample(randomness: Randomness): A 
}
object Random {
  /** Create a distribution that is certainly a single value. I.e. a diract delta. */
  def pure[A](a: A): Random[A] =
    Pure(a)

  /** Create a distribution over the given elements with the given probability */
  def discrete[A](events: List[(A, Probability)]): Random[A] =
    Discrete(events)

  /** Construct a distribution that is true with probability weight and false
    * otherwise. The weight should be between 0 and 1. */
  def bernoulli[A](weight: Probability): Random[Boolean] =
    Bernoulli(weight)

  /** Construct a uniform distribution over the given atoms. */
  def uniform[A](atoms: List[A]): Random[A] = {
    val n = atoms.length
    val weight = (1.0 / n)
    discrete(atoms.map(a => a -> weight))
  }

  /** Construct a uniform distribution over the interval (left, right) */
  def uniform(left: Double, right: Double): Random[Double] =
    Uniform(left, right)

  /** Construct a normal distribution with given mean and standard deviation. */
  def normal(mean: Double, stdDev: Double): Random[Double] =
    Normal(mean, stdDev)

  final case class Pure[A](get: A) extends Random[A] {
    def sample(randomness: Randomness): A =
      get
  }

  final case class Discrete[A](events: List[(A, Probability)]) extends Random[A] {
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
  final case class Uniform(left: Double, right: Double) extends Random[Double] {
    def sample(randomness: Randomness): Double = {
      val choice = randomness.nextDouble()
      val width = right - left
      (choice * width) + left
    }
  }
  final case class Normal(mean: Double, stdDev: Double) extends Random[Double] {
    def sample(randomness: Randomness): Double = {
      val choice = randomness.nextGaussian()
      (stdDev * choice) + mean
    }
  }
  final case class Bernoulli(weight: Probability) extends Random[Boolean] {
    def sample(randomness: Randomness): Boolean =
      weight < randomness.nextDouble()
  }

  implicit object sampleableInstance extends Sampleable[Random] {
    override def sample[A](distribution: Random[A])(randomness: Randomness): A =
      distribution.sample(randomness)
  }

  implicit def comonadInstance(implicit randomness: Randomness): Comonad[Random] =
    new Comonad[Random] {
      override def coflatMap[A, B](fa: Random[A])(f: (Random[A]) => B): Random[B] =
        Random.pure(f(fa))
      override def extract[A](x: Random[A]): A =
        x.sample(randomness)
      override def map[A, B](fa: Random[A])(f: (A) ⇒ B): Random[B] =
        Random.pure(f(fa.sample(randomness)))
    }
}
