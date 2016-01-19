package pfennig

/** A generator of randomly chosen values */
sealed trait Random[A] {
  def sample(randomness: Randomness): A 
}
final case class Bernoulli(weight: Probability) extends Random[Boolean] {
  def sample(randomness: Randomness): Boolean =
    weight < randomness.nextDouble()
}

object Random {
  implicit object randomInstances extends Sampleable[Random] {
    def sample[A](distribution: Random[A])(randomness: Randomness): A =
      distribution.sample(randomness)
  }

  def bernoulli(weight: Probability): Random[Boolean] =
    Bernoulli(weight)
}
