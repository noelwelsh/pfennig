package object pfennig {
  type Probability = Double
  type Likelihood[A] = A => Probability
  type Randomness = java.util.Random
}
