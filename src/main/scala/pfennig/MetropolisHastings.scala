package pfennig

object MetropolisHastings {
  def mh[A](d: Distribution[A]): Distribution[List[A]] = {
    val proposal: Distribution[(A, Probability)] = Prior.weightedPrior(d)

    def iterate(weighted: (A, Probability)): Distribution[List[(A, Probability)]] = {
      val (x, s) = weighted
      for {
        p <- proposal
        (y, r) = p
        accept <- Distribution.bernoulli(1.0 min (r/s))
        next = if(accept) (y -> r) else (x -> s)
        rest <- iterate(next)
      } yield next :: rest
    }

    proposal flatMap (iterate _) map (_ map { case (x, w) => x })
  }
}
