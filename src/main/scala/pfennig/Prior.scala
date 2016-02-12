package pfennig

object Prior {
  def prior[A](d: Distribution[A]): Distribution[A] =
    d match {
      case Conditional(d, l) =>
        prior(d)
      case FlatMap(da, f) =>
        FlatMap(prior(da), f)
      case other => other
    }

  def weightedPrior[A](d: Distribution[A]): Distribution[(A, Probability)] =
    d match {
      case Conditional(d, l) =>
        for {
          o <- weightedPrior(d)
          (x, weight) = o
        } yield (x -> weight * l(x))
      case FlatMap(d, f) =>
        for {
          o <- weightedPrior(d)
          (x, weight) = o
          y <- f(x)
        } yield (y -> weight)
      case other =>
        for {
          x <- other
        } yield (x -> 1)
    }
}
